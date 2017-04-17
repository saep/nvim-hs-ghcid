{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Neovim.Ghcid.Plugin
Description :  Ghcid quickfix integration plugin
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.Ghcid.Plugin
    where

import           Data.Yaml
import           GHC.Generics
import           Neovim
import           Neovim.BuildTool
import           Neovim.Quickfix              as Q
import           Neovim.User.Choice           (yesOrNo)
import           Neovim.User.Input

import           Language.Haskell.Ghcid       as Ghcid

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString              as BS
import           Data.Either                  (rights)
import           Data.List                    (groupBy, sort)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (mapMaybe)
import           System.FilePath


-- | Simple data type containing a few information on how to start ghcid.
data ProjectSettings = ProjectSettings
        { rootDir :: FilePath
        -- ^ Project directory from which ghcid can be started successfully.
        --
        , cmd     :: String
        -- ^ Command to start a ghci session (usually @cabal repl@ or
        -- @stack ghci@).
        }
    deriving (Eq, Ord, Show, Generic)


instance ToJSON ProjectSettings


instance FromJSON ProjectSettings


data GhcidState r = GhcidState
    { startedSessions :: Map FilePath (Ghci, Neovim r (GhcidState r) ())
    -- ^ A map from the root directory (see 'rootDir') to a 'Ghci' session and a
    -- release function which unregisters some autocmds and stops the ghci
    -- session.

    , quickfixItems   :: [QuickfixListItem String]
    }


modifyStartedSessions :: (Map FilePath (Ghci, Neovim r (GhcidState r) ())
                          -> Map FilePath (Ghci, Neovim r (GhcidState r) ()))
                      -> Neovim r (GhcidState r) ()
modifyStartedSessions f = modify $ \s -> s { startedSessions = f (startedSessions s) }


-- | Start or update a ghcid session.
--
-- This will call 'determineProjectSettings' and ask you to confirm or overwrite
-- its proposed settings. If you prepend a bang, it acts as if you have
-- confirmed all settings.
ghcidStart :: CommandArguments -> Neovim r (GhcidState r) ()
ghcidStart copts = do
    currentBufferPath <- errOnInvalidResult $ vim_call_function "expand" [ObjectBinary "%:p:h"]
    liftIO (determineProjectSettings' currentBufferPath) >>= \case
        Nothing -> void $
            yesOrNo "Could not determine project settings. This plugin needs a project with a .cabal file to work."
        Just s -> case bang copts of
            Just True ->
                startOrReload s

            _ -> do
                d <- askForDirectory
                        "Specify directory from which ghcid should be started."
                        (Just (rootDir s))
                c <- askForString
                        "Specify the command to execute (e.g. \"ghci\")."
                        (Just (cmd s))

                let s' = ProjectSettings d c
                whenM (yesOrNo "Save settings to file?") .
                    liftIO . BS.writeFile (d </> "ghcid.yaml") $ encode s'
                startOrReload s


-- | Start a new ghcid session or reload the modules to update the quickfix
-- list.
startOrReload :: ProjectSettings -> Neovim r (GhcidState r) ()
startOrReload s@(ProjectSettings d c) = Map.lookup d <$> gets startedSessions >>= \case
    Nothing -> do
        (g, ls) <- liftIO $ startGhci c (Just d) (\_ _ -> return ())
        applyQuickfixActions $ loadToQuickfix ls
        void $ vim_command "cwindow"
        ra <- addAutocmd "BufWritePost" def (startOrReload s) >>= \case
            Nothing ->
                return $ return ()

            Just (Left a) ->
                return a

            Just (Right rk) ->
                return $ Resource.release rk

        modifyStartedSessions $ Map.insert d (g,ra >> liftIO (stopGhci g))

    Just (ghci, _) -> do
        applyQuickfixActions =<< loadToQuickfix <$> liftIO (reload ghci)
        void $ vim_command "cwindow"


applyQuickfixActions :: [QuickfixListItem String] -> Neovim r (GhcidState r) ()
applyQuickfixActions qs = do
    fqs <- (nub' . rights . map bufOrFile) <$> gets quickfixItems
    modify $ \s -> s { quickfixItems = qs }
    forM_ fqs $ \f -> void . vim_command $ "sign unplace * file=" <> f
    setqflist qs Replace
    placeSigns qs
  where
    nub' = map head . groupBy (==) . sort


placeSigns :: [QuickfixListItem String] -> Neovim r st ()
placeSigns qs = forM_ (zip [(1::Integer)..] qs) $ \(i, q) -> case (lnumOrPattern q, bufOrFile q) of
    (Right _, _) ->
        -- Patterns not handled as they are not produced
        return ()

    (_, Left _) ->
        -- Buffer type not handled because i don't know how to pass that here
        -- and it is not produced.
        return ()

    (Left lnum, Right f) -> do
        let signType = case errorType q of
                Q.Error -> "GhcidErr"
                Q.Warning -> "GhcidWarn"

        -- TODO What if the file name contains spaces?
        void . vim_command $ unwords
            [ "sign place", show i, "line=" <> show lnum
            , "name=" <> signType, "file=" <> f
            ]

-- | Stop a ghcid session associated to the currently active buffer.
ghcidStop :: CommandArguments -> Neovim r (GhcidState r) ()
ghcidStop _ = do
    d <- errOnInvalidResult $ vim_call_function "expand" [ObjectBinary "%:p:h"]
    Map.lookupLE d <$> gets startedSessions >>= \case
        Nothing ->
            return ()
        Just (p,(_, releaseAction)) -> do
            modifyStartedSessions $ Map.delete p
            releaseAction


-- | Same as @:GhcidStop@ followed by @:GhcidStart!@. Note the bang!
ghcidRestart :: CommandArguments -> Neovim r (GhcidState r) ()
ghcidRestart _ = do
    ghcidStop def
    ghcidStart def { bang = Just True }


loadToQuickfix :: [Load] -> [QuickfixListItem String]
loadToQuickfix = dropWarningsIfErrorsArePresent . mapMaybe f
  where
    f m@Message{} =
        Just $ (quickfixListItem
                    ((Right . loadFile) m)
                    ((Left . fst . loadFilePos) m))
                    { col = Just $ ((snd . loadFilePos) m, True)
                    , Q.text = (unlines . loadMessage) m
                    , errorType = case loadSeverity m of
                        Ghcid.Warning -> Q.Warning
                        _             -> Q.Error
                    }
    f _ = Nothing

    dropWarningsIfErrorsArePresent xs =
        case filter ((== Q.Error) . errorType) xs of
            [] -> xs
            xs' -> xs'


maybePluginConfig :: MonadIO io => Directory -> io (Maybe BuildTool)
maybePluginConfig d = fmap (const Custom)
    <$> mkFile (Just d) "ghcid.yaml"

-- | Determine project settings for a directory.
--
-- This will traverse through all parent directories and search for a hint on
-- how to start the ghcid background process. The following configurations will
-- be tried in this order:
--
-- * A @ghcid.yaml@ file which can be created with the @GhcidStart@ command
-- * A @stack.yaml@ file
-- * A @cabal.sandbox.config@ file
-- * A @\*.cabal@ file
--
-- Note that 'ghcidStart' prompts for confirmation unless you prepend a bang.
-- So, if you want to use your preferred settings, simply save them to the
-- @ghcid.yaml@ file and you're done.
determineProjectSettings' :: FilePath -> IO (Maybe ProjectSettings)
determineProjectSettings' dir = runMaybeT $ do
    ds <- MaybeT $ fmap thisAndParentDirectories <$> mkDirectory dir
    buildTool <- MaybeT $ determineProjectSettings (maybePluginConfig : defaultProjectIdentifiers) ds
    case buildTool of
      (Stack, d) -> return $ ProjectSettings (getDirectory d) "stack ghci"
      (Cabal _, d) -> return $ ProjectSettings (getDirectory d) "cabal repl"
      (Custom, d) -> do
          f <- MaybeT $ mkFile (Just d) "ghcid.yaml"
          MaybeT $ decode <$> BS.readFile (getFile f)
      _ -> MaybeT $ return Nothing


