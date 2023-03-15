{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.LSP.Server where

import Data.ByteString (hGetSome, hPut)
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.Default (def)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Development.IDE (Logger (Logger), Pretty (pretty), Recorder (..), WithPriority, cmapWithPrio, logWith)
import Development.IDE.Main (Arguments (..), defaultArguments, defaultMain)
import Development.IDE.Session (findCradle, loadCradle)
import Dosh.Prelude
import Dosh.Util hiding (withTimeout)
import GHC.Paths qualified as GHC
import GHC.Settings.Config qualified as GHC
import HIE.Bios.Config (CradleConfig (..), CradleType (..))
import HIE.Bios.Cradle (getCradle)
import HIE.Bios.Types (ComponentOptions (..), Cradle (..), CradleAction (..), CradleLoadResult (..))
import HIE.Bios.Types qualified as Cradle
import HlsPlugins (idePlugins)
import Language.LSP.Client qualified as LSP
import Language.LSP.Client.Session (Session)
import Reflex
    ( Reflex (Event)
    , TriggerEvent (newTriggerEvent)
    )
import System.Process.Extra

data Server t = Server
    { input :: Session () -> IO ()
    , error :: Event t SomeException
    , log :: Event t (WithPriority Text)
    }

server
    :: forall t m
     . ( MonadIO m
       , TriggerEvent t m
       )
    => m (Server t)
server = do
    (log, logTrigger) <- newTriggerEvent
    (error, reportError) <- newTriggerEvent
    (serverInput, serverOutput) <- liftIO $ do
        -- TODO: try to use Knob rather than pipes
        (inRead, inWrite) <- createPipe
        hSetBuffering inRead NoBuffering
        hSetBuffering inWrite NoBuffering
        (inRead', inWrite') <- createPipe
        hSetBuffering inRead' NoBuffering
        hSetBuffering inWrite' NoBuffering
        (outRead, outWrite) <- createPipe
        hSetBuffering outRead NoBuffering
        hSetBuffering outWrite NoBuffering
        (outRead', outWrite') <- createPipe
        hSetBuffering outRead' NoBuffering
        hSetBuffering outWrite' NoBuffering
        let recorder = Recorder{logger_ = liftIO . logTrigger}
        forkIO $ forever $ do
            c <- hGetSome inRead defaultChunkSize
            Text.appendFile "hls-input.log" $ Text.decodeUtf8 c
            hPut inWrite' c
        forkIO $ forever $ do
            c <- hGetSome outRead' defaultChunkSize
            Text.appendFile "hls-output.log" $ Text.decodeUtf8 c
            hPut outWrite c
        forkIO $ ghcide recorder inRead' outWrite'
        pure (inWrite, outRead)
    sessionActions <- newChan
    liftIO $
        forkIO $
            LSP.runSessionWithHandles serverOutput serverInput $
                forever $ do
                    action <- readChan sessionActions
                    action `catch` (liftIO . reportError)
    pure
        Server
            { input = writeChan sessionActions
            , error
            , log
            }

-- TODO: can we get rid of handles altogether?
ghcide :: Recorder (WithPriority Text) -> Handle -> Handle -> IO ()
ghcide recorder stdin stdout = do
    let logger = Logger $ logWith recorder
        recorder' = cmapWithPrio (tshow . pretty) recorder
        plugins = idePlugins $ cmapWithPrio (tshow . pretty) recorder
        arguments =
            (defaultArguments recorder' logger plugins)
                { argsHandleIn = pure stdin
                , argsHandleOut = pure stdout
                , argsSessionLoadingOptions =
                    def
                        { findCradle = const $ pure Nothing
                        , loadCradle = \_ _ ->
                            pure $
                                getCradle
                                    undefined
                                    ( CradleConfig
                                        { cradleDependencies = []
                                        , cradleType = Direct{arguments = []}
                                        }
                                    , "."
                                    )
                        }
                }
    defaultMain recorder' arguments

directCradle :: FilePath -> [String] -> Cradle a
directCradle wdir args =
    Cradle
        { cradleRootDir = wdir
        , cradleOptsProg =
            CradleAction
                { actionName = Cradle.Direct
                , runCradle = \_ _ ->
                    return (CradleSuccess (ComponentOptions (args ++ argDynamic) wdir []))
                , runGhcCmd =
                    pure . \case
                        ["--print-libdir"] -> CradleSuccess GHC.libdir
                        ["--numeric-version"] -> CradleSuccess GHC.cProjectVersion
                        _ -> CradleNone
                }
        }
  where
    argDynamic :: [String]
    argDynamic = ["-dynamic"]
