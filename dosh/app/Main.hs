module Main where

import Data.Text.IO qualified as Text
import Dosh.GHC.Client qualified as GHC
import Dosh.GHC.Server qualified as GHC
import Dosh.LSP.Client qualified as LSP
import Dosh.LSP.Server qualified as LSP
import Dosh.Notebook
import Dosh.Prelude
import Dosh.Util
import Reflex.Network (networkView)
import Reflex.Vty

main :: IO ()
main = mainWidget $ do
    ghcServer <- GHC.server
    ghcClient <- GHC.client ghcServer
    lspServer <- LSP.server
    lspClient <- LSP.client lspServer
    initialNotebook <- newNotebook "Haskell" "hs"
    liftIO $ do
        lspClient.request LSP.Initialize
        lspClient.request $ LSP.CreateDocument $ initialNotebook ^. #document
        forkIO $ forever $ do
            threadDelay 100_000
            lspClient.request LSP.GetDocumentContents{uri = initialNotebook ^. #document . #uri}
    performEvent $ lspClient.onLog <&> \l -> liftIO $ Text.appendFile "hls-log.log" $ tshow l <> "\n"
    performEvent $ lspClient.onError <&> \e -> liftIO $ Text.appendFile "hls-error.log" $ tshow e <> "\n"
    initManager_ $ mdo
        dn :: Dynamic t Notebook <- holdDyn initialNotebook u
        u :: Event t Notebook <- networkView (notebook ghcClient lspClient <$> dn) >>= switchHold never
        grout flex $ col $ do
            grout (fixed $ pure 1) $ text $ pure "Contents:"
            grout flex $ text $ current $ dn <&> withLineNumbers . view (#document . #contents)
        void <$> ctrldPressed
