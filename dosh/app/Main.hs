module Main where

import Data.Text.IO qualified as Text
import Dosh.GHC.Client qualified as GHC
import Dosh.GHC.Server qualified as GHC
import Dosh.LSP.Client qualified as LSP
import Dosh.LSP.Document (newDocument)
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
    initialNotebook <- newNotebook $ #language .~ "haskell"
    let identifier = initialNotebook.identifier
    let document = newDocument identifier & #getSessionDynFlags .~ GHC.getSessionDynFlags ghcServer
    liftIO $ do
        lspClient.request LSP.Initialize
        lspClient.request $ LSP.CreateDocument document
    performEvent $ lspClient.onLog <&> \l -> liftIO $ Text.appendFile "hls-log.log" $ tshow l <> "\n"
    performEvent $ lspClient.onError <&> \e -> liftIO $ Text.appendFile "hls-error.log" $ tshow e <> "\n"
    initManager_ $ mdo
        dn :: Dynamic t Notebook <- holdDyn initialNotebook u
        u :: Event t Notebook <- networkView (notebook ghcClient lspClient <$> dn) >>= switchHold never
        void <$> ctrldPressed
