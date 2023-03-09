module Main where

import Data.Sequence.Zipper (SeqZipper (after, before))
import Data.Text qualified as Text
import Dosh.GHC.Client qualified as GHC
import Dosh.GHC.Server qualified as GHC
import Dosh.LSP.Client qualified as LSP
import Dosh.LSP.Document
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
    initialNotebook <- newNotebook
    liftIO $ do
        lspClient.request
            LSP.OpenDocument
                { uri = initialNotebook.document.uri
                , language = "haskell"
                , text = ""
                }
    liftIO $ forkIO $ forever $ do
        threadDelay 1_000_000
        lspClient.request LSP.GetDocumentContents{uri = initialNotebook.document.uri}
        -- lspClient.request LSP.WaitForDiagnostics
        lspClient.request LSP.GetDiagnostics{uri = initialNotebook.document.uri}
    initManager_ $ mdo
        dn :: Dynamic t Notebook <- holdDyn initialNotebook u
        u :: Event t Notebook <- networkView (notebook ghcClient lspClient <$> dn) >>= switchHold never
        grout flex $ col $ do
            grout (fixed $ pure 1) $ text $ pure "before:"
            grout flex $ text $ current $ Text.unlines . fmap tshow . toList . before . chunks . document <$> dn
            grout (fixed $ pure 1) $ text $ pure "after:"
            grout flex $ text $ current $ Text.unlines . fmap tshow . toList . after . chunks . document <$> dn
            grout (fixed $ pure 1) $ text $ pure "content:"
            grout flex $ text $ current $ withLineNumbers . contents . document <$> dn
            grout (fixed $ pure 1) $ text $ pure "diagnostics:"
            grout flex $ text $ current $ tshow . diagnostics . document <$> dn
            grout (fixed $ pure 1) $ text $ pure "error:"
            grout flex $ text $ current $ (.error) . document <$> dn
        void <$> ctrldPressed
  where
    withLineNumbers :: Text -> Text
    withLineNumbers = Text.intercalate "\n" . zipWith (\ln t -> tshow ln <> " " <> t) [0 :: Int ..] . Text.splitOn "\n"