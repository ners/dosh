{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

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
    initialNotebook <- newNotebook
    initManager_ $ mdo
        dn :: Dynamic t Notebook <- holdDyn initialNotebook u
        u :: Event t Notebook <- networkView (notebook ghcClient lspClient <$> dn) >>= switchHold never
        void <$> ctrldPressed
