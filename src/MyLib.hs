module MyLib (someFunc) where

import Control.Monad
import Dosh.Notebook
import Dosh.Util
import Reflex
import Reflex.Network (networkView)
import Reflex.Vty

someFunc :: IO ()
someFunc = mainWidget $ do
    (i, o) <- echoServer
    initManager_ $ mdo
        dn <- holdDyn newNotebook u
        u <- networkView (notebook i o <$> dn) >>= switchHold never
        grout flex $ text $ tshow <$> current dn
        void <$> ctrldPressed
