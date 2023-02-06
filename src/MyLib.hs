module MyLib (someFunc) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as Text
import Dosh.Notebook
import Reflex
import Reflex.ExternalRef
import Reflex.Vty
import Util

echoServer
    :: ExternalRef t Text
    -> ExternalRef t Text
    -> IO ()
echoServer i o = forever $ do
    incomingText <- readExternalRef i
    forM_ (Text.inits incomingText) $ \prefix -> do
        threadDelay 100_000
        writeExternalRef o prefix

someFunc :: IO ()
someFunc = mainWidget $ do
    i <- newExternalRef ""
    o <- newExternalRef ""
    _ <- liftIO $ forkIO $ echoServer i o
    initManager_ $ mdo
        dn <- holdDyn newNotebook u
        u <- dyn (notebook i o <$> dn) >>= switchHold never
        grout flex $ text $ tshow <$> current dn
        void <$> ctrldPressed
