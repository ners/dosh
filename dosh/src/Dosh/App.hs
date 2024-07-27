{-# LANGUAGE UndecidableInstances #-}

module Dosh.App where

import Control.Monad.Schedule.Class
import Data.List.NonEmpty
import Language.LSP.Client.Session (MonadSession, SessionT)
import System.Terminal
import System.Terminal.Extra ()
import System.Terminal.Internal (LocalTerminal, Terminal)
import Prelude

newtype AppT t m a = App {unApp :: TerminalT t (SessionT m) a}
    deriving newtype
        ( Monad
        , Applicative
        , Functor
        , MonadIO
        , MonadThrow
        , MonadPrinter
        , MonadInput
        , MonadScreen
        , MonadTerminal
        , MonadSession
        )

type AppExceptT e t m = ExceptT e (AppT t m)

type App = AppT LocalTerminal IO

type AppExcept = AppExceptT Interrupt LocalTerminal IO

instance (MonadSchedule (TerminalT t (SessionT m)), Monad m) => MonadSchedule (AppT t m) where
    schedule :: NonEmpty (AppT t m a) -> AppT t m (NonEmpty a, [AppT t m a])
    schedule as = App do
        (x, y) <- schedule (as <&> (.unApp))
        pure (x, App <$> y)

instance
    ( MonadIO m
    , MonadThrow m
    , MonadColorPrinter (TerminalT t (SessionT m))
    , Terminal t
    )
    => MonadColorPrinter (AppT t m)
    where
    data Color (AppT t m)
        = ColorT (Color (TerminalT t (SessionT m)))
    black = ColorT black
    red = ColorT red
    green = ColorT green
    yellow = ColorT yellow
    blue = ColorT blue
    magenta = ColorT magenta
    cyan = ColorT cyan
    white = ColorT white
    bright (ColorT c) = ColorT $ bright c
    foreground (ColorT c) = AttributeT (foreground c)
    background (ColorT c) = AttributeT (background c)

instance
    ( MonadIO m
    , MonadThrow m
    , MonadMarkupPrinter (TerminalT t (SessionT m))
    , Terminal t
    )
    => MonadMarkupPrinter (AppT t m)
    where
    data Attribute (AppT t m)
        = AttributeT
            (Attribute (TerminalT t (SessionT m)))
        deriving stock (Eq, Show)
    setAttribute (AttributeT a) = App (setAttribute a)
    resetAttribute (AttributeT a) = App (resetAttribute a)
    resetAttributes = App resetAttributes
    resetsAttribute (AttributeT a) (AttributeT b) = resetsAttribute a b

instance
    ( MonadIO m
    , MonadThrow m
    , MonadFormattingPrinter (TerminalT t (SessionT m))
    , Terminal t
    )
    => MonadFormattingPrinter (AppT t m)
    where
    bold = AttributeT bold
    italic = AttributeT italic
    underlined = AttributeT underlined
    inverted = AttributeT inverted
