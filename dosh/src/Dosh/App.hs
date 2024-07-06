{-# LANGUAGE UndecidableInstances #-}

module Dosh.App where

import Language.LSP.Client.Session (MonadSession, SessionT)
import System.Terminal
import System.Terminal.Extra ()
import System.Terminal.Internal (LocalTerminal)
import Prelude

newtype AppT m a = App {unApp :: ExceptT Interrupt (TerminalT LocalTerminal (SessionT m)) a}
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

type App = AppT IO

instance
    ( MonadIO m
    , MonadThrow m
    , MonadColorPrinter (TerminalT LocalTerminal (SessionT m))
    )
    => MonadColorPrinter (AppT m)
    where
    data Color (AppT m)
        = ColorT (Color (ExceptT Interrupt (TerminalT LocalTerminal (SessionT m))))
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
    , MonadMarkupPrinter (ExceptT Interrupt (TerminalT LocalTerminal (SessionT m)))
    )
    => MonadMarkupPrinter (AppT m)
    where
    data Attribute (AppT m)
        = AttributeT
            (Attribute (ExceptT Interrupt (TerminalT LocalTerminal (SessionT m))))
        deriving stock (Eq)
    setAttribute (AttributeT a) = App (setAttribute a)
    resetAttribute (AttributeT a) = App (resetAttribute a)
    resetAttributes = App resetAttributes
    resetsAttribute (AttributeT a) (AttributeT b) = resetsAttribute a b

instance
    ( MonadIO m
    , MonadThrow m
    , MonadFormattingPrinter
        (ExceptT Interrupt (TerminalT LocalTerminal (SessionT m)))
    )
    => MonadFormattingPrinter (AppT m)
    where
    bold = AttributeT bold
    italic = AttributeT italic
    underlined = AttributeT underlined
    inverted = AttributeT inverted
