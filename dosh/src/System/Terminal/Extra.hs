{-# OPTIONS_GHC -Wno-orphans #-}

module System.Terminal.Extra where

import Control.Monad.Except
import Control.Monad.Reader
import System.Terminal
import Prelude hiding (putChar)

instance (MonadInput m) => MonadInput (ExceptT e m) where
    awaitWith = lift . awaitWith
    setBracketedPasteMode = lift . setBracketedPasteMode

instance (MonadPrinter m) => MonadPrinter (ExceptT e m) where
    putLn = lift putLn
    putChar = lift . putChar
    putString = lift . putString
    putStringLn = lift . putStringLn
    putText = lift . putText
    putTextLn = lift . putTextLn
    flush = lift flush
    getLineWidth = lift getLineWidth

instance (MonadScreen m) => MonadScreen (ExceptT e m) where
    getWindowSize = lift getWindowSize
    moveCursorUp = lift . moveCursorUp
    moveCursorDown = lift . moveCursorDown
    moveCursorForward = lift . moveCursorForward
    moveCursorBackward = lift . moveCursorBackward
    getCursorPosition = lift getCursorPosition
    setCursorPosition = lift . setCursorPosition
    setCursorRow = lift . setCursorRow
    setCursorColumn = lift . setCursorColumn
    saveCursor = lift saveCursor
    restoreCursor = lift restoreCursor
    insertChars = lift . insertChars
    deleteChars = lift . deleteChars
    eraseChars = lift . eraseChars
    insertLines = lift . insertLines
    deleteLines = lift . deleteLines
    eraseInLine = lift . eraseInLine
    eraseInDisplay = lift . eraseInDisplay
    showCursor = lift showCursor
    hideCursor = lift hideCursor
    setAutoWrap = lift . setAutoWrap
    setAlternateScreenBuffer = lift . setAlternateScreenBuffer

instance (MonadColorPrinter m) => MonadColorPrinter (ExceptT e m) where
    data Color (ExceptT e m) = ColorT (Color m)
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

deriving stock instance (Eq (Attribute m)) => Eq (Attribute (ExceptT e m))

instance (MonadMarkupPrinter m) => MonadMarkupPrinter (ExceptT e m) where
    data Attribute (ExceptT e m) = AttributeT (Attribute m)
    setAttribute (AttributeT a) = lift (setAttribute a)
    resetAttribute (AttributeT a) = lift (resetAttribute a)
    resetAttributes = lift resetAttributes
    resetsAttribute (AttributeT a) (AttributeT b) = resetsAttribute a b

instance (MonadFormattingPrinter m) => MonadFormattingPrinter (ExceptT e m) where
    bold = AttributeT bold
    italic = AttributeT italic
    underlined = AttributeT underlined
    inverted = AttributeT inverted

instance (MonadTerminal m) => MonadTerminal (ExceptT e m)

instance (MonadInput m) => MonadInput (ReaderT r m) where
    awaitWith = lift . awaitWith
    setBracketedPasteMode = lift . setBracketedPasteMode
