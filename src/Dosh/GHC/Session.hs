{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Session where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Data.List (stripPrefix)
import Development.IDE.GHC.Compat qualified as GHC
import Dosh.Util
import GHC (GhcMonad)
import GHC qualified
import GHC.Driver.Session qualified as GHC
import GHC.Generics (Generic)
import GHC.Platform.Ways qualified as GHC
import Language.Haskell.TH.LanguageExtensions

deriving instance Generic GHC.DynFlags

setExtension :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags
setExtension = flip GHC.xopt_set

unsetExtension :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags
unsetExtension = flip GHC.xopt_unset

setGeneral :: GHC.GeneralFlag -> GHC.DynFlags -> GHC.DynFlags
setGeneral = flip GHC.gopt_set

unsetGeneral :: GHC.GeneralFlag -> GHC.DynFlags -> GHC.DynFlags
unsetGeneral = flip GHC.gopt_unset

setWay :: GHC.Way -> GHC.DynFlags -> GHC.DynFlags
setWay w dflags0 =
    let platform = GHC.targetPlatform dflags0
        dflags1 = dflags0{GHC.targetWays_ = GHC.addWay w (GHC.targetWays_ dflags0)}
        dflags2 = foldr GHC.setGeneralFlag' dflags1 (GHC.wayGeneralFlags platform w)
        dflags3 = foldr GHC.unSetGeneralFlag' dflags2 (GHC.wayUnsetGeneralFlags platform w)
     in dflags3

overDynFlags :: GhcMonad m => (GHC.DynFlags -> GHC.DynFlags) -> m ()
overDynFlags f =
    GHC.getSessionDynFlags
        >>= ((GHC.getLogger >>=) . (liftIO .) . flip GHC.interpretPackageEnv)
        >>= GHC.setSessionDynFlags . f

initialiseSession :: GhcMonad m => m ()
initialiseSession = do
    overDynFlags $
        #backend .~ GHC.Interpreter
            >>> setGeneral GHC.Opt_GhciSandbox
            >>> filtered (const GHC.hostIsDynamic) %~ setWay GHC.WayDyn
    addImport "Dosh.Prelude"

deriving instance Generic (GHC.ImportDecl GHC.GhcPs)

addImport :: GhcMonad m => String -> m ()
addImport lib = do
    parsed <- GHC.parseImportDecl $ "import " <> lib
    context <- GHC.getContext
    GHC.setContext $ GHC.IIDecl parsed : context

deriving stock instance Read Extension

enableExtension :: GhcMonad m => Extension -> m ()
enableExtension ext = overDynFlags $ setExtension ext

disableExtension :: GhcMonad m => Extension -> m ()
disableExtension ext = overDynFlags $ unsetExtension ext

applyExtensionString :: GhcMonad m => String -> m ()
applyExtensionString (stripPrefix "No" >=> toMaybe =<< startsWithUpper -> Just ext) = overDynFlags $ unsetExtension $ read ext
applyExtensionString ext = overDynFlags $ setExtension $ read ext
