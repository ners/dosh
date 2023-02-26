{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Session where

import Data.Text (stripPrefix, unpack)
import Development.IDE.GHC.Compat qualified as GHC
import Dosh.Prelude
import Dosh.Util
import GHC (GhcMonad)
import GHC qualified
import GHC.Driver.Session qualified as GHC
import GHC.Platform.Ways qualified as GHC
import Language.Haskell.TH.LanguageExtensions

deriving stock instance Generic GHC.DynFlags

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
            >>> unsetExtension GHC.MonomorphismRestriction
            >>> setExtension GHC.IncoherentInstances
    addImport "Dosh.Prelude"

deriving stock instance Generic (GHC.ImportDecl GHC.GhcPs)

addImport :: GhcMonad m => Text -> m ()
addImport lib = do
    parsed <- GHC.parseImportDecl $ "import " <> unpack lib
    context <- GHC.getContext
    GHC.setContext $ GHC.IIDecl parsed : context

deriving stock instance Read Extension

enableExtension :: GhcMonad m => Extension -> m ()
enableExtension ext = overDynFlags $ setExtension ext

disableExtension :: GhcMonad m => Extension -> m ()
disableExtension ext = overDynFlags $ unsetExtension ext

applyExtensionText :: GhcMonad m => Text -> m ()
applyExtensionText (stripPrefix "No" -> Just ext@(maybeStartsWith True isUpper -> True)) = overDynFlags $ unsetExtension $ tread ext
applyExtensionText ext = overDynFlags $ setExtension $ tread ext
