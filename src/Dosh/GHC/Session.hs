{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Dosh.GHC.Session where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Generics.Labels ()
import Development.IDE.GHC.Compat qualified as GHC
import GHC (GhcMonad)
import GHC qualified
import GHC.Driver.Session qualified as GHC
import GHC.Generics (Generic)
import GHC.Platform.Ways qualified as GHC
#if __GLASGOW_HASKELL__ >= 941
import GHC.Data.Bool qualified as GHC
#else
import GHC.Utils.Misc qualified as GHC
#endif

deriving instance Generic GHC.DynFlags

enableExtension :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags
enableExtension = flip GHC.xopt_set

disableExtension :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags
disableExtension = flip GHC.xopt_unset

enableGeneral :: GHC.GeneralFlag -> GHC.DynFlags -> GHC.DynFlags
enableGeneral = flip GHC.gopt_set

disableGeneral :: GHC.GeneralFlag -> GHC.DynFlags -> GHC.DynFlags
disableGeneral = flip GHC.gopt_unset

initialiseSession :: GhcMonad m => m ()
initialiseSession = do
    logger <- GHC.getLogger
    GHC.getSessionDynFlags
        >>= liftIO . GHC.interpretPackageEnv logger
        >>= \flags ->
            GHC.setSessionDynFlags $
                flags
                    & #backend .~ GHC.Interpreter
                    & #useColor .~ GHC.Always
                    & disableGeneral GHC.Opt_GhciSandbox
                    & filtered (const GHC.hostIsDynamic) %~ addWay' GHC.WayDyn
    prelude <- GHC.parseImportDecl "import Dosh.Prelude"
    GHC.setContext
        [ GHC.IIDecl $ prelude{GHC.ideclImplicit = True}
        ]

addWay' :: GHC.Way -> GHC.DynFlags -> GHC.DynFlags
addWay' w dflags0 =
    let platform = GHC.targetPlatform dflags0
        dflags1 = dflags0{GHC.targetWays_ = GHC.addWay w (GHC.targetWays_ dflags0)}
        dflags2 = foldr GHC.setGeneralFlag' dflags1 (GHC.wayGeneralFlags platform w)
        dflags3 = foldr GHC.unSetGeneralFlag' dflags2 (GHC.wayUnsetGeneralFlags platform w)
     in dflags3
