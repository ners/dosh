module Dosh.GHC.Session where

import Control.Monad.IO.Class (liftIO)
import Data.Functor
import Development.IDE.GHC.Compat qualified as GHC
import GHC (GhcMonad)
import GHC qualified
import GHC.Driver.Session qualified as GHC
import GHC.Platform.Ways qualified as GHC

initializeSession :: GhcMonad m => m ()
initializeSession = do
    originalFlagsNoPackageEnv <- GHC.getSessionDynFlags
    logger <- GHC.getLogger
    originalFlags <- liftIO $ GHC.interpretPackageEnv logger originalFlagsNoPackageEnv
    let flag = flip GHC.xopt_set
        unflag = flip GHC.xopt_unset
        dflags = flag GHC.ExtendedDefaultRules . unflag GHC.MonomorphismRestriction $ setWayDynFlag originalFlags
    void $
        GHC.setSessionDynFlags $
            dflags
                { GHC.backend = GHC.Interpreter
                }
    prelude <- GHC.parseImportDecl "import Prelude"
    GHC.setContext [GHC.IIDecl $ prelude{GHC.ideclImplicit = True}]

addWay'
    :: GHC.Way
    -> GHC.DynFlags
    -> GHC.DynFlags
addWay' w dflags0 =
    let platform = GHC.targetPlatform dflags0
        dflags1 = dflags0{GHC.targetWays_ = GHC.addWay w (GHC.targetWays_ dflags0)}
        dflags2 = foldr GHC.setGeneralFlag' dflags1 (GHC.wayGeneralFlags platform w)
        dflags3 = foldr GHC.unSetGeneralFlag' dflags2 (GHC.wayUnsetGeneralFlags platform w)
     in dflags3

{- | Consult the RTS to find if GHC has been built with dynamic linking and then turn on the
 dynamic way for GHC. Otherwise it does nothing.
-}
setWayDynFlag
    :: GHC.DynFlags
    -> GHC.DynFlags
setWayDynFlag =
    if GHC.hostIsDynamic
        then addWay' GHC.WayDyn
        else id
