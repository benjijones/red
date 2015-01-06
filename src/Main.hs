import GHC
import GHC.Paths (libdir)
import GHC.IO.Handle.FD (stdout)
import GhcMonad
import DynFlags
import CoreSyn
import Outputable (ppr, printForC)

main :: IO ()
main = runGhc (Just libdir) core
    
core :: (GhcMonad m) => m ()      
core = do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    coreModule <- compileToCoreModule "test/test_main.hs"
    liftIO . printForC dflags stdout . ppr $ cm_binds coreModule
    
    
--prettyPrint :: CoreModule -> String
--prettyPrint core = foldr f (cm_binds core)