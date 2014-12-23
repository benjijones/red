{-# LANGUAGE CPP #-}
import GHC
import GHC.Paths ( libdir )
import GhcMonad
import DynFlags

main :: IO ()
main = runGhc (Just libdir) core
    
core :: (GhcMonad m) => m ()      
core = do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
--    target <- guessTarget "test/test_main.hs" Nothing
--    setTargets [target]
--    load LoadAllTargets
    core <- compileToCoreModule "test/test_main.hs"
    liftIO . print $ cm_safe core
    
--prettyPrint :: CoreModule -> String
--prettyPrint core = foldr f (cm_binds core)