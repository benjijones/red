module Main where

import GHC (runGhc, compileToCoreModule, CoreModule, setSessionDynFlags, getSessionDynFlags, cm_binds)
import GHC.Paths (libdir)

import GhcMonad
import DynFlags
import CoreSyn

import Red.Translate (translate)

-- for printing
--import GHC.IO.Handle.FD (stdout)
--import Outputable (ppr, printForAsm)

main :: IO ()
main = do 
    core <- runGhc (Just libdir) (coreModule "test/test_main.hs")
    return $ translate $ cm_binds core
    return ()
--    liftIO . printForAsm dflags stdout . ppr $ cm_binds core
    
-- |Sets up a session in the GhcMonad and
-- compiles the given file to a CoreModule
coreModule :: (GhcMonad m) => String -> m CoreModule 
coreModule fileName = do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    compileToCoreModule fileName

