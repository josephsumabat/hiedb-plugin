{-# LANGUAGE GADTs #-}

module Plugin.HieDb (plugin) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IORef
import GHC
import GHC.Driver.Hooks
import GHC.Driver.Pipeline
import GHC.Driver.Pipeline.Phases
import GHC.Plugins as Plugins
import GHC.Types.Name.Cache
import HieDb.Create
import HieDb.Types
import System.Directory (doesPathExist, makeAbsolute)
import System.FilePath
import qualified System.IO.Unsafe as Unsafe

plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = Plugins.purePlugin
    , driverPlugin = driver
    }

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver _ hscEnv = do
  initializeHiedb
  pure
    hscEnv
      { hsc_hooks =
          (hsc_hooks hscEnv)
            { runPhaseHook = Just phaseHook
            }
      }
 where
  initializeHiedb = liftIO $ withHieDb defaultHiedbFile initConn

  -- We index using a phase hook instead of typeCheckResultAction since
  -- the hie file can be written after that plugin phase
  phaseHook =
    PhaseHook $ \phase -> do
      case phase of
        T_HscPostTc _ modSummary _ _ _ -> do
          let dynFlags = hsc_dflags hscEnv
              hieDirectory = hieDir dynFlags
          _ <- liftIO $ addModuleToDb defaultHiedbFile (ms_mod modSummary) hieDirectory
          runPhase phase
        _ -> runPhase phase

addModuleToDb :: FilePath -> Module -> Maybe FilePath -> IO ()
addModuleToDb hiedbFile mod' mHieBaseDir = do
  let
    -- Note: For performance reasons we intentionally skip the type
    -- indexing phase
    -- TODO: pass this in as a user defined option
    skipOptions = defaultSkipOptions{skipTypes = True}
    modToPath = moduleNameSlashes . moduleName

  let mHieFile = do
        hieBaseDir <- mHieBaseDir
        pure (hieBaseDir </> modToPath mod' -<.> ".hie")

  case mHieFile of
    Nothing -> pure ()
    Just hieFile -> do
      absoluteHieFile <- makeAbsolute hieFile
      hieExists <- doesPathExist absoluteHieFile
      when hieExists $ do
        _ <- withDbLock $ do
          nc <- newIORef =<< initNameCache 'a' []
          _ <-
            withHieDb
              hiedbFile
              (\conn -> runDbM nc $ addRefsFrom conn (Just ".") skipOptions absoluteHieFile)
              -- TODO: report this and maybe make configurable in future versions
              `catch` (\(_ :: SomeException) -> pure False)
          pure ()
        pure ()
 where
  acquireDbLock =
    liftIO $ atomically $ takeTMVar dbLock
  releaseDbLock =
    liftIO $ atomically $ putTMVar dbLock ()
  -- Safely use a db lock - ensure the lock is released if an exception occurs
  withDbLock :: IO () -> IO ()
  withDbLock fn = do
    acquireDbLock
    fn `catch` (\(_ :: SomeException) -> pure ())
    releaseDbLock

defaultHiedbFile :: String
defaultHiedbFile = ".hiedb"

-----------------------------------------------------
-- Since we cant pass state along through the phases we use unsafePerformIO
-- to define global mutable state
-----------------------------------------------------

-- | We need to ensure only one thread writes to the db at once since sqlite
-- only maintains one WAL file and will throw an error on concurrent writes
dbLock :: TMVar ()
dbLock = Unsafe.unsafePerformIO $ newTMVarIO ()
{-# NOINLINE dbLock #-}
