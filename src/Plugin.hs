module Plugin (plugin) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IORef
import GHC
import GHC.Plugins as Plugins
import GHC.Types.Name.Cache
import HieDb.Create
import HieDb.Types
import System.Directory (doesFileExist)
import System.FilePath
import qualified System.IO.Unsafe as Unsafe

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = install
        , pluginRecompile = Plugins.purePlugin
        , driverPlugin = driver
        }

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver _ hscEnv = do
    let dynFlags = hsc_dflags hscEnv
        hieDirectory = hieDir dynFlags

    liftIO $ atomically $ writeTVar hieDirDynflag hieDirectory
    initializeHiedb
    pure hscEnv
  where
    initializeHiedb = liftIO $ withHieDb defaultHiedbFile initConn

addModuleToDb :: FilePath -> Module -> IO ()
addModuleToDb hiedbFile mod' = do
    let
        -- Note: For performance reasons we intentionally skip the type
        -- indexing phase
        -- TODO: pass this in as a user defined option
        skipOptions = defaultSkipOptions{skipTypes = True}
        modToPath = moduleNameSlashes . moduleName

    mHieBaseDir <- liftIO $ readTVarIO hieDirDynflag
    let mHieFile = ((</>) <$> mHieBaseDir) <*> pure (modToPath mod' -<.> ".hie")

    case mHieFile of
        Nothing -> pure ()
        Just hieFile -> do
            hieExists <- doesFileExist hieFile
            when hieExists $ do
                _ <- acquireDbLock
                nc <- newIORef =<< initNameCache 'a' []
                _ <-
                    withHieDb
                        hiedbFile
                        (\conn -> runDbM nc $ addRefsFrom conn (Just ".") skipOptions hieFile)
                        -- If indexing fails we dont want to
                        -- TODO: report this and maybe make configurable in future versions
                        `catch` (\(_ :: SomeException) -> pure False)
                _ <- releaseDbLock
                pure ()
  where
    acquireDbLock =
        liftIO $ atomically $ takeTMVar dbLock
    releaseDbLock =
        liftIO $ atomically $ putTMVar dbLock ()

-- We index on a pass to core since the .hiefile may not be available yet
-- after the typechecking phase is complete
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    let pluginPass modGuts =
            do
                _ <- liftIO $ addModuleToDb defaultHiedbFile (mg_module modGuts)
                pure modGuts
        newTodo = todo ++ [CoreDoPluginPass "queue for indexing" pluginPass]

    return newTodo

defaultHiedbFile :: String
defaultHiedbFile = ".hiedb"

-----------------------------------------------------
-- Since we cant pass state along through the phases we use unsafePerformIO
-- to define global mutable state
-----------------------------------------------------

-- | We need to keep track of the hie file from dynflags
hieDirDynflag :: TVar (Maybe String)
hieDirDynflag = Unsafe.unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE hieDirDynflag #-}

{- | We need to ensure only one thread writes to the db at once since sqlite
only maintains one WAL file and will throw an error on concurrent writes
-}
dbLock :: TMVar ()
dbLock = Unsafe.unsafePerformIO $ newTMVarIO ()
{-# NOINLINE dbLock #-}
