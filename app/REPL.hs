{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module REPL where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Data.Foldable
import Data.List (sort)
import Data.Typeable hiding (typeOf)
import qualified GHC
import Language.Haskell.Interpreter
import Outputable (showPpr)
import System.Console.Haskeline (InputT, runInputT)
import qualified System.Console.Haskeline as Haskeline

class MonadREPL m where
  getInputLine :: String -> m (Maybe String)
  getInputLineWithInitial :: String -> (String, String) -> m (Maybe String)
  getExternalPrint :: m (String -> IO ())
  outputStr, outputStrLn :: String -> m ()

newtype REPLT m a = REPLT (InputT (InterpreterT m) a)
                  deriving
                  ( Functor, Applicative, Monad
                  , MonadIO, MonadThrow, MonadCatch, MonadMask
                  )
instance (MonadIO m, MonadMask m) => MonadInterpreter (REPLT m) where
  fromSession = REPLT . lift . fromSession
  modifySessionRef r = REPLT . lift . modifySessionRef r
  runGhc x = REPLT . lift $ runGhc x
instance MonadTrans REPLT where
  lift = REPLT . lift . lift
instance (Monad m, MonadIO m, MonadMask m) => MonadREPL (REPLT m) where
  getInputLine = REPLT . Haskeline.getInputLine
  getInputLineWithInitial p = REPLT . Haskeline.getInputLineWithInitial p
  getExternalPrint = REPLT Haskeline.getExternalPrint
  outputStr = REPLT . Haskeline.outputStr
  outputStrLn = REPLT . Haskeline.outputStrLn
instance (Monad m, MonadREPL m) => MonadREPL (ReaderT e m) where
  getInputLine = lift . getInputLine
  getInputLineWithInitial p = lift . getInputLineWithInitial p
  getExternalPrint = lift getExternalPrint
  outputStr = lift . outputStr
  outputStrLn = lift . outputStrLn
instance (Monad m, MonadREPL m) => MonadREPL (StateT s m) where
  getInputLine = lift . getInputLine
  getInputLineWithInitial p = lift . getInputLineWithInitial p
  getExternalPrint = lift getExternalPrint
  outputStr = lift . outputStr
  outputStrLn = lift . outputStrLn

runREPLT :: (MonadIO m, MonadMask m) => REPLT m a -> Haskeline.Settings (InterpreterT m) -> m (Either InterpreterError a)
runREPLT (REPLT m) settings = runInterpreter . runInputT settings $ m

namesInScope :: MonadInterpreter m => m [String]
namesInScope = runGhc $
  fmap sort $ map . showPpr <$> GHC.getSessionDynFlags
                            <*> GHC.getRdrNamesInScope
