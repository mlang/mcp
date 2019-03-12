{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, TupleSections #-}
module Main (main, repl) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, throwIO)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.State.Class
import Data.Foldable
import Data.Function
import Data.List (isPrefixOf, nub, sort)
import Language.Haskell.Interpreter hiding (get)
import REPL
import Shh (Failure(..))
import System.IO.Error
import System.Posix.IO
import System.Console.Haskeline hiding (outputStr, outputStrLn, getInputLine)
import qualified System.Console.Haskeline as Haskeline

main :: IO ()
main = repl

type REPL = StateT REPLState (REPLT IO)

repl = either print pure =<< interp `evalStateT` defaultREPLState
                                    `runREPLT` settings where
  settings = defaultSettings { autoAddHistory = True
                             , historyFile = Just ".mcp_history" }
           & setComplete completion
  interp = do
    setup
    loop
    outputStrLn "Goodbye Mr. Anderson."

setup :: REPL ()
setup = do
  (rFd, wFd) <- liftIO createPipe
  eprint <- REPL.getExternalPrint
  liftIO . forkIO . forever $ do
    (s, bc) <- fdRead rFd 1024
    eprint s
  set [languageExtensions := [ExtendedDefaultRules, OverloadedStrings]]
  setImportsF [ ModuleImport "Prelude" NotQualified NoImportList
              , ModuleImport "Control.Concurrent" NotQualified NoImportList
              , ModuleImport "Control.Monad" NotQualified NoImportList
              , ModuleImport "Data.Foldable" NotQualified NoImportList
              , ModuleImport "Data.IORef" NotQualified NoImportList
              , ModuleImport "Data.List" NotQualified NoImportList
              , ModuleImport "Diagrams" NotQualified NoImportList
              , ModuleImport "Geo.Computations" NotQualified NoImportList
              , ModuleImport "ICAO" (QualifiedAs Nothing) NoImportList
              , ModuleImport "METAR" (QualifiedAs Nothing) NoImportList
              , ModuleImport "Network.URI.Encode" (QualifiedAs (Just "URI")) NoImportList
              , ModuleImport "Radio" NotQualified NoImportList
              , ModuleImport "Ops" NotQualified NoImportList
              , ModuleImport "OS" (QualifiedAs Nothing) NoImportList
              , ModuleImport "Shh" NotQualified NoImportList
              , ModuleImport "System.Directory" NotQualified NoImportList
              , ModuleImport "System.Posix.IO" NotQualified NoImportList
              , ModuleImport "System.Posix.Types" NotQualified NoImportList
              , ModuleImport "WhereAmI" NotQualified NoImportList
              ]
  runStmt $ "let externalPrint s = fdWrite (read " <> show (show wFd) <> ") s >> pure ()"
  runStmt "let printProc p = readProc p >>= externalPrint"
  runStmt "radio <- stations externalPrint"
  runStmt "printBanner"
  loadRC ".mcprc"

loadRC :: FilePath -> REPL ()
loadRC fp =
  traverse_ runStmt =<< lines <$> liftIO (readFile fp `catch` doesNotExist)
 where
  doesNotExist e | isDoesNotExistError e = pure ""
                 | otherwise             = throwIO e

loop :: REPL ()
loop = prompt >>= \case
  Nothing -> pure ()
  Just h | h == "" || h == "?" || h == "help" -> help *> loop
  Just (':':rest) -> let (cmd, arg) = break (==' ') rest
                     in command cmd $ if null arg then arg else tail arg
  Just haskell -> do
    handle printSomeException $ handle printInterpreterError $ runStmt haskell
    loop

command :: String -> String -> REPL ()
command cmd arg = case cmd of
  "s" -> case arg of
    'p':'=':prompt -> setPrompt prompt *> loop
    _ -> help *> loop
  "k" -> do
    handle printInterpreterError $ outputStrLn =<< typeOf arg
    loop
  "t" -> do
    handle printInterpreterError $ outputStrLn =<< typeOf arg
    loop
  "e" -> do
    handle printInterpreterError $ outputStrLn =<< eval arg
    loop
  _ -> help *> loop

help = outputStr . unlines $ [
    ":s p=expr -- prompt :: String"
  , ":k type   -- Kind of type"
  , ":t expr   -- Type of expression"
  , "expr      -- Run in IO"
  , "Ctrl-D    -- Exit"
  ]

data REPLState = REPLState {
  replPrompt :: String
} deriving (Eq, Show)

defaultREPLState = REPLState { replPrompt = show "% " }

prompt :: REPL (Maybe String)
prompt = gets replPrompt >>= (`interpret` infer) >>= getInputLine

setPrompt prompt = handle printInterpreterError $ do
  interpret prompt (as :: String)
  modify $ \s -> s { replPrompt = prompt }

completion :: MonadInterpreter m => CompletionFunc m
completion line@(revl, r) = case reverse revl of
  "" -> (revl,) . map simpleCompletion . nub <$> namesInScope
  ":" -> pure (revl, simpleCompletion <$> [ "k", "t", "s" ])
  ":s" -> pure (' ':revl, [(simpleCompletion "p=") {isFinished = False}])
  ":s p=" -> (revl,) . map simpleCompletion . sort . nub <$> namesInScope
  'c':'d':' ':'"':d -> completeFilename line
  _ -> completeWord Nothing " " completeExpression line

completeExpression w =
  map simpleCompletion . nub . filter (w `isPrefixOf`) <$> namesInScope

printInterpreterError (WontCompile es) =
  for_ es $ outputStrLn . unbox where unbox (GhcError e) = e
printInterpreterError e = outputStrLn . show $ e

printSomeException :: MonadREPL m => SomeException -> m ()
printSomeException = outputStrLn . show

instance (MonadInterpreter m, MonadIO m) => MonadInterpreter (StateT s m) where
  fromSession = lift . fromSession
  modifySessionRef r = lift . modifySessionRef r
  runGhc m = lift $ runGhc m
