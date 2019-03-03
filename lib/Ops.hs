{-# LANGUAGE DefaultSignatures, GADTs #-}
module Ops (cd, glob, draw) where

import Data.String
import Diagrams
import Diagrams.Backend.Braille
import qualified System.FilePath.Glob as Glob
import System.Directory

class CDArg a where
  asArg :: a -> [String]
  default asArg :: Show a => a -> [String]
  asArg a = [show a]
  asArgFromList :: [a] -> [String]
  default asArgFromList :: Show a => [a] -> [String]
  asArgFromList = concatMap asArg

instance CDArg Bool
instance CDArg Int
instance CDArg Integer
instance CDArg Word

instance CDArg Char where
  asArg s = [[s]]
  asArgFromList s = [s]

instance CDArg a => CDArg [a] where
  asArg = asArgFromList
  asArgFromList = concatMap asArg

class CDResult r where
  changeDirectory :: [String] -> r

instance (a ~ ()) => CDResult (IO a) where
  changeDirectory [] = setCurrentDirectory =<< getHomeDirectory
  changeDirectory [dir] = setCurrentDirectory dir
  changeDirectory _ = putStrLn "Too many arguments"

instance (CDArg a, CDResult r) => CDResult (a -> r) where
  changeDirectory args dir = changeDirectory $ args <> asArg dir

cd :: CDResult r => r
cd = changeDirectory mempty

draw :: Diagram Braille -> IO ()
draw = putStr . renderDia Braille (BrailleOptions (mkWidth 80))

glob pattern = traverse makeRelativeToCurrentDirectory =<< Glob.glob pattern
