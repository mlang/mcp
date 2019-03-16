{-# LANGUAGE DefaultSignatures, GADTs #-}
module Ops (glob, draw, printBanner, tetris) where

import Command.Betris (betris)
import qualified Command.Betris as Betris (Options(..))
import Data.String
import Data.Time.Units (fromMicroseconds)
import Diagrams
import Diagrams.Backend.Braille
import qualified System.FilePath.Glob as Glob
import System.Directory

draw :: Diagram Braille -> IO ()
draw = putStr . renderDia Braille (BrailleOptions (mkWidth 80))

printBanner :: IO ()
printBanner = draw $
    triangle 1
 <> pentagon 1
 <> septagon 1
 <> nonagon 1
 <> translateY (-0.15) (translateX (-0.25) (baselineText "Marios"))
 <> translateY (-0.4)  (translateX (-0.25) (baselineText "Control"))
 <> translateY (-0.7)  (translateX (-0.25) (baselineText "Program"))

glob pattern = traverse makeRelativeToCurrentDirectory =<< Glob.glob pattern

tetris :: IO ()
tetris = betris (Betris.Options (fromMicroseconds 1000000))
