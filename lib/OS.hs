{-# LANGUAGE TemplateHaskell #-}
module OS where

import Shh

$(loadEnv SearchPath)
