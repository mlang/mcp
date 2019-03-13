module METAR where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import qualified ICAO
import qualified Network.Wreq as Web

get :: ICAO.ICAOCode -> IO String
get code = do
  r <- Web.get $ "https://tgftp.nws.noaa.gov/data/observations/metar/stations/"
              <> show code <> ".TXT"
  pure . unwords . drop 1 . words . head . drop 1 . lines . unpack $ r ^. Web.responseBody
