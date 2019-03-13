{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Exception (IOException, catch)
import Data.Char (isUpper)
import Data.List (sortOn)
import System.IO (withFile, IOMode(..))

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString hiding (split)
import qualified Data.ByteString.Char8 as ByteString (split)


-- cassava
import Data.Csv (FromField(parseField), FromNamedRecord(parseNamedRecord), (.:))
import qualified Data.Csv as Cassava

-- gps

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

data Item = Item
  { item_ident :: Text
  , item_type :: ItemType
  , item_name :: Text
  , item_elevation :: Maybe Double
  , item_continent :: Text
  , item_iso_country :: Text
  , item_iso_region :: Text
  , item_municipality :: Text
  , item_gps_code :: Text
  , item_iata_code :: Text
  , item_local_code :: Text
  , item_coordinates :: ItemCoordinates
  } deriving (Eq, Show)

data ItemType = Balloonport
              | Closed
              | Heliport
              | LargeAirport
              | MediumAirport
              | SeaplaneBase
              | SmallAirport
              deriving (Eq, Read, Show)

data ItemCoordinates = ItemCoordinates
  { coord_lat :: Double
  , coord_lon :: Double
  } deriving (Eq, Show)

instance FromNamedRecord Item where
  parseNamedRecord m =
    Item <$> m .: "ident"
         <*> m .: "type"
         <*> m .: "name"
         <*> (fmap feetToMeters <$> m .: "elevation_ft")
         <*> m .: "continent"
         <*> m .: "iso_country"
         <*> m .: "iso_region"
         <*> m .: "municipality"
         <*> m .: "gps_code"
         <*> m .: "iata_code"
         <*> m .: "local_code"
         <*> m .: "coordinates"

instance FromField ItemType where
  parseField "balloonport" = pure Balloonport
  parseField "closed" = pure Closed
  parseField "heliport" = pure Heliport
  parseField "large_airport" = pure LargeAirport
  parseField "medium_airport" = pure MediumAirport
  parseField "seaplane_base" = pure SeaplaneBase
  parseField "small_airport" = pure SmallAirport
  parseField other = fail . ("Unknown item type: " <>) =<< parseField other

instance FromField ItemCoordinates where
  parseField coord = case ByteString.split ',' coord of
    [lat, lon] -> ItemCoordinates <$> parseField lat <*> parseField lon
    _ -> fail . ("Unable to parse coordinates: " <>) =<< parseField coord

decodeItems :: ByteString -> Either String (Vector Item)
decodeItems = fmap snd . Cassava.decodeByName

decodeItemsFromFile :: FilePath -> IO (Either String (Vector Item))
decodeItemsFromFile filePath =
  catchShowIO (ByteString.readFile filePath) >>= pure . either Left decodeItems

main :: IO ()
main = do
  items <- decodeItemsFromFile "airport-codes/data/airport-codes.csv"
  case items of
    Left e -> putStrLn e
    Right v -> do
      let icao = sortOn item_ident . Vector.toList . Vector.filter (isICAO . item_ident) $ v
      putStr $ "Writing lib/ICAO.hs (" <> show (length icao) <> " entries)..."
      writeModule "lib/ICAO.hs" icao
      putStrLn "done"

isICAO :: Text -> Bool
isICAO t = Text.length t == 4 && all isUpper (Text.unpack t)

feetToMeters :: Fractional a => a -> a
feetToMeters ft = ft * 3.2808

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `catch` handleIOException
  where handleIOException :: IOException -> IO (Either String a)
        handleIOException = pure . Left . show

writeModule :: FilePath -> [Item] -> IO ()
writeModule fp items = do
  withFile fp WriteMode $ \h -> do
    Text.hPutStrLn h $ Text.unlines [
        "{-# LANGUAGE RecordWildCards #-}"
      , "module ICAO (ICAOCode(..), point, nearest) where"
      , ""
      , "import Data.List (sortOn)"
      , "import Data.Map.Strict as Map"
      , "import Data.Maybe (fromJust)"
      , "import qualified Geo.Computations as GPS"
      , ""
      , "point :: ICAOCode -> GPS.Point"
      , "point c = icaoPoint . fromJust $ Map.lookup c icaoData"
      , ""
      , "nearest :: GPS.Point -> (ICAOCode, GPS.Vector)"
      , "nearest p = head . sortOn (fst . snd) $"
      , "            fmap vec <$> Map.assocs icaoData"
      , " where vec ICAOData{..} = GPS.getVector p icaoPoint"
      , ""
      ]
    Text.hPutStrLn h $ "data ICAOCode = " <>
                  Text.intercalate "\n              | " ((\Item{..} ->
                    item_ident <> " -- " <> item_name <> ", " <> item_iso_country <> ", " <> item_continent) <$> items)
    Text.hPutStrLn h "          deriving (Bounded, Enum, Eq, Ord, Read, Show)"
    Text.hPutStrLn h ""
    Text.hPutStrLn h "data ICAOData = ICAOData { icaoPoint :: GPS.Point }"
    Text.hPutStrLn h ""
    Text.hPutStrLn h "icaoData = Map.fromDistinctAscList ["
    Text.hPutStrLn h $ "    " <>
                  Text.intercalate "\n  , " (fmap (\Item{..} ->
                    "(" <> item_ident <>
                    ", ICAOData (GPS.pt " <>
                    "(" <> Text.pack (show (coord_lat item_coordinates)) <> ") " <>
                    "(" <> Text.pack (show (coord_lon item_coordinates)) <> ") " <>
                    "(" <> Text.pack (show item_elevation) <> ") " <>
                    "Nothing" <> ")" <>
                    ")") items)
    Text.hPutStrLn h "  ]"
