{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Lens hiding (noneOf, oneOf)
import Data.ByteString.Lazy hiding (count, filter, head, intercalate, null)
import Data.Foldable (for_)
import Data.List (intercalate, sortOn)
import Geo.Computations
import Network.Wreq
import System.IO
import Text.Parser.Char (anyChar, char, digit, noneOf, notChar, oneOf, string)
import Text.Parser.Combinators (count, endBy, many, optional, sepBy, sepEndBy)
import Text.Trifecta.Parser (parseByteString)
import Text.Trifecta.Result

main :: IO ()
main = writeModule "lib/ICAO.hs"

data METARException = NotAvailable Status
                    | ParseFailed ErrInfo
                    deriving (Show)

instance Exception METARException

data Info = Info { code :: String, name :: String, point :: Point }
          deriving (Show)

bbsss :: IO [Info]
bbsss = do
  r <- get "https://tgftp.nws.noaa.gov/data/nsd_bbsss.txt"
  pure $ case r ^. responseStatus . statusCode of
    200 -> case parseByteString parser mempty (toStrict $ r ^. responseBody) of
      Success x -> filter (("----" /=) . code) x
      Failure e -> throw . ParseFailed $ e
    _ -> throw . NotAvailable $ r ^. responseStatus

parser = line `endBy` (string "\r\n" <|> string "\n") where
  line = do
    many (notChar ';')
    char ';'
    many (notChar ';')
    char ';'
    code <- count 4 anyChar 
    char ';'
    name <- many (notChar ';') <* char ';'
    many (notChar ';')
    char ';'
    many (notChar ';')
    char ';'
    many (notChar ';')
    char ';'
    latH <- many digit
    char '-'
    latM <- many digit
    latS <- optional $ char '-' >> many digit
    latD <- optional $ oneOf "NS"
    char ';'
    lonH <- many digit
    char '-'
    lonM <- many digit
    lonS <- optional $ char '-' >> many digit
    lonD <- optional $ oneOf "EWN" -- buggy entries
    char ';'
    many (notChar ';')
    char ';'
    many (notChar ';')
    char ';'
    ele <- toEle <$> optional (char '-') <*> many digit <* char ';'
    many (notChar ';')
    char ';'
    many (noneOf ";\r\n")
    let lat = dir 'S' latD $ read latH + (read latM / 60)
    let lon = dir 'W' lonD $ read lonH + (read lonM / 60)
    pure $ Info code name (pt lat lon ele Nothing)

dir d x = case x of
  Just d' | d == d' -> negate
  _ -> id

toEle _ "" = Nothing
toEle (Just '-') v = Just $ negate (read v)
toEle Nothing v = Just $ read v

writeModule fp = do
  l <- sortOn code <$> bbsss
  withFile fp WriteMode $ \h -> do
    hPutStrLn h "{-# LANGUAGE RecordWildCards #-}"
    hPutStrLn h "module ICAO (ICAOCode(..), point, nearest) where"
    hPutStrLn h ""
    hPutStrLn h "import Data.List (sortOn)"
    hPutStrLn h "import Data.Map.Strict as Map"
    hPutStrLn h "import Data.Maybe (fromJust)"
    hPutStrLn h "import Geo.Computations"
    hPutStrLn h ""
    hPutStrLn h "point :: ICAOCode -> Point"
    hPutStrLn h "point c = icaoPoint . fromJust $ Map.lookup c icaoData"
    hPutStrLn h ""
    hPutStrLn h "nearest :: Point -> (ICAOCode, Vector)"
    hPutStrLn h "nearest p = head . sortOn (fst . snd) $"
    hPutStrLn h "            fmap vec <$> Map.assocs icaoData"
    hPutStrLn h " where vec ICAOData{..} = getVector p icaoPoint"
    hPutStrLn h ""
    hPutStrLn h $ "data ICAOCode = " <>
                  intercalate "\n              | " ((\Info{..} -> code <> " -- " <> name) <$> l)
    hPutStrLn h "          deriving (Bounded, Enum, Eq, Ord, Read, Show)"
    hPutStrLn h ""
    hPutStrLn h "data ICAOData = ICAOData { icaoPoint :: Point }"
    hPutStrLn h ""
    hPutStrLn h "icaoData = Map.fromDistinctAscList ["
    hPutStrLn h $ "    " <>
                  intercalate "\n  , " (fmap (\Info{..} ->
                    "(" <> code <> ", ICAOData (" <> show point <> "))") l)
    hPutStrLn h "  ]"
