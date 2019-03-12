{-# LANGUAGE RecordWildCards #-}
module METAR where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Lens hiding (noneOf, oneOf)
import Data.ByteString.Lazy hiding (count, filter, head, null)
import Data.List (sortOn)
import Geo.Computations
import Network.Wreq
import Text.Parser.Char (anyChar, char, digit, noneOf, notChar, oneOf, string)
import Text.Parser.Combinators (count, endBy, many, optional, sepBy, sepEndBy)
import Text.Trifecta.Parser (parseByteString)
import Text.Trifecta.Result

nearest :: Point -> IO (String, Distance)
nearest p = do
  l <- bbsss
  pure . head . sortOn snd $ (\Info {..} -> (code, distance point p)) <$> l

data METARException = NotAvailable Status
                    | ParseFailed ErrInfo
                    deriving (Show)

instance Exception METARException

data Info = Info { code :: String, point :: Point }
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
    many (notChar ';')
    char ';'
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
    pure $ Info code (pt lat lon ele Nothing)

dir d x = case x of
  Just d' | d == d' -> negate
  _ -> id

toEle _ "" = Nothing
toEle (Just '-') v = Just $ negate (read v)
toEle Nothing v = Just $ read v
