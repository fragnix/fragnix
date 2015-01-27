{-# LINE 1 "Network/HTTP/Date/Parser.hs" #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Date.Parser (parseHTTPDate) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.ByteString
import Data.Char
import Network.HTTP.Date.Types

----------------------------------------------------------------

-- |
-- Parsing HTTP Date. Currently only RFC1123 style is supported.
--
-- >>> parseHTTPDate "Tue, 15 Nov 1994 08:12:31 GMT"
-- Just (HTTPDate {hdYear = 1994, hdMonth = 11, hdDay = 15, hdHour = 8, hdMinute = 12, hdSecond = 31, hdWkday = 2})

parseHTTPDate :: ByteString -> Maybe HTTPDate
parseHTTPDate bs = case parseOnly rfc1123Date bs of
    Right ut -> Just ut
    _        -> Nothing

rfc1123Date :: Parser HTTPDate
rfc1123Date = do
    w <- wkday
    void $ string ", "
    (y,m,d) <- date1
    sp
    (h,n,s) <- time
    sp
    void $ string "GMT"
    return $ defaultHTTPDate {
        hdYear   = y
      , hdMonth  = m
      , hdDay    = d
      , hdHour   = h
      , hdMinute = n
      , hdSecond = s
      , hdWkday  = w
      }

wkday :: Parser Int
wkday = 1 <$ string "Mon"
    <|> 2 <$ string "Tue"
    <|> 3 <$ string "Wed"
    <|> 4 <$ string "Thu"
    <|> 5 <$ string "Fri"
    <|> 6 <$ string "Sat"
    <|> 7 <$ string "Sun"

date1 :: Parser (Int,Int,Int)
date1 = do
    d <- day
    sp
    m <- month
    sp
    y <- year
    return (y,m,d)
 where
   day = digit2
   year = digit4

sp :: Parser ()
sp = () <$ char ' '

time :: Parser (Int,Int,Int)
time = do
    h <- digit2
    void $ char ':'
    m <- digit2
    void $ char ':'
    s <- digit2
    return (h,m,s)

month :: Parser Int
month =  1 <$ string "Jan"
    <|>  2 <$ string "Feb"
    <|>  3 <$ string "Mar"
    <|>  4 <$ string "Apr"
    <|>  5 <$ string "May"
    <|>  6 <$ string "Jun"
    <|>  7 <$ string "Jul"
    <|>  8 <$ string "Aug"
    <|>  9 <$ string "Sep"
    <|> 10 <$ string "Oct"
    <|> 11 <$ string "Nov"
    <|> 12 <$ string "Dec"

digit2 :: Parser Int
digit2 = do
    x1 <- toInt <$> digit
    x2 <- toInt <$> digit
    return $ x1 * 10 + x2

digit4 :: Parser Int
digit4 = do
    x1 <- toInt <$> digit
    x2 <- toInt <$> digit
    x3 <- toInt <$> digit
    x4 <- toInt <$> digit
    return $ x1 * 1000 + x2 * 100 + x3 * 10 + x4

toInt :: Char -> Int
toInt c = ord c - ord '0'
