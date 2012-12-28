{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module TokyoForecast
( acquireTodayWeather
) where

import Text.XML.HXT.Core
import Control.Monad
import Text.HandsomeSoup
import Data.List
import Data.Char
    ( isAlphaNum
    , isSpace
    , toLower)

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
textAtTag tag = atTag tag >>> text

noNewLines :: [String] -> [String]
noNewLines = filter (/= "\n")

-- Time seems to be in format "xx-yy\160", so we remove the "\160"
-- part
cleanTime = take 5

-- Description has odd symbols in it. Clean them up
cleanDescription :: String -> String
cleanDescription = map toLower . filter (\x -> isSpace x || isAlphaNum x)

groupToTimes :: [String] -> [(String, String)]
groupToTimes [] = []
groupToTimes (time:persp:xs) = (cleanTime time, persp) : groupToTimes xs

groupToDates :: [String] -> [[(String, String)]]
groupToDates [] = []
groupToDates xs = [groupToTimes $ (take 8 xs)] ++ (groupToDates $ (drop 8 xs))

getWeather str = css str >>>
    proc x -> do
        info    <- textAtTag "td"          -< x
        returnA -< info

acquireTodayWeather :: IO (String, [(String, String)])
acquireTodayWeather = do
    doc <- fromUrl "http://www.jma.go.jp/en/yoho/319.html"
    desc <- runX $ doc >>> getWeather ".info"
    persp <- runX $ doc >>> getWeather ".rain"
    let todayPersp = (head . groupToDates . noNewLines) persp
    return $ (cleanDescription $ head desc, todayPersp)

main = do
    weather <- acquireTodayWeather
    print weather
