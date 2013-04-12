{-# LANGUAGE OverloadedStrings #-}
module Main where

--For Snap
import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

--For me
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Time
import Data.Hash.MD5 (Str (..), md5i) --from MissingH
import Data.List
import Data.Text hiding (map, concat, head, last, zip, tail, toLower)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import CensusData

girlsNames' = [ [head name] ++ (map toLower . tail) name | name <- girlsNames ]

day = do
    today <- fmap utctDay getCurrentTime
    let (year, _, _) = toGregorian today
    let days = diffDays today (fromGregorian year 0 0)
    return days


main :: IO ()
main = quickHttpServe site

usageMessage = "Access your girl's name at mygirlsname.csh.rit.edu/NAME, and bookmark. Note there is a text-only API at /textAPI/NAME"

site :: Snap ()
site =
    ifTop (writeBS usageMessage) <|>
    route [ (":name", girlName),
            ("textAPI", writeBS usageMessage),
            ("textAPI/:name", girlName)
          ]

md5 :: String -> Integer
md5 = md5i . Str

--Michiel Buddingh' thankfully went through the trouble of looking into the (output) distribution normity of various hashing functions
-- ( http://michiel.buddingh.eu/distribution-of-hash-values )
-- Particularly look at Dataset #2, as it is closest to our probable input
getGirlsName name = do
	d <- day
	let string = (name ++ (show d))
	let hash = (md5i . Str) string
	let hash' = 1000*hash
	let index = (hash' `div` (2^128))
	let intIndex = (fromInteger index :: Int)
	return $ (girlsNames' !! intIndex)

safeGetParam :: MonadSnap f => B.ByteString -> f B.ByteString
safeGetParam paramName = fromMaybe "" <$> getParam paramName

girlName :: Snap ()
girlName = do
  name <- safeGetParam "name"
  result <- liftIO $ getGirlsName $ C.unpack name
  writeBS $ C.pack result
