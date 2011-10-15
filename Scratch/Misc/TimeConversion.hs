{-|
Module      : $Header$
License     : BSD3
Stability   : unstable
Portability : portable

Module to profile and tune up time converting functions.

Situation is, we want to convert graduately increasing posix seconds to Zoned
time. Looking more efficient way than merely applying conversion function
everytime, to every posix seconds.

-}
module TimeConversion where

import Data.Time
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import qualified Data.Map as M

{-

Naive conversion simply using posixSecondsToUTCTime and utcToZonedTime from
Data.Time package. This uses lots of divMod' from Data.Fixed package, and
taking almost 40% of computation time in quote-parser.

-}

convert :: POSIXTime -> ZonedTime
convert = utcToZonedTime jst . posixSecondsToUTCTime

jst :: TimeZone
jst = TimeZone 540 False "JST"

{-

How can we memoize increasing time?

One problem is, carrying up the whole year, month, day, hour, min can happen
with single movement of seconds, when changing from one second before midnight
to 00:00:00 of new date.

We define a new data type, to hold whether each segment (min, hour, day ...)
will go back to its least value with next increase. Also, we will memoize the
value with minutes.

-}

-- | Successing time. Never decrease.
data ForwardTime  = ForwardTime
  { stDay :: Int
  , stHour :: (Bool,Int)
  , stMinute :: (Bool,Int)
  , stSecond :: (Bool,Int)
  , stMiliSec :: Int
  } deriving (Eq, Show)

convert' :: POSIXTime -> ZonedTime
convert' = undefined

pt2fwdt :: POSIXTime -> ForwardTime
pt2fwdt = undefined

fwdt2zt :: ForwardTime -> ZonedTime
fwdt2zt = undefined

memoize'min :: POSIXTime -> M.Map POSIXTime ForwardTime -> ForwardTime
memoize'min pt stMap = undefined

timeMap :: M.Map POSIXTime ForwardTime
timeMap = undefined

main :: IO ()
main = do
  let psxt = fromIntegral (10^9)
  go psxt 100000
  where
    go _ 0 = return ()
    go t n = print (convert t) >> go (t+1) (n-1)
