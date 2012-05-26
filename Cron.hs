import Control.Concurrent
import Data.Time
import Data.Time.Lens
import Control.Monad
import Text.Regex.Applicative
import Data.Char
import Data.Maybe
import System.Process

data Time a = MakeTime
    { tSec :: a
    , tMin :: a
    , tHour :: a
    , cmd :: String
    }
    deriving Show

data NumOrStar = Num Int | Star
    deriving Show

sleep n = threadDelay (n * 10^6)

(==*) :: Int -> NumOrStar -> Bool
n ==* Star = True
n ==* Num m = n == m

checkTime :: ZonedTime -> Time NumOrStar -> Bool
checkTime currentTime time =
    round (getL seconds currentTime) ==* tSec time &&
    getL minutes currentTime ==* tMin time &&
    getL hours currentTime ==* tHour time

cron :: String -> IO ()
cron time = do
    let timeStruct = readEntry time
    currentTime <- getZonedTime
    cron' currentTime timeStruct
  where
  cron' currentTime timeStruct = do
    -- currentTime <- getZonedTime
    when (checkTime currentTime timeStruct) $
        (void $ system $ cmd timeStruct)
    sleep 1
    cron' (modL seconds (+1) currentTime) timeStruct

testTimeSpec = MakeTime Star (Num 56) (Num 12)

-- Parsing
number :: RE Char Int
number = read <$> many (psym isDigit)

star :: RE Char NumOrStar
star = Star <$ sym '*'

numOrStar :: RE Char NumOrStar
numOrStar = (Num <$> number) <|> star

entry = mkTime <$> many (numOrStar <* many (psym isSpace))
    <*> many anySym
    where
    mkTime [s,m,h] = MakeTime s m h

readEntry :: String -> Time NumOrStar
readEntry s = fromJust $ s =~ entry
