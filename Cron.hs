import Control.Concurrent
import Data.Time
import Data.Time.Lens
import Control.Monad

data Time = MakeTime
    { tSec :: Int
    , tMin :: Int
    , tHour :: Int
    }
    deriving Show

sleep n = threadDelay (n * 10^6)

checkTime :: ZonedTime -> Time -> Bool
checkTime currentTime time =
    round (getL seconds currentTime) == tSec time &&
    getL minutes currentTime == tMin time &&
    getL hour currentTime == tHour time

cron :: Time -> IO () -> IO ()
cron time action = do
    currentTime <- getZonedTime
    when (checkTime currentTime time) $
        action
    sleep 1
    cron time action
