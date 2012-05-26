import Control.Concurrent
import Data.Time
import Data.Time.Lens
import Control.Monad

data Time a = MakeTime
    { tSec :: a
    , tMin :: a
    , tHour :: a
    }
    deriving Show

data NumOrStar = Num Int

sleep n = threadDelay (n * 10^6)

checkTime :: ZonedTime -> Time Int -> Bool
checkTime currentTime time =
    round (getL seconds currentTime) == tSec time &&
    getL minutes currentTime == tMin time &&
    getL hours currentTime == tHour time

cron :: Time Int -> IO () -> IO ()
cron time action = do
    currentTime <- getZonedTime
    when (checkTime currentTime time) $
        action
    sleep 1
    cron time action
