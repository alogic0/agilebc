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

cron :: Time -> IO () -> IO ()
cron time action = do
    currentTime <- getZonedTime
    when (round (getL seconds currentTime) == tSec time) $
        action
    sleep 1
    cron time action
