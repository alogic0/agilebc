import Control.Concurrent
import Data.Time
import Data.Time.Lens

data Time = MakeTime
    { tSec :: Int
    , tMin :: Int
    , tHour :: Int
    }
    deriving Show

sleep n = threadDelay (n * 10^6)

cron time action = do
    currentTime <- getZonedTime
    action
    sleep 1
    cron action
