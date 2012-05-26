import Control.Concurrent

data Time = MakeTime
    { tSec :: Int
    , tMin :: Int
    , tHour :: Int
    }

sleep n = threadDelay (n * 10^6)

cron action = do
    action
    sleep 1
    cron action
