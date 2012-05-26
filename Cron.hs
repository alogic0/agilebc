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

cron time action = do
    currentTime <- getZonedTime
    when (if getL seconds currentTime == tSec time) $
        action
    sleep 1
    cron time action
