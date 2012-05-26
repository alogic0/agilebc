import Control.Concurrent

sleep n = threadDelay (n * 10^6)

cron action = do
    action
    sleep 1
    cron action
