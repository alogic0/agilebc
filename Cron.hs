import Control.Concurrent

sleep n = threadDelay (n * 10^6)

hello = do
    putStrLn "Hello #agilebc!"
    hello
