module Main where

import Control.Exception (bracket)
import Control.Monad (replicateM_, unless)
import Network.Socket
import System.Posix.Process (getProcessID)
import System.Posix.User (getEffectiveUserID, getEffectiveGroupID)

main :: IO ()
main = do
    pid <- getProcessID
    uid <- getEffectiveUserID
    gid <- getEffectiveGroupID
    let expected = (fromIntegral pid, fromIntegral uid, fromIntegral gid)
    bracket (socketPair AF_UNIX Stream 0)
            (\(a, b) -> sClose a >> sClose b) $ \(a, b) -> do
        -- Exercise both endpoints repeatedly, including allocation lifetimes.
        replicateM_ 100 $ do
            first <- getPeerCred a
            second <- getPeerCred b
            unless (first == expected && second == expected) $
                fail "socket peer credentials differ from the creating process"
    putStrLn "PEERCRED_CI_OK"
