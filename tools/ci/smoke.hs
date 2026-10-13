module Main where

import Control.Monad (unless)
import Control.Exception (bracket)
import Data.List (sort)
import Foreign.C.Types
import Foreign.Storable (sizeOf)
import System.Directory (doesFileExist, doesDirectoryExist, removeFile)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (openFile, IOMode(ReadMode), hClose, hGetContents, hIsClosed)
import Network.Socket (socket, sClose, Family(AF_INET), SocketType(Stream),
                       SocketOption(Type, ReuseAddr), getSocketOption,
                       setSocketOption, packSocketType)

assert :: String -> Bool -> IO ()
assert label ok = unless ok (fail label)

checkLazyRead :: FilePath -> String -> IO ()
checkLazyRead path expected = bracket (openFile path ReadMode) hClose $ \handle -> do
    value <- hGetContents handle
    assert "lazy file contents" (value == expected)
    closed <- hIsClosed handle
    assert "lazy input closes at EOF" closed

main :: IO ()
main = do
    assert "arithmetic" ((sum [1..100] :: Integer) == 5050)
    assert "arbitrary precision" ((2^100 :: Integer) == 1267650600228229401496703205376)
    assert "Integer to Int wraps" ((fromInteger (2^32) :: Int) == 0)
    assert "lazy evaluation" (take 5 [1..] == [1,2,3,4,5 :: Int])
    assert "library import" (sort "hugs" == "ghsu")
    assert "CIntMax mapping" (sizeOf (undefined :: CIntMax) >= sizeOf (undefined :: CInt))
    now <- getPOSIXTime
    assert "gettimeofday" (now > 0)
    bracket (socket AF_INET Stream 0) sClose $ \sock -> do
        socketType <- getSocketOption sock Type
        assert "getsockopt socket type" (socketType == fromIntegral (packSocketType Stream))
        setSocketOption sock ReuseAddr 1
        reuse <- getSocketOption sock ReuseAddr
        assert "socket option round trip" (reuse /= 0)
    writeFile "ci-smoke.tmp" "Hugs file IO\n"
    exists <- doesFileExist "ci-smoke.tmp"
    assert "regular-file mode" exists
    directory <- doesDirectoryExist "."
    assert "directory mode" directory
    value <- readFile "ci-smoke.tmp"
    assert "file IO" (value == "Hugs file IO\n")
    checkLazyRead "ci-smoke.tmp" "Hugs file IO\n"
    removeFile "ci-smoke.tmp"
    writeFile "ci-smoke.tmp" ""
    checkLazyRead "ci-smoke.tmp" ""
    removeFile "ci-smoke.tmp"
    putStrLn "HUGS_CI_OK"
