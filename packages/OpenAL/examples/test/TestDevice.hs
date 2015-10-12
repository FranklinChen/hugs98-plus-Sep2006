import Control.Monad ( unless )
import System.IO ( hPutStrLn, stderr )
import System.Posix.Unistd ( sleep )
import Sound.OpenAL

main :: IO ()
main = do
   maybeDevice <- openDevice (Just "'( ( devices '( native null ) ) )")
   case maybeDevice of
      Nothing -> hPutStrLn stderr "openDevice failed"
      Just device -> do
         sleep 1
         ok <- closeDevice device
         unless ok $
            hPutStrLn stderr "closeDevice failed"
