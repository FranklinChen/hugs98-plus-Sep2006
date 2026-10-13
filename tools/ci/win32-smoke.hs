module Main where

import Control.Exception (bracket)
import Control.Monad (unless)
import Foreign.ForeignPtr (finalizeForeignPtr)
import Graphics.Win32.GDI.HDC (createCompatibleDC, deleteDC, selectRgn)
import Graphics.Win32.GDI.Region (createRectRgn)
import Graphics.Win32.GDI.Types (sIMPLEREGION, nULLREGION)

main :: IO ()
main = do
    bracket (createCompatibleDC Nothing) deleteDC $ \dc -> do
        bracket (createRectRgn 0 0 10 10) finalizeForeignPtr $ \region -> do
            kind <- selectRgn dc region
            unless (kind == sIMPLEREGION) (fail "SelectObject rectangle region code")
        bracket (createRectRgn 0 0 0 0) finalizeForeignPtr $ \region -> do
            kind <- selectRgn dc region
            unless (kind == nULLREGION) (fail "SelectObject empty region code")
    putStrLn "WIN32_CI_OK"
