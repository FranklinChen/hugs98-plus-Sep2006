module Main where

import Control.Exception (bracket)
import Control.Monad (unless)
import Graphics.Win32.GDI.HDC (createCompatibleDC, deleteDC, selectRgn)
import Graphics.Win32.GDI.Region (createRectRgn)
import Graphics.Win32.GDI.Types (sIMPLEREGION, nULLREGION)

main :: IO ()
main = do
    bracket (createCompatibleDC Nothing) deleteDC $ \dc -> do
        -- Regions have DeleteObject finalizers registered by createRectRgn.
        region <- createRectRgn 0 0 10 10
        kind <- selectRgn dc region
        unless (kind == sIMPLEREGION) (fail "SelectObject rectangle region code")
        empty <- createRectRgn 0 0 0 0
        emptyKind <- selectRgn dc empty
        unless (emptyKind == nULLREGION) (fail "SelectObject empty region code")
    putStrLn "WIN32_CI_OK"
