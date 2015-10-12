{-
   OpenALInfo.hs (modeled after OpenGL's glxinfo)
   Copyright (c) Sven Panne 2005 <sven.panne@aedion.de>
   This file is part of the ALUT package & distributed under a BSD-style license
   See the file libraries/ALUT/LICENSE
-}

import Text.PrettyPrint
import Sound.ALUT

-- This program prints some basic information about ALC and AL.

printVar :: (a -> Doc) -> GettableStateVar a -> IO ()
printVar format var = get var >>= putStrLn . render . format

printStringVar :: String -> GettableStateVar String -> IO ()
printStringVar header = printVar (\v -> text header <> colon <+> text v)

printStringListVar :: String -> GettableStateVar [String] -> IO ()
printStringListVar header =
   printVar (\xs -> text header <> colon $$
                    nest 4 (fsep (punctuate comma (map text xs))))

main :: IO ()
main =
   withProgNameAndArgs runALUT $ \_progName _args -> do
      Just ctx <- get currentContext
      Just dev <- get (contextsDevice ctx)
      printStringVar "ALC version" alcVersion
      printStringListVar "ALC extensions" (alcExtensions dev)
      printStringVar "AL version" alVersion
      printStringVar "AL renderer" alRenderer
      printStringVar "AL vendor" alVendor
      printStringListVar "AL extensions" alExtensions
