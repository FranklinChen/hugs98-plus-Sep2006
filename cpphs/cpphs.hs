{-
-- The main program wrapper for cpphs, a simple C pre-processor
-- written in Haskell.

-- Author: Malcolm Wallace, 2004
-- This file is licensed under the GPL.  Note however, that all other
-- modules used by it are either distributed under the LGPL, or are Haskell'98.
--
-- Thus, when compiled as a standalone executable, this program will fall
-- under the GPL.
-}
module Main where

import System ( getArgs, getProgName, exitWith, ExitCode(..) )
import Maybe
import Language.Preprocessor.Cpphs ( runCpphs, CpphsOption, parseOption )
import IO     ( stdout, IOMode(WriteMode), openFile, hPutStr, hFlush, hClose )
import Monad  ( when )
import List   ( isPrefixOf )


version :: String
version = "1.2"


main :: IO ()
main = do
  args <- getArgs
  args <- return $ if "--cpp" `elem` args then convertArgs args else args
  
  prog <- getProgName
  when ("--version" `elem` args)
       (do putStrLn (prog++" "++version)
           exitWith ExitSuccess)
  when ("--help" `elem` args)
       (do putStrLn ("Usage: "++prog
                ++" [file ...] [ -Dsym | -Dsym=val | -Ipath ]*  [-Ofile]\n"
                ++"\t\t[--nomacro] [--noline] [--text]"
                ++" [--strip] [--hashes] [--layout]"
                ++" [--unlit] [--cpp]")
           exitWith ExitSuccess)

  let parsedArgs = parseOptions args
      Right (opts, ins, outs) = parsedArgs
      out = listToMaybe outs
  
  when (isLeft parsedArgs)
       (do putStrLn $ "Unknown option, for valid options try "
                      ++prog++" --help\n"++fromLeft parsedArgs
           exitWith (ExitFailure 1))
  when (length outs > 1)
       (do putStrLn $ "At most one output file (-O) can be specified"
           exitWith (ExitFailure 2))
  if null ins then execute opts out Nothing
              else mapM_ (execute opts out) (map Just ins)


isLeft (Left _) = True
isLeft _ = False

fromLeft (Left x) = x

-- | Parse the list of options
--   Return either Right (options, input files, output files)
--   or Left invalid flag
parseOptions :: [String] -> Either String ([CpphsOption], [FilePath], [FilePath])
parseOptions xs = f ([], [], []) xs
  where
    f (opts, ins, outs) (('-':'O':x):xs) = f (opts, ins, x:outs) xs
    f (opts, ins, outs) (x@('-':_):xs) = case parseOption x of
                                           Nothing -> Left x
                                           Just a  -> f (a:opts, ins, outs) xs
    f (opts, ins, outs) (x:xs) = f (opts, x:ins, outs) xs
    f (opts, ins, outs) []     = Right (reverse opts, reverse ins, reverse outs)


-- | Parse a list of options, remaining compatible with cpp if possible
--   Based on a shell script cpphs.compat
data ConvertArgs = ConvertArgs {traditional :: Bool, strip :: Bool, infile :: String, outfile :: String}

convertArgs :: [String] -> [String]
convertArgs xs = f (ConvertArgs False True "-" "-") xs
    where
        flg = "DUI"
    
        f e (['-',r]:x:xs) | r `elem` flg = ('-':r:x) : f e xs
        f e (x@('-':r:_):xs) | r `elem` flg = x : f e xs
        f e ("-o":x:xs) = ('-':'O':x) : f e xs
        f e (('-':'o':x):xs) = ('-':'O':drop 2 x) : f e xs
        f e (('-':x):xs) | "ansi" `isPrefixOf` x = f e{traditional=False} xs
                         | "tranditional" `isPrefixOf` x = f e{traditional=True} xs
                         | "std" `isPrefixOf` x = f e xs -- ignore language spec
        f e ("-x":x:xs) = f e xs -- ignore langauge spec
        f e ("-include":x:xs) = x : f e xs
        f e ("-P":xs) = "--noline" : f e xs
        f e (x:xs) | x == "-C" || x == "-CC" = f e{strip=False} xs
        f e ("-A":x:xs) = f e xs -- strip assertions
        f e ("--help":xs) = "--help" : f e xs
        f e ("--version":xs) = "--version" : f e xs
        f e ("-version":xs) = "--version" : f e xs
        f e (('-':x):xs) = f e xs -- strip all other flags
        f e (x:xs) = f (if infile e == "-" then e{infile=x} else e{outfile=x}) xs
        
        f e [] = ["--hashes" | not (traditional e)] ++
                 ["--strip" | strip e] ++
                 [infile e] ++
                 ["-O" ++ outfile e | outfile e /= "-"]



-- | Execute the preprocessor,
--   using the given options; an output path; and an input path.
--   If the filepath is Nothing then default to stdout\/stdin as appropriate.
execute :: [CpphsOption] -> Maybe FilePath -> Maybe FilePath -> IO ()
execute opts output input =
  let (filename, action) =
        case input of
          Just x -> (x, readFile x)
          Nothing -> ("stdin", getContents)
  in
  do contents <- action
     result <- runCpphs opts filename contents
     case output of
       Nothing -> do putStr result
                     hFlush stdout
       Just x  -> do h <- openFile x WriteMode
                     hPutStr h result
                     hFlush h
                     hClose h
