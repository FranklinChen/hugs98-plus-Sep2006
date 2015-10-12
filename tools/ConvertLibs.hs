--
-- Converting fptools/libraries/ into Hugs useable form.
--
module Main(main) where

import System
import IO
import Directory
import Maybe
import List
import Monad

cpp :: String
cpp = "gcc -P -E -xc -traditional"

wash :: FilePath -> FilePath -> String -> IO ()
wash inp outp extraArgs = do
  hPutStrLn stderr ("Pre-processing: " ++ inp)
  rc <- System.system cmd
  case rc of
    ExitSuccess   -> return ()
    ExitFailure{} -> hPutStrLn stderr ("Error: " ++ show rc)
 where 
  cmd = unwords [ cpp
  		, "-D__HUGS__"
		, "-D__HASKELL98__"
		, extraArgs
		, inp
		, "-o " ++ outp
		] 

outDir :: String
outDir = "c:/src/hugs98/libraries"

inpDir :: String
inpDir = "c:/fptools/HEAD/libraries"

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator '/'  = True
isPathSeparator '\\' = True
isPathSeparator _    = False


main :: IO ()
main = do
  putStrLn "Converting the Haskell hierarchical libraries into Hugs friendly form"
  putStrLn ("Source directory: " ++ inpDir)
  putStrLn ("Output directory: " ++ outDir)
  isThere <- doesDirectoryExist inpDir
  when (not isThere)
       (hPutStrLn stderr "input directory does not exist, stopping." >> exitSuccess)
  isThere <- doesDirectoryExist outDir
  when (not isThere)
       (hPutStrLn stderr "output directory does not exist, stopping." >> exitSuccess)
  fs <- findAllFilesMatching pathFilter fileFilter inpDir
  print "done"
  let theFs = mapMaybe ofInterest fs
  mapM_ washFile theFs
  return ()
 where
      pathFilter :: [FilePath] -> Bool
      pathFilter []        = True
      pathFilter ("CVS":_) = False
      pathFilter fs@(x:_) | "_split" `isSuffixOf` x = False
      pathFilter _ = True

      fileFilter :: [FilePath] -> Bool
      fileFilter = withSuffix ["hs", "lhs"]

      ofInterest :: [String] -> Maybe ([String], Maybe String)
      ofInterest comps = Just (comps, Nothing)

      washFile (comps, mbArgs) = do
	 makeDirectory oDir
         wash (inpDir ++ pathSeparator : relNm)
	      (outDir ++ pathSeparator : relNm)
	      (fromMaybe "" mbArgs)
       where
	  rcomps = reverse comps
	  relDir = concat (intersperse [pathSeparator] (reverse (tail comps)))
	  relNm  = concat (intersperse [pathSeparator] rcomps)

	  oDir   = outDir ++ pathSeparator:relDir

exitSuccess = exitWith ExitSuccess


{-
  Given a base directory, locate all files satisfying 'pred' in
  that directory tree.
  
  The files returned are all in 
-}  
findAllFilesMatching :: ([String] -> Bool)
		     -> ([String] -> Bool)
		     -> FilePath
		     -> IO [[String]]
findAllFilesMatching predPath predFile base = go []
 where
   go prefix = do
     ls <- getDirectoryContents fPath
     let entries = filter (not.isHereUp) ls
     stuff <- mapM (\ f -> classifyEntry (f:prefix) (mkFilePath base prefix f)) entries

     let (dirs, allFiles) = unzipEithers stuff
         theFiles         = filter predFile allFiles

     print theFiles
     lss <- mapM go (filter predPath dirs)
     return (theFiles ++ concat lss)
    where
     fPath = mkFilePath base prefix ""

makeDirectory :: FilePath -> IO ()
makeDirectory fpath = do
  flg <- doesDirectoryExist fpath
  print (fpath,flg,dirName fpath)
  if flg
   then return ()
   else do
       -- try creating the parent.
     case dirName fpath of
       ""   -> createDirectory fpath
       "./" -> createDirectory fpath
       d  -> do
         flg <- doesDirectoryExist d
         when (not flg) (makeDirectory d)
         createDirectory fpath	   

classifyEntry f fpath = do
  flg <- doesDirectoryExist fpath
  return ((case flg of { True -> Left; _ -> Right}) f)


mkFilePath :: FilePath -> [String] -> String -> FilePath
mkFilePath base comps s = foldr (\ x acc -> acc ++ pathSeparator:x) base (consL s comps)
 where
   consL [] xs = xs
   consL x  xs = x:xs

isHereUp :: FilePath -> Bool
isHereUp "."  = True
isHereUp ".." = True
isHereUp _    = False

unzipEithers :: [Either a b] -> ([a], [b])
unzipEithers [] = ([], [])
unzipEithers (x:xs) = 
  case x of
    Left  v -> (v:as, bs)
    Right v -> (as, v:bs)
 where
  (as,bs)   = unzipEithers xs

withSuffix :: [String] -> [String] -> Bool
withSuffix _ []       = False
withSuffix sufs (s:_) = fileSuffix s `elem` sufs

hasSuffix [] = False
hasSuffix (x:_) = not (null (fileSuffix x))

-- FileUtil outtakes:

dirName :: FilePath -> FilePath
dirName fname =
  case revDropWhile (not.isPathSeparator) (revDropWhile isPathSeparator fname) of
    "" -> "./" -- no separator was found, dir-name is "."
    xs -> xs

revDropWhile :: (a -> Bool) -> [a] -> [a]
revDropWhile p = foldr f []
  where f x [] = if p x then [] else [x]
        f x xs = (x:xs)

-- suffix _does not_ include the dot. In case there isn't a suffix,
-- return empty string.
fileSuffix :: FilePath -> String
fileSuffix = findLast (=='.') "" 

findLast :: (Char -> Bool)
	 -> String
	 -> String
	 -> String
findLast pred noMatch f = go False f f
  where
    go matched acc [] 
      | matched   = acc
      | otherwise = noMatch
    go matched acc (x:xs)
      | pred x    = go True xs xs
      | otherwise = go matched acc xs



