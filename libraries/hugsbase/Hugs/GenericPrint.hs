----------------------------------------------------------------
-- A "generic" (or "polymorphic") print function in Haskell
-- This is very heavily based on the code in printer.c
-- (Together with the decompiler and error catching primitives,
-- this might make a good base on which to build a debugger?)
--
-- NB This library is an _experimental_ feature which may be
--    removed in future versions of Hugs.
--    It can only be used if Hugs was configured with the
--    "--enable-internal--prims" flag.
----------------------------------------------------------------

module Hugs.GenericPrint(
        printError, 
        outputString, 
        print
        ) where

import Prelude hiding (print)

import Hugs.Internals(
        Name, nameInfo, nameString,
        Cell, getCell,
        CellKind(..), classifyCell,
        )

import Hugs.IOExts( unsafePerformIO )
import Hugs.Array
import Data.Char( showLitChar, isAlpha )
import Data.List( intersperse )

----------------------------------------------------------------
-- The top-level print routine 
----------------------------------------------------------------

printError   :: Cell -> IO ()
outputString :: String -> IO ()
print        :: a -> IO ()

printError err = do
  putStr "\nProgram error: " 
  printDBadRedex err 
  putStr "\n"

outputString s = outputStr (getCell s)

print x        = print' True (getCell x)

----------------------------------------------------------------

printBadRedex err = do
  putChar '{' 
  print' False err 
  putChar '}'

printDBadRedex err = do
  kind <- classifyCell False err
  case kind of
    Apply fun args -> do
      funkind <- classifyCell False fun
      case (funkind, args) of
        (Fun nm, [msg]) | nm == nameError ->
          outputStr msg
        _ -> printBadRedex err
    _ -> printBadRedex err

outputStr :: Cell -> IO ()
outputStr xs = do
  kind <- hugsClassifyCell True xs
  case kind of
    Apply fun args ->
      hugsClassifyCell True fun >>= \ funkind ->
      case (funkind, args) of
      (Con nm, [y,ys]) | nm == nameCons ->
        hugsClassifyCell True y >>= \ ykind ->
        case ykind of
        Char c ->
          putChar c >>
          outputStr ys
        Error err ->
          printBadRedex err >>
          outputStr ys
        _ ->
          printBadRedex y >>
          outputStr ys
      (Error err, _) ->
        printBadRedex err
      _ ->
        printBadRedex xs
    Con nm | nm == nameNil ->
        return ()
    Error err ->
        printBadRedex err
    _ ->
        printBadRedex xs

print' :: Bool -> Cell -> IO ()
print' strict x = printCell strict min_prec x

--ToDo: combine with sprint (if possible)
lprint :: Bool -> Cell -> Cell -> IO ()
lprint strict x xs =
  printCell strict min_prec x >>
  hugsClassifyCell strict xs >>= \ kind ->
  case kind of
  Apply fun args ->
      hugsClassifyCell strict fun >>= \ funkind ->
        case (funkind, args) of
        (Con nm, [y,ys]) | nm == nameCons ->
          putStr ", " >>
          lprint strict y ys
        (Error err, _) ->
          printBadRedex err
        _ ->
          putStr "] ++ " >>
          printBadRedex xs
  Con nm | nm == nameNil ->
          putChar ']'
  Error err ->
          printBadRedex err
  _ ->
          putStr "] ++ " >>
          printBadRedex xs

sprint :: Bool -> Char -> Cell -> IO ()
sprint strict c xs =
  putStr (showLitChar c "") >>
  hugsClassifyCell strict xs >>= \ kind ->
  case kind of
  Apply fun args ->
      hugsClassifyCell strict fun >>= \ funkind ->
        case (funkind, args) of
        (Con nm, [y,ys]) | nm == nameCons ->
          hugsClassifyCell strict y >>= \ ykind ->
          case ykind of
          Char c -> sprint strict c ys
          _      -> lprint False y ys
        _ ->
          putStr "\" ++ " >>
          printBadRedex xs
  Con nm | nm == nameNil ->
          putChar '"'
  _ ->
          putStr "\" ++ " >>
          printBadRedex xs

printCell :: Bool -> Int -> Cell -> IO ()
printCell strict d x =
  hugsClassifyCell strict x >>= \ kind ->
  case kind of
  Apply fun args ->
      hugsClassifyCell strict fun >>= \ funkind ->
      case funkind of
      Con nm ->
        case args of
          [x,xs] | nm == nameCons
            -> hugsClassifyCell strict x >>= \ xkind ->
               case xkind of
               Char c -> putChar '"' >> sprint strict c xs
               _      -> putChar '[' >> lprint strict x xs

          [x] | assoc /= 'A'
            -> printParen True (
                 printCell strict (fun_prec-1) x >>
                 putChar ' ' >>
                 putStr (asOp nameStr)
               )

          (x1:x2:xs) | assoc /= 'A'
            -> printParen (not (null xs) && d >= fun_prec) (
                 printParen (d <= p) (do
                   printCell strict lp x1
                   putChar ' '           
                   putStr (asOp nameStr) 
                   putChar ' '           
                   printCell strict rp x2
                   ) >>
                 mapM_ (\ arg ->
                   putChar ' ' >>
                   printCell strict p arg
                 ) xs
                 )

          xs
            -> printParen (not (null xs) && d >= fun_prec) (
                 -- test that xs is nonNull should be redundant but
                 -- no harm being robust
                 putStr (asVar nameStr)       >>
                 mapM_ (\arg ->
                   putChar ' ' >>
                   printCell strict fun_prec arg
                 ) xs
                 )
         where
          (arity, p, assoc) = nameInfo nm
          nameStr = nameString nm

          -- from Appendix E2 of Haskell 1.2 report
          lp = if assoc == 'L' then p else p+1
          rp = if assoc == 'R' then p else p+1
        
      Fun nm ->
        printParen (d >= fun_prec) (
          putStr (asVar nameStr)       >>
          mapM_ (\arg ->
            putChar ' ' >>
            -- switch to lazy printing!
            printCell False fun_prec arg
          ) args
          )
       where
        nameStr = nameString nm
      
      Tuple arity ->
        printParen (not (null extra) && d >= fun_prec) (
          printParen True (
            for__ fields (\ field ->
              printCell strict min_prec field
            ) (putChar ',') >>
            -- Haskell's syntax makes it impossible to construct an
            -- incomplete tuple - but let's play safe!
            mapM_ (\_ ->
              putChar ','
            ) [numArgs+1..arity]
          ) >>
          -- Haskell's type system makes extra arguments impossible
          -- - but let's play safe!
          mapM_ (\ arg ->
            putChar ' ' >>
            printCell strict fun_prec arg
          ) extra
        )
       where
        (fields, extra) = splitAt arity args

      Error err ->
          printBadRedex err

      _
        -> printParen (not (null args) && d >= fun_prec) (
             printCell strict fun_prec fun   >>
             mapM_ (\arg ->
               putChar ' ' >>
               printCell strict fun_prec arg
             ) args
             )
     where
        numArgs = length args

  Fun nm ->
    putStr (asVar (nameString nm))

  Con nm ->
    putStr (asVar (nameString nm))

  Tuple arity ->
    putStr ('(' : replicate arity ',' ++ ")")

  Int x ->
    putStr (show x)

  Integer x ->
    putStr (show x)

  Float x ->
    putStr (show x)

  Char x ->
    putStr ('\'' : showLitChar x "\'")

  Prim prim ->
    putStr prim

  Error err ->
    printBadRedex err

----------------------------------------------------------------
-- Cell/Name utilities
----------------------------------------------------------------

nameCons    =  cellName (:)
nameNil     =  cellName []
nameError   =  cellName error

-- Here's something VERY subtle.
-- We use classifyCell instead of hugsClassifyCell because
-- otherwise, this gets put in the same dependency class as everything
-- else and the lack of polymorphic recursion bites us.
-- (Using classifyCell is equally good here because it wont fail.)
cellName :: a -> Name
cellName x = unsafePerformIO (
  classifyCell True (getCell x) >>= \ kind ->
  case kind of
  Fun nm -> return nm
  Con nm -> return nm
  )

-- This implements the error-handling policy:
hugsClassifyCell :: Bool -> Cell -> IO CellKind
hugsClassifyCell strict obj =
  classifyCell strict obj >>= \ kind ->
  case kind of
  Error err ->
    if failOnError then
      exitWith (printError err)
    else
      return kind
  _ ->
    return kind

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

for__ :: Monad m => [a] -> (a -> m ()) -> m () -> m ()
for__ xs f inc = sequence_ $ intersperse inc $ map f xs

min_prec, max_prec, fun_prec :: Int
min_prec = 0
max_prec = 9
fun_prec = max_prec+2

asOp str
 | isOp str  = str
 | otherwise = '`' : str ++ "`"

asVar str
 | isOp str  = '(' : str ++ ")"
 | otherwise = str

isOp (c:_) = not (isAlpha c || c == '[')
isOp _     = False

printParen :: Bool -> IO () -> IO ()
printParen True m  = putChar '(' >> m >> putChar ')'
printParen False m = m

----------------------------------------------------------------
-- Missing primitives
----------------------------------------------------------------

-- In Hugs0, this accessed the value of the :set -f" flag
failOnError :: Bool
failOnError = True

-- In Hugs0, this executed the action and terminated the current evaluation
exitWith :: IO () -> IO a
exitWith m = m >> error "{exitWith}"

----------------------------------------------------------------
