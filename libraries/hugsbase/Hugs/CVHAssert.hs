----------------------------------------------------------------
-- This is a simple implementation of Cordy Hall's assertions
-- (for performance debugging).
--
-- NB These primitives are an _experimental_ feature which may be
--    removed in future versions of Hugs.
--    They can only be used if hugs was configured with the
--    "--enable-internal-prims" flag.
--
-- These primitives mostly break referential transparency - but you're
-- only supposed to use them for debugging purposes.
----------------------------------------------------------------

module Hugs.CVHAssert(
	Test, Action, 
	assert,
	isEvaluated,
	pointerEqual
	) where

import Hugs.Internals(
	ptrEq,
	Name,   nameInfo,
	Cell,   getCell, cellPtrEq,
	CellKind(..), classifyCell,
	)
import Hugs.IOExts(
	unsafePerformIO
	)

----------------------------------------------------------------
-- High level operations
----------------------------------------------------------------

type Test a   = a -> Bool
type Action a = a -> IO ()

assert :: Test a -> Action a -> a -> a
assert test action x = 
  unsafePerformIO (if test x then return () else action x)
  `seq`
  x 

isEvaluated :: a -> Bool
isEvaluated x = unsafePerformIO (
  isEvaluatedCell (getCell x)
  )

representationSize :: a -> Int
representationSize x = unsafePerformIO (do 
  cells <- cellsOf (getCell x) []
  return (cellSize * length cells)
  )

pointerEqual :: a -> a -> Bool
pointerEqual = ptrEq

----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

isEvaluatedCell :: Cell -> IO Bool
isEvaluatedCell cell = do
  kind <- classifyCell False cell

  case kind of
    Apply fun args -> do 
		    funkind <- classifyCell False fun
                    case funkind of
                      Fun nm    -> return (nameArity nm > length args)
                      _         -> return True
    _            -> return True

arityOf :: Cell -> IO Int
arityOf cell = do
  kind <- classifyCell False cell

  case kind of
    Apply fun args -> do 
		    funarity <- arityOf fun
		    return (funarity - length args)

    Fun   nm -> return (nameArity nm)
    Con   nm -> return (nameArity nm)
    Tuple i  -> return i
    _            -> return 0


nameArity :: Name -> Int
nameArity nm = case nameInfo nm of (arity,_,_) -> arity

-- list cells occurring in Cell
cellsOf :: Cell -> [Cell] -> IO [Cell]

cellsOf cell seen 
  | cell `elemCell` seen 
  = return seen
  | otherwise
  = do
      let seen' = cell:seen

      kind <- classifyCell False cell

      case kind of
	Apply f xs -> do
			    seen'' <- cellsOf f seen'
			    cellsOf' xs seen''
	Fun     _  -> return seen'
	Con     _  -> return seen'
	Tuple   _  -> return seen'
	Int     _  -> return seen'
	Integer _  -> return seen'
	Float   _  -> return seen'
	Char    _  -> return seen'
	Prim    _  -> return seen'
	Error   _  -> return seen'	-- we could argue about this one

cellsOf' :: [Cell] -> [Cell] -> IO [Cell]
cellsOf' []     seen = return seen
cellsOf' (x:xs) seen = do seen' <- cellsOf x seen
                          cellsOf' xs seen'

elemCell :: Cell -> [Cell] -> Bool
x `elemCell` []     = False
x `elemCell` (y:ys) = x `cellPtrEq` y || x `elemCell` ys

cellSize :: Int
cellSize = 8

----------------------------------------------------------------
