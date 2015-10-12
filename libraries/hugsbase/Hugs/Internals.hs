----------------------------------------------------------------
-- Primitives for accessing Hugs internals.
--
-- NB These primitives are an _experimental_ feature which may be
--    removed in future versions of Hugs.
--    They can only be used if hugs was configured with the
--    "--enable-internal-prims" flag.
--
-- The primitives defined in this module provide the means with
-- which to implement simple error-recovery and debugging facilities
-- in Haskell.  
--
-- The error catching primitive only works if the "failOnError" flag 
-- is FALSE - ie Hugs was invoked with the "-f" flag.
--
-- Despite appearances, these primitives are referentially transparent
-- (with the exception of the rarely used pointer equality operations)
-- (The proof is really neat - but there just isn't enough space in the margin)
----------------------------------------------------------------

module Hugs.Internals(
	ptrEq,

	Name,
	  nameString,
	  nameInfo,
	  nameEq,
	Cell,
	  getCell,
	  cellPtrEq,
	CellKind(..),
	  classifyCell,

	catchError,

	Addr,
          nameCode,
	Instr(..),
	  instrAt, instrsAt,

	) where

import Hugs.Prelude hiding ( Addr )

----------------------------------------------------------------
-- pointer equality
----------------------------------------------------------------

-- breaks referential transparency - use with care
primitive ptrEq "unsafePtrEq" :: a -> a -> Bool


----------------------------------------------------------------
-- Name
----------------------------------------------------------------

data Name
-- newtype Name = Name Int

-- returns (arity, precedence, associativity)
primitive nameInfo       :: Name -> (Int, Int, Char)
primitive nameString     :: Name -> String
primitive nameEq         :: Name -> Name -> Bool

instance Show Name where
  showsPrec _ nm = showString (nameString nm)

instance Eq Name where
  (==) = nameEq


----------------------------------------------------------------
-- Cell
-- Note: cellPtrEq breaks referential transparency - use with care
----------------------------------------------------------------

data Cell

primitive getCell                  :: a -> Cell
primitive cellPtrEq                :: Cell -> Cell -> Bool
primitive catchError "catchError2" :: a -> Either Cell a

instance Show Cell where 
  showsPrec _ _ = showString "{Cell}"

----------------------------------------------------------------
-- CellType
----------------------------------------------------------------

data CellKind       
  = Apply   Cell [Cell]
  | Fun     Name    
  | Con     Name    
  | Tuple   Int         
  | Int     Int         
  | Integer Integer   
  | Float   Float       
  | Double  Double       
  | Char    Char        
  | Prim    String      
  | Error   Cell  
  deriving (Show)

primitive classifyCell :: Bool -> Cell -> IO CellKind

----------------------------------------------------------------
-- Addr
----------------------------------------------------------------

newtype Addr  = Addr  Int deriving (Eq, Show)

s :: Addr -> Addr
s (Addr a) = Addr (a+1)

primitive nameCode    :: Name -> Addr
primitive intAt       :: Addr -> Int
primitive floatAt     :: Addr -> Float
primitive doubleAt    :: Addr -> Double
primitive cellAt      :: Addr -> Cell
primitive nameAt      :: Addr -> Name
primitive textAt      :: Addr -> String
primitive addrAt      :: Addr -> Addr
primitive bytecodeAt :: Addr -> Bytecode


----------------------------------------------------------------
-- Bytecode
----------------------------------------------------------------

newtype Bytecode = Bytecode Int deriving (Eq, Show)

iLOAD    = Bytecode 0
iCELL	 = Bytecode 1
iCHAR	 = Bytecode 2
iINT	 = Bytecode 3
iFLOAT	 = Bytecode 4
iSTRING	 = Bytecode 5
iMKAP	 = Bytecode 6
iUPDATE	 = Bytecode 7
iUPDAP	 = Bytecode 8
iEVAL	 = Bytecode 9
iRETURN	 = Bytecode 10
iTEST	 = Bytecode 11
iGOTO	 = Bytecode 12
iSETSTK	 = Bytecode 13
iROOT	 = Bytecode 14
iDICT	 = Bytecode 15
iFAIL	 = Bytecode 16
iALLOC	 = Bytecode 17
iSLIDE	 = Bytecode 18
iSTAP	 = Bytecode 19
iTABLE	 = Bytecode 20
iLEVAL	 = Bytecode 21
iRUPDAP	 = Bytecode 22
iRUPDATE = Bytecode 23

data Instr 
  = LOAD    Int
  | CELL    Cell
  | CHAR    Char
  | INT	    Int    
  | FLOAT   Float   	  
  | DOUBLE  Double
  | STRING  String  	  
  | MKAP    Int   
  | UPDATE  Int  	  
  | UPDAP   Int  	  
  | EVAL    	   
  | RETURN  	   
  | TEST    Name Addr
  | GOTO    Addr  	  
  | SETSTK  Int  	  
  | ROOT    Int  	  
  | DICT    Int
  | FAIL    	   
  | ALLOC   Int
  | SLIDE   Int	   
  | STAP    	   
  | TABLE   	   
  | LEVAL   Int	   
  | RUPDAP  	   
  | RUPDATE 
  deriving (Show)

instrAt :: Addr -> (Instr, Addr)
instrAt pc = case bytecodeAt pc of 
  i | i == iLOAD    -> (LOAD    (intAt   (s pc)), s (s pc))
  i | i == iCELL    -> (CELL    (cellAt  (s pc)), s (s pc))
  i | i == iCHAR    -> (CHAR    (toEnum (intAt (s pc))), s (s pc))
  i | i == iINT     -> (INT     (intAt   (s pc)), s (s pc))
  i | i == iFLOAT   -> (FLOAT   (floatAt (s pc)), s (s pc))
  i | i == iSTRING  -> (STRING  (textAt  (s pc)), s (s pc))
  i | i == iMKAP    -> (MKAP    (intAt   (s pc)), s (s pc))
  i | i == iUPDATE  -> (UPDATE  (intAt   (s pc)), s (s pc))
  i | i == iUPDAP   -> (UPDAP   (intAt   (s pc)), s (s pc))
  i | i == iEVAL    -> (EVAL                    , s pc)
  i | i == iRETURN  -> (RETURN                  , s pc)
  i | i == iTEST    -> (TEST    (nameAt  (s pc)) (addrAt (s (s (pc)))), s (s (s pc)))
  i | i == iGOTO    -> (GOTO    (addrAt  (s pc)), s (s pc))
  i | i == iSETSTK  -> (SETSTK  (intAt   (s pc)), s (s pc))
  i | i == iROOT    -> (ROOT    (intAt   (s pc)), s (s pc))
  i | i == iDICT    -> (DICT    (intAt   (s pc)), s (s pc))
  i | i == iFAIL    -> (FAIL                    , s pc)
  i | i == iALLOC   -> (ALLOC   (intAt   (s pc)), s (s pc))
  i | i == iSLIDE   -> (SLIDE   (intAt   (s pc)), s (s pc))
  i | i == iSTAP    -> (STAP                    , s pc)
  i | i == iTABLE   -> (TABLE                   , s pc)
  i | i == iLEVAL   -> (LEVAL   (intAt   (s pc)), s (s pc))
  i | i == iRUPDAP  -> (RUPDAP                  , s pc)
  i | i == iRUPDATE -> (RUPDATE                 , s pc)

-- list of instructions starting at given address
instrsAt :: Addr -> [Instr]
instrsAt pc = let (i, pc')  = instrAt pc in i : instrsAt pc'


----------------------------------------------------------------



----------------------------------------------------------------
-- tests
----------------------------------------------------------------

-- test1, test2 :: Either Cell Int
-- 
-- test1 = catchError (error "foo")
-- test2 = catchError 1
-- 
-- 
-- test3, test4, test5 :: Int
-- 
-- test3 = myCatch (1+error "foo") 2
-- test4 = myCatch 1 (error "bar")
-- test5 = myCatch (error "foo") (error "bar")
-- 
-- 
-- test6, test7, test8, test9 :: IO ()
-- 
-- test6 = printString "abcdefg"
-- test7 = printString (error "a" : "bcdefg")
-- test8 = printString ("abc" ++ error "defg")
-- test9 = printString (error "a" : "bc" ++ error "defg")
-- 
-- -- if an error occurs, replace it with a default (hopefully error-free) value
-- myCatch :: a -> a -> a
-- myCatch x deflt = case catchError x of
-- 		   Right x' -> x'
-- 		   Left _   -> deflt
-- 
-- -- lazily print a string - catching any errors as necessary
-- printString :: String -> IO ()
-- printString str =
--   case catchError str of
--   Left _       -> putStr "<error>"
--   Right []     -> return ()
--   Right (c:cs) -> case catchError c of
-- 		     Left _   -> putStr "<error>" >> printString cs
-- 		     Right c' -> putChar c' >> printString cs



