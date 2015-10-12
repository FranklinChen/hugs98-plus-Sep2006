--
-- .NET library
--
-- (c) 2002, sof
--
module Dotnet 
	( Object	-- abstract. Instances: Eq, Show
	, isNullObj     -- :: Object a -> Bool

	, ( # )         -- :: a -> (a -> IO b) -> IO b
	, ( ## )        -- :: IO a -> (a -> IO b) -> IO b

	   -- marshalling classes
	, NetType (arg, result)
	, NetArg(..)
	, InArg

	, createObj     -- :: ClassName -> [InArg] -> IO (Object a)
	, new	        -- :: ClassName -> IO (Object a)

	   -- all String synonyms.
	, FieldName
	, ClassName
	, MethodName
	
	  -- low-level method invokers.
	, method	-- :: (NetType a) 
	                -- => MethodName
			-- -> [InArg]
                        -- -> Object b 
			-- -> IO a
	, staticMethod  -- :: (NetType a)
                        -- => ClassName
	                -- -> MethodName
                        -- -> [InArg]
                        -- -> IO a

	, method_       -- :: MethodName
                        -- -> [InArg]
                        -- -> Object a
                        -- -> IO ()
	, staticMethod_ -- :: ClassName
	        	-- -> MethodName
	                -- -> [InArg]
                        -- -> IO ()

	, fieldGet -- :: (NetType a) => FieldName -> Object b -> IO a
	, fieldSet -- :: (NetType a) => FieldName -> Object b -> a -> IO ()
	
	, staticFieldGet -- :: (NetType a) => ClassName -> FieldName -> IO a
	, staticFieldSet -- :: (NetType a) => ClassName -> FieldName -> a -> IO ()
	
	   -- automatic marshalling, provided you 'uncurry' the arguments, i.e.,
	   --     foo # invoke "MyMethod" (arg1,arg2,arg3)
           --
	, newObj       -- :: (NetArg a) => ClassName -> a -> IO (Object res)
	, invoke       -- :: (NetArg a, NetType res)
		       -- => MethodName
	               -- -> a
	               -- -> Object b
		       -- -> IO (Object res)
	, invokeStatic -- :: (NetArg a, NetType res)
	 	       -- => ClassName
		       -- -> MethodName
	               -- -> a
	               -- -> Object b
	               -- -> IO (Object res)
	
	   -- creates a delegator, that is a System.Delegator object
	   -- that when invoked, will execute the provided Haskell function
	   -- value. Not particularly type safe.
	, newDelegator -- :: (Object a -> Object b -> IO ())
		       -- -> IO (Object c)

	   -- converting 
	, hsString     -- :: Object a -> IO String
	, hsValue      -- :: (NetType a) => Object obj -> IO a
	, boxValue     -- :: (NetType a) => a -> IO (Object obj)

	, msgBox       -- :: String -> String -> IO ()	
	
	, BaseType(..)
	, Class(..)
	, Method(..)
	, defineClass 

	, mkVector     -- :: BaseType -> Int -> IO (Object a)

	   -- no longer needed, use boxValue and hsValue, respectively, instead.
	, mkInt32
	, toInt32

	  -- use with care.
	, castObjTy     -- :: Object a -> Object b
	, objType       -- :: Object a -> a
	

	  -- ToDo: remove; use mkVector.
	, mkPrimVector
	
	, newString
	, toString
	, isNullPtr
	, getStaticField
	, setStaticField
	, getField
	, setField
	
	) where

import Hugs.Prelude
import Data.Char
import Int
import Word
import IOExts
--import Foreign ( makeStablePtr )
import StablePtr ( newStablePtr )

import Monad
import List ( intersperse )
import Maybe
--ximport Char ( chr )

infix 8 #
infix 9 ##

-- OO-style application operators:
( # )  :: a -> (a -> IO b) -> IO b
obj # method = method obj

( ## ) :: IO a -> (a -> IO b) -> IO b
mObj ## method = mObj >>= method

{-
 At the heart of it all is the representation
 of object references - the Object type.
 
 The phantom type variable is used to maintain
 type safety.
-}
-- value equality
instance Eq (Object a) where
  (==) obj1 obj2 = unsafePerformIO $ 
      obj1 # invoke "Equals" obj2

instance Show (Object a) where
  show obj1 = unsafePerformIO $ 
     obj1 # invoke "ToString" ()


castObjTy :: Object a -> Object b
castObjTy o = unsafeCoerce o

objType :: Object a -> a
objType = error "objType"

isNullObj :: Object a -> Bool
isNullObj x = unsafePerformIO (isNullPtr x)

type InArg = IO (Object ())

class NetType a where
  arg    :: a -> InArg
  result :: Object () -> IO a

instance NetType (Object a) where
  arg    x = return (castObjTy x)
  result x = return (castObjTy x)

instance NetType () where
  arg ()   = error "NetType.arg{()}: not defined"
  result _ = return ()

instance NetType Int where
  arg i  = mkInt i
  result x = toInt_ x

instance NetType Int8 where
  arg i  = mkSByte (fromIntegral i)
  result x = toSByte x >>= return.fromIntegral

instance NetType Int16 where
  arg i  = mkInt32 (fromIntegral i)
  result x = toInt32 x >>= return.fromIntegral

instance NetType Int32 where
  arg i  = mkInt32 (fromIntegral i)
  result x = toInt32 x >>= return.fromIntegral

instance NetType Word32 where
  arg i  = mkUInt32 i
  result x = toUInt32 x

instance NetType Word16 where
  arg i  = mkUInt32 (fromIntegral i)
  result x = toUInt32 x >>= return.fromIntegral

instance NetType Word8 where
  arg i  = mkByte (chr (fromIntegral i))
  result x = toByte x >>= return.fromIntegral.ord

instance NetType Bool where
  arg i  = mkBool i
  result x = toBool x

instance NetType Char where
  arg i  = mkByte i
  result x = toByte x

instance NetType String where
  arg s = newString s
  result x = toString x

instance NetType Float where
  arg i  = mkSingle i
  result x = toSingle x

instance NetType Double where
  arg i  = mkDouble i
  result x = toDouble x


-- self-documenting type synonyms
type ClassName = String
type FieldName = String
type MethodName = String

{-
 Creating a new Object: via a parameterised constructor.
-}
createObj :: ClassName -> [InArg] -> IO (Object a)
createObj clsName args = do
  ls    <- sequence args
  args  <- mkArgs ls
  primCreateObject clsName args

{-
 Creating a new Object via the nullary constructor.
-}
new :: ClassName -> IO (Object a)
new clsName = primCreateObject clsName emptyArgArray

emptyArgArray :: Object ()
emptyArgArray = unsafePerformIO $ do
   args <- newArgArray 0
   return args

mkArgs :: [Object ()] -> IO (Object a)
mkArgs ls = do
   args <- newArgArray (length ls)
   zipWithM_ (setArrayArg args)
	     ls
	     [(0::Int)..]
   return args

buildArgs :: [String] -> IO (Object a)
buildArgs ls = do
   args <- newArgArray (length ls)
   zipWithM_ (\ idx v -> do
   		x <- newString v
		setArrayArg args x idx)
	     [(0::Int)..]
	     ls
   return args

{-
 Method invocation wrappers -- you currently have the choice of
 using the generic 'method'/'staticMethod' kind, which requires
 you to explicitly marshal the arguments, or use the method_X, where
 X is the arity of the method (and the number of arguments expected
 by the method_X wrapper.)
-}

method :: (NetType a)
       => MethodName
       -> [InArg]
       -> Object b
       -> IO a
method methName args obj = do
  ls    <- sequence args
  args  <- mkArgs ls
  res   <- invokeMethod obj methName args
  result res

staticMethod :: (NetType a)
             => ClassName
	     -> MethodName
             -> [InArg]
             -> IO a
staticMethod clsName methName args = do
  ls    <- sequence args
  args  <- mkArgs ls
  res   <- invokeStaticMethod (mkStaticMethod clsName methName) args
  result res

mkStaticMethod clsName mName = clsName ++ '.':mName

method_ :: MethodName
        -> [InArg]
        -> Object a
        -> IO ()
method_ methName args obj = do
  ls    <- sequence args
  args  <- mkArgs ls
  res   <- invokeMethod obj methName args
  return ()

staticMethod_ :: ClassName
	      -> MethodName
              -> [InArg]
              -> IO ()
staticMethod_ clsName methName args = do
  ls    <- sequence args
  args  <- mkArgs ls
  res   <- invokeStaticMethod (mkStaticMethod clsName methName) args
  return ()

fieldGet :: (NetType a) => FieldName -> Object b -> IO a
fieldGet fName obj = do
  res <- getField obj fName
  result (castObjTy res)

staticFieldGet :: (NetType a) => ClassName -> FieldName -> IO a
staticFieldGet cName fName = do
  res <- getStaticField cName fName
  result (castObjTy res)

fieldSet :: (NetType a) => FieldName -> a -> Object b -> IO ()
fieldSet fName val obj = do
  p     <- arg val
  setField obj fName p

staticFieldSet :: (NetType a) => ClassName -> FieldName -> a -> IO ()
staticFieldSet cName fName val = do
  p     <- arg val
  setStaticField cName fName p

data System obj
data Delegate obj

foreign import dotnet
  "static Hugs.Wrapper.DefineDelegator"
  defineDelegator :: String -> StablePtr a -> IO String

newDelegator :: (Object a -> Object b -> IO ())
	     -> IO (Object (System (Delegate ())))
newDelegator fun = do
  sp   <- newStablePtr (delegatorWrapper fun)
  tyNm <- defineDelegator "Delegate" sp
  obj  <- new tyNm
  obj # fieldGet "Delegate_handler"
 where
   delegatorWrapper :: (Object a -> Object b -> IO ())
   		    -> Object a -> Object b -> IO ()
   delegatorWrapper inner obj1 obj2 = inner obj1 obj2

{-
 To support the creation of .NET classes/types whose methods
 are implemented in Haskell, we provide the Class and Method
 data types. 
 
 Status: experimental.
-}
data Class 
 = Class String		-- type/class name
 	 (Maybe String) -- Just x => derive from x
 	 [Method]

{-
 The Method type describes the mapping between a .NET method,
 and the Haskell function value which implements it. 
 Terribly type-unsafe at the moment.
-} 
data Method
 = Method MethodName       -- .NET name (unqualified).
	  Bool             -- True => override.
 	  String           -- Haskell function to call.
	  [BaseType]       -- Argument types
	  (Maybe BaseType) -- result (Nothing => void).

-- ToDo: automate this -- given a Haskell function value, automatically
--       derive the .NET arguments it expects (and returns). i.e., as in
--       Lambada.

data BaseType
  = ObjectTy  (Maybe ClassName) -- Nothing => System.Object
  | StringTy 
  | IntTy
  | ByteTy
  | BooleanTy
  | CharTy
  | DoubleTy
  | Int16Ty
  | Int32Ty
  | Int64Ty
  | SByteTy
  | SingleTy
  | UInt16Ty
  | UInt32Ty
  | UInt64Ty
  | VoidTy
    deriving ( Eq )

foreign import dotnet
  "static Hugs.Wrapper.DefineType"
  defineType :: String -> String -> String -> IO String

-- create a new class/type + an instance of it (via the default constructor.)
defineClass :: Class -> IO (Object b)
defineClass cls@(Class clsName mbFrom meths) = do
  tyStr    <- defineType clsName superTy methString
  if (null tyStr)
   then ioError (userError "unable to create class")
   else new tyStr
 where
  superTy    = fromMaybe "" mbFrom
  methString = concat $ intersperse "/" $ map mkFunctionInfo meths

  mkFunctionInfo (Method name override haskellFun argus mbRes) =
    name ++ '#':haskellFun ++ 
    '|':map toTag argus ++ 
    ['|', fromMaybe 'V' (fmap toTag mbRes)]

  toTag x = 
    case x of
      ObjectTy{} -> 'O'
      StringTy   -> 'S'
      IntTy      -> 'I'
      VoidTy     -> 'V'

{-
  Function: msgBox
  
  Purpose:  Pops up a message box; no title.
-}
msgBox :: String -> String -> IO ()
msgBox caption msg = invokeStatic "System.Windows.Forms.MessageBox" "Show" (msg,caption)

class NetArg a where
  marshal   :: a -> IO [Object ()]
--  unmarshal :: [Object ()] -> (a -> IO b) -> IO b

instance NetArg () where
  marshal _ = return []
  
instance NetType a => NetArg a where
  marshal x = arg x >>= \ p -> return [p]

instance (NetArg a1, NetArg a2) => 
	 NetArg (a1,a2) where
  marshal (a1,a2) = do
     lss <- sequence [marshal a1, marshal a2]
     return (concat lss)

instance (NetArg a1, NetArg a2,NetArg a3) => 
	 NetArg (a1,a2,a3) where
  marshal (a1,a2,a3) = do
     lss <- sequence [marshal a1, marshal a2, marshal a3]
     return (concat lss)

instance (NetArg a1, NetArg a2,NetArg a3,
	  NetArg a4) => 
	 NetArg (a1,a2,a3,a4) where
  marshal (a1,a2,a3,a4) = do
     lss <- sequence [marshal a1, marshal a2, marshal a3, marshal a4]
     return (concat lss)

instance (NetArg a1, NetArg a2,NetArg a3,
	  NetArg a4, NetArg a5) => 
	 NetArg (a1,a2,a3,a4,a5) where
  marshal (a1,a2,a3,a4,a5) = do
     lss <- sequence [marshal a1, marshal a2, marshal a3, 
     		      marshal a4, marshal a5]
     return (concat lss)

instance (NetArg a1, NetArg a2, NetArg a3,
	  NetArg a4, NetArg a5, NetArg a6) => 
	 NetArg (a1,a2,a3,a4,a5,a6) where
  marshal (a1,a2,a3,a4,a5,a6) = do
     lss <- sequence [marshal a1, marshal a2, marshal a3,
     		      marshal a4, marshal a5, marshal a6]
     return (concat lss)

instance (NetArg a1, NetArg a2, NetArg a3,
	  NetArg a4, NetArg a5, NetArg a6,
	  NetArg a7) => 
	 NetArg (a1,a2,a3,a4,a5,a6,a7) where
  marshal (a1,a2,a3,a4,a5,a6,a7) = do
     lss <- sequence [marshal a1, marshal a2, marshal a3,
     	              marshal a4, marshal a5, marshal a6,
		      marshal a7]
     return (concat lss)

invoke :: (NetArg a, NetType res)
       => MethodName
       -> a
       -> Object b
       -> IO res
invoke methName args obj = do
  ls    <- marshal args
  args  <- mkArgs ls
  res   <- invokeMethod obj methName args
  result res

newObj :: (NetArg a)
       => ClassName
       -> a
       -> IO (Object res)
newObj clsName args = do
  ls    <- marshal args
  args  <- mkArgs ls
  primCreateObject clsName args

invokeStatic :: (NetArg a, NetType res)
	     => ClassName
	     -> MethodName
             -> a
             -> IO res
invokeStatic clsName methName args = do
  ls    <- marshal args
  args  <- mkArgs ls
  res   <- invokeStaticMethod (mkStaticMethod clsName methName) args
  result res

hsString :: Object a -> IO String
hsString x = toString x

hsValue :: NetType a => Object obj -> IO a
hsValue x = result (castObjTy x)

boxValue :: NetType a => a -> IO (Object b)
boxValue v = do
 r <- arg v
 return (castObjTy (r :: Object ()))

-- type unsafe.
mkVector :: BaseType -> Int -> IO (Object a)
mkVector eltTy sz = do
  case eltTy of
    ObjectTy{} -> newArgArray sz
    StringTy{} -> newArgArray sz
    VoidTy     -> ioError (userError "DotNet.mkVector: can't create a vector of Voids")
    x          -> mkPrimVector (toTag x) sz
 where
  toTag x =
    case x of
      ByteTy     -> 0    
      BooleanTy  -> 1
      CharTy     -> 2
      DoubleTy   -> 3
      Int16Ty    -> 4
      Int32Ty    -> 5
      Int64Ty    -> 6
      IntTy      -> 5
      SByteTy    -> 7
      SingleTy   -> 8
      UInt16Ty   -> 9
      UInt32Ty   -> 10
      UInt64Ty   -> 11
     

foreign import dotnet
  "static method System.Convert.ToInt32"
  mkInt32 :: Int32 -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToInt32"
  toInt32 :: Object a -> IO Int32
  
foreign import dotnet
  "static method System.Convert.ToInt32"
  mkInt :: Int -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToInt32"
  toInt_ :: Object a -> IO Int
  
foreign import dotnet
  "static method System.Convert.ToSByte"
  mkSByte :: Int8 -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToSByte"
  toSByte :: Object a -> IO Int8
  
foreign import dotnet
  "static method System.Convert.ToByte"
  mkByte :: Char -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToByte"
  toByte :: Object a -> IO Char
  
foreign import dotnet
  "static method System.Convert.ToUInt32"
  mkUInt32 :: Word32 -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToUInt32"
  toUInt32 :: Object a -> IO Word32
  
foreign import dotnet
  "static method System.Convert.ToBoolean"
  mkBool :: Bool -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToBoolean"
  toBool :: Object a -> IO Bool

foreign import dotnet
  "static method System.Convert.ToSingle"
  mkSingle :: Float -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToSingle"
  toSingle :: Object a -> IO Float

foreign import dotnet
  "static method System.Convert.ToDouble"
  mkDouble :: Double -> IO (Object ())

foreign import dotnet
  "static method System.Convert.ToDouble"
  toDouble :: Object a -> IO Double

primitive primCreateObject "createObject" :: String -> Object args -> IO (Object o)
primitive invokeMethod :: Object this -> String -> Object args -> IO (Object res)
primitive invokeStaticMethod ::String -> Object args -> IO (Object res)
primitive newArgArray  :: Int -> IO (Object arr)
primitive setArrayArg  :: Object a -> Object val -> Int -> IO ()
primitive getArrayArg  :: Object a -> Int -> IO (Object val)
primitive getField     :: Object a -> String -> IO (Object b)
primitive setField     :: Object a -> String -> Object b -> IO ()
primitive getStaticField :: String -> String -> IO (Object res)
primitive setStaticField :: String -> String -> Object o -> IO ()
primitive isNullPtr    :: Object o -> IO Bool

primitive mkPrimVector :: Int{-type tag-} -> Int{-sz-} -> IO (Object o)
-- tag values:
--   [ Byte=0, Boolean=1, Byte, Char, Double,
--     Int16, Int32, Int64, SByte, Single, UInt16,
--     UInt32, UInt64]
-- 
primitive newString :: String   -> IO (Object a)
primitive toString  :: Object a -> IO String


