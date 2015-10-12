module Dotnet.System.Math where

import Dotnet
import qualified Dotnet.System.Object
import qualified Dotnet.System.Decimal
import qualified Data.Int
import qualified Data.Word

data Math_ a
type Math a = Dotnet.System.Object.Object (Math_ a)

foreign import dotnet
  "static method System.Math.Acos"
  acos :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Asin"
  asin :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Atan"
  atan :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Atan2"
  atan2 :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Cos"
  cos :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Sin"
  sin :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Tan"
  tan :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Cosh"
  cosh :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Sinh"
  sinh :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Tanh"
  tanh :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Round"
  round :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Round"
  round_1 :: Double -> Int -> IO (Double)

foreign import dotnet
  "static method System.Math.Round"
  round_2 :: Dotnet.System.Decimal.Decimal a0 -> IO (Dotnet.System.Decimal.Decimal a1)

foreign import dotnet
  "static method System.Math.Round"
  round_3 :: Dotnet.System.Decimal.Decimal a0 -> Int -> IO (Dotnet.System.Decimal.Decimal a2)

foreign import dotnet
  "static method System.Math.Ceiling"
  ceiling :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Floor"
  floor :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Sqrt"
  sqrt :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Log"
  log :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Log10"
  log10 :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Exp"
  exp :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Pow"
  pow :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.IEEERemainder"
  iEEERemainder :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Abs"
  abs :: Data.Int.Int8 -> IO (Data.Int.Int8)

foreign import dotnet
  "static method System.Math.Abs"
  abs_1 :: Data.Int.Int16 -> IO (Data.Int.Int16)

foreign import dotnet
  "static method System.Math.Abs"
  abs_2 :: Int -> IO (Int)

foreign import dotnet
  "static method System.Math.Abs"
  abs_3 :: Data.Int.Int64 -> IO (Data.Int.Int64)

foreign import dotnet
  "static method System.Math.Abs"
  abs_4 :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Abs"
  abs_5 :: Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Abs"
  abs_6 :: Dotnet.System.Decimal.Decimal a0 -> IO (Dotnet.System.Decimal.Decimal a1)

foreign import dotnet
  "static method System.Math.Max"
  max :: Data.Int.Int8 -> Data.Int.Int8 -> IO (Data.Int.Int8)

foreign import dotnet
  "static method System.Math.Max"
  max_1 :: Data.Word.Word8 -> Data.Word.Word8 -> IO (Data.Word.Word8)

foreign import dotnet
  "static method System.Math.Max"
  max_2 :: Data.Int.Int16 -> Data.Int.Int16 -> IO (Data.Int.Int16)

foreign import dotnet
  "static method System.Math.Max"
  max_3 :: Data.Word.Word16 -> Data.Word.Word16 -> IO (Data.Word.Word16)

foreign import dotnet
  "static method System.Math.Max"
  max_4 :: Int -> Int -> IO (Int)

foreign import dotnet
  "static method System.Math.Max"
  max_5 :: Data.Word.Word32 -> Data.Word.Word32 -> IO (Data.Word.Word32)

foreign import dotnet
  "static method System.Math.Max"
  max_6 :: Data.Int.Int64 -> Data.Int.Int64 -> IO (Data.Int.Int64)

foreign import dotnet
  "static method System.Math.Max"
  max_7 :: Data.Word.Word64 -> Data.Word.Word64 -> IO (Data.Word.Word64)

foreign import dotnet
  "static method System.Math.Max"
  max_8 :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Max"
  max_9 :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Max"
  max_10 :: Dotnet.System.Decimal.Decimal a0 -> Dotnet.System.Decimal.Decimal a1 -> IO (Dotnet.System.Decimal.Decimal a2)

foreign import dotnet
  "static method System.Math.Min"
  min :: Data.Int.Int8 -> Data.Int.Int8 -> IO (Data.Int.Int8)

foreign import dotnet
  "static method System.Math.Min"
  min_1 :: Data.Word.Word8 -> Data.Word.Word8 -> IO (Data.Word.Word8)

foreign import dotnet
  "static method System.Math.Min"
  min_2 :: Data.Int.Int16 -> Data.Int.Int16 -> IO (Data.Int.Int16)

foreign import dotnet
  "static method System.Math.Min"
  min_3 :: Data.Word.Word16 -> Data.Word.Word16 -> IO (Data.Word.Word16)

foreign import dotnet
  "static method System.Math.Min"
  min_4 :: Int -> Int -> IO (Int)

foreign import dotnet
  "static method System.Math.Min"
  min_5 :: Data.Word.Word32 -> Data.Word.Word32 -> IO (Data.Word.Word32)

foreign import dotnet
  "static method System.Math.Min"
  min_6 :: Data.Int.Int64 -> Data.Int.Int64 -> IO (Data.Int.Int64)

foreign import dotnet
  "static method System.Math.Min"
  min_7 :: Data.Word.Word64 -> Data.Word.Word64 -> IO (Data.Word.Word64)

foreign import dotnet
  "static method System.Math.Min"
  min_8 :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Min"
  min_9 :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Min"
  min_10 :: Dotnet.System.Decimal.Decimal a0 -> Dotnet.System.Decimal.Decimal a1 -> IO (Dotnet.System.Decimal.Decimal a2)

foreign import dotnet
  "static method System.Math.Log"
  log_1 :: Double -> Double -> IO (Double)

foreign import dotnet
  "static method System.Math.Sign"
  sign :: Data.Int.Int8 -> IO (Int)

foreign import dotnet
  "static method System.Math.Sign"
  sign_1 :: Data.Int.Int16 -> IO (Int)

foreign import dotnet
  "static method System.Math.Sign"
  sign_2 :: Int -> IO (Int)

foreign import dotnet
  "static method System.Math.Sign"
  sign_3 :: Data.Int.Int64 -> IO (Int)

foreign import dotnet
  "static method System.Math.Sign"
  sign_4 :: Double -> IO (Int)

foreign import dotnet
  "static method System.Math.Sign"
  sign_5 :: Double -> IO (Int)

foreign import dotnet
  "static method System.Math.Sign"
  sign_6 :: Dotnet.System.Decimal.Decimal a0 -> IO (Int)

foreign import dotnet
  "static field System.Math.PI"
  get_PI :: IO (Double)

foreign import dotnet
  "static field System.Math.PI"
  set_PI :: Double -> IO ()

foreign import dotnet
  "static field System.Math.E"
  get_E :: IO (Double)

foreign import dotnet
  "static field System.Math.E"
  set_E :: Double -> IO ()


