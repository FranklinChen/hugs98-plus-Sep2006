-- This is a cut-down version of GHC's Exception module
--
-- The main difference is that Hugs does not throw asynchronous
-- exceptions, in particular heap and stack overflow and ctrl-C.
-- Indeed, it is not entirely clear what to do in response to ctrl-C.

module Hugs.Exception(
        Exception(..),
	IOException(..),
	ArithException(..),
	ArrayException(..),
	AsyncException(..),

	catchException,		-- :: IO a -> (Exception -> IO a) -> IO a

	-- Throwing exceptions

	throwIO,		-- :: Exception -> IO a
	throw,			-- :: Exception -> a

	evaluate,		-- :: a -> IO a

	-- Async exception control

        block,			-- :: IO a -> IO a
        unblock,		-- :: IO a -> IO a
  ) where

import Hugs.Prelude

instance Eq Exception where
  ArithException e1   == ArithException e2   = e1 == e2
  ArrayException e1   == ArrayException e2   = e1 == e2
  AssertionFailed e1  == AssertionFailed e2  = e1 == e2
  AsyncException e1   == AsyncException e2   = e1 == e2
  BlockedOnDeadMVar   == BlockedOnDeadMVar   = True
  Deadlock            == Deadlock            = True
  DynException _      == DynException _      = False -- incomparable
  ErrorCall e1        == ErrorCall e2        = e1 == e2
  ExitException e1    == ExitException e2    = e1 == e2
  IOException e1      == IOException e2      = e1 == e2
  NoMethodError e1    == NoMethodError e2    = e1 == e2
  NonTermination      == NonTermination      = True
  PatternMatchFail e1 == PatternMatchFail e2 = e1 == e2
  RecConError e1      == RecConError e2      = e1 == e2
  RecSelError e1      == RecSelError e2      = e1 == e2
  RecUpdError e1      == RecUpdError e2      = e1 == e2
  _                   == _                   = False

----------------------------------------------------------------
-- Primitive throw and catch
----------------------------------------------------------------

throwIO :: Exception -> IO a
throwIO exn = IO (\ s -> throw exn)

evaluate :: a -> IO a
evaluate x = IO (\ s -> x `seq` s x)

----------------------------------------------------------------
-- dummy implementations of block and unblock
----------------------------------------------------------------

block, unblock :: IO a -> IO a
block   m = m
unblock m = m

----------------------------------------------------------------
-- End
----------------------------------------------------------------
