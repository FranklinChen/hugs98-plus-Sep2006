--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.Context
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 6.2 (Managing Rendering Contexts) of the
-- OpenAL Specification and Reference (version 1.1).
--
-- All operations of the AL core API affect a current AL context. Within the
-- scope of AL, the ALC is implied - it is not visible as a handle or function
-- parameter. Only one AL Context per process can be current at a time.
-- Applications maintaining multiple AL Contexts, whether threaded or not,
-- have to set the current context accordingly. Applications can have multiple
-- threads that share one more or contexts. In other words, AL and ALC are
-- threadsafe.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.Context (
   Frequency, ContextAttribute(..), Context, createContext, currentContext,
   processContext, suspendContext, destroyContext, contextsDevice, allAttributes
) where


import Foreign.Marshal.Array ( withArray0 )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.StateVar (
   StateVar, makeStateVar, GettableStateVar, makeGettableStateVar )
import Sound.OpenAL.ALC.BasicTypes ( ALCint )
import Sound.OpenAL.ALC.ALCboolean ( marshalALCboolean, unmarshalALCboolean )
import Sound.OpenAL.ALC.Device ( Device )
import Sound.OpenAL.ALC.QueryUtils ( IntQuery(..), getInteger, getIntegerv )
import Sound.OpenAL.Config (
   ALCdevice, marshalDevice, unmarshalDevice,
   ALCcontext, Context, nullContext, marshalContext, unmarshalContext,
   alcProcessContext, alcDestroyContext, alcMakeContextCurrent )
import Sound.OpenAL.Constants (
   alc_FREQUENCY, alc_REFRESH, alc_SYNC, alc_MONO_SOURCES, alc_STEREO_SOURCES )

#ifdef __HADDOCK__
import Sound.OpenAL.ALC.Errors ( ALCErrorCategory(..) )
#endif

--------------------------------------------------------------------------------

-- | Frequency, specified in samples per second, i.e. units of Hertz \[Hz\].
-- Note that the underlying OpenAL API currently uses integral frequencies
-- only, but we want to mirror physical reality here more closely.

type Frequency = Float

--------------------------------------------------------------------------------

-- | The application can choose to specify certain attributes for a context at
-- context-creation time. Attributes not specified explicitly are set to
-- implementation dependent defaults.

data ContextAttribute =
     Frequency Frequency   -- ^ Frequency for mixing output buffer, in units of
                           --   Hz
   | Refresh Frequency     -- ^ Refresh intervals, in units of Hz
   | Sync Bool             -- ^ Flag, indicating a synchronous context
   | MonoSources Int       -- ^ A hint indicating how many sources should be
                           --   capable of supporting mono data
   | StereoSources Int     -- ^ A hint indicating how many sources should be
                           --   capable of supporting stereo data
   deriving ( Eq, Ord, Show )

marshalContextAttribute :: ContextAttribute -> (ALCint,ALCint)
marshalContextAttribute a = case a of
   Frequency f -> (alc_FREQUENCY, round f)
   Refresh r -> (alc_REFRESH, round r)
   Sync s -> (alc_SYNC, fromIntegral (marshalALCboolean s))
   MonoSources m -> (alc_MONO_SOURCES, fromIntegral m)
   StereoSources s -> (alc_STEREO_SOURCES, fromIntegral s)

unmarshalContextAttribute :: (ALCint,ALCint) -> ContextAttribute
unmarshalContextAttribute a@(x,y)
   | x == alc_FREQUENCY = Frequency (fromIntegral y)
   | x == alc_REFRESH = Refresh (fromIntegral y)
   | x == alc_SYNC = Sync (unmarshalALCboolean (fromIntegral y))
   | x == alc_MONO_SOURCES = MonoSources (fromIntegral y)
   | x == alc_STEREO_SOURCES = StereoSources (fromIntegral y)
   | otherwise = error ("unmarshalContextAttribute: illegal value " ++ show a)

--------------------------------------------------------------------------------

-- | Create a context for a given device and given attributes. Context creation
-- will fail in the following cases: a) if the application requests attributes
-- that, by themselves, can not be provided b) if the combination of specified
-- attributes can not be provided c) if a specified attribute, or the
-- combination of attributes, does not match the default values for unspecified
-- attributes If context creation fails, 'Nothing' will be returned, otherwise
-- 'Just' the new context. Note that 'createContext' does /not/ set the current
-- context, this must be done separately via 'currentContext'.

createContext :: Device -> [ContextAttribute] -> IO (Maybe Context)
createContext device attributes = do
   let pairToList (key, value) = [key, value]
       attrs = concatMap (pairToList . marshalContextAttribute) attributes
   fmap unmarshalContext .
      withArray0 0 attrs . alcCreateContext . marshalDevice $ device

foreign import CALLCONV unsafe "alcCreateContext"
   alcCreateContext :: ALCdevice -> Ptr ALCint -> IO ALCcontext

--------------------------------------------------------------------------------

-- | Contains 'Just' the current context with respect to OpenAL operation, or
-- 'Nothing' if there is no current context. Setting it to the latter is useful
-- when shutting OpenAL down. The state variable applies to the device that the
-- context was created for. For each OS process (usually this means for each
-- application), only one context can be current at any given time. All AL
-- commands apply to the current context. Commands that affect objects shared
-- among contexts (e.g. buffers) have side effects on other contexts.

currentContext :: StateVar (Maybe Context)
currentContext = makeStateVar getCurrentContext makeContextCurrent

getCurrentContext :: IO (Maybe Context)
getCurrentContext = fmap unmarshalContext $ alcGetCurrentContext

foreign import CALLCONV unsafe "alcGetCurrentContext"
   alcGetCurrentContext :: IO ALCcontext

makeContextCurrent :: Maybe Context -> IO ()
makeContextCurrent =
   fmap (const ()) . alcMakeContextCurrent . marshalContext . maybe nullContext id

--------------------------------------------------------------------------------

-- | The current context is the only context accessible to state changes by AL
-- commands (aside from state changes affecting shared objects). However,
-- multiple contexts can be processed at the same time. To indicate that a
-- context should be processed (i.e. that internal execution state like offset
-- increments are supposed to be performed), the application has to use
-- 'processContext'. Repeated calls to 'processContext' are legal, and do not
-- affect a context that is already marked as processing. The default state of a
-- context created by 'createContext' is that it is processing.

processContext :: Context -> IO ()
processContext = fmap (const ()) . alcProcessContext . marshalContext

-- | The application can suspend any context from processing (including the
-- current one). To indicate that a context should be suspended from processing
-- (i.e. that internal execution state like offset increments is not supposed to
-- be changed), the application has to use 'suspendContext'. Repeated calls to
-- 'suspendContext' are legal, and do not affect a context that is already
-- marked as suspended.

suspendContext :: Context -> IO ()
suspendContext = alcSuspendContext . marshalContext

foreign import CALLCONV unsafe "alcSuspendContext"
   alcSuspendContext :: ALCcontext -> IO ()

--------------------------------------------------------------------------------

-- | Destroy the given context. Note that the the correct way to destroy a
-- context is to first release it by setting 'currentContext' to
-- 'Nothing'. Applications should not attempt to destroy a current context,
-- doing so will not work and will result in an 'ALCInvalidOperation' error.

destroyContext :: Context -> IO ()
destroyContext = fmap (const ()) . alcDestroyContext . marshalContext

--------------------------------------------------------------------------------

-- | Contains 'Just' the device of the given context or 'Nothing' if the context
-- is invalid.

contextsDevice :: Context -> GettableStateVar (Maybe Device)
contextsDevice =
   makeGettableStateVar .
      fmap unmarshalDevice . alcGetContextsDevice . marshalContext

foreign import CALLCONV unsafe "alcGetContextsDevice"
   alcGetContextsDevice :: ALCcontext -> IO ALCdevice

--------------------------------------------------------------------------------

-- | Contains the attribute list for the current context of the specified
-- device.

allAttributes :: Device -> GettableStateVar [ContextAttribute]
allAttributes device = makeGettableStateVar $ do
   numALCints <- fmap fromIntegral $ getInteger (Just device) AttributesSize
   fmap toContextAttributes $ getIntegerv (Just device) AllAttributes numALCints

toContextAttributes :: [ALCint] -> [ContextAttribute]
toContextAttributes xs = case xs of
   [] -> []  -- should only happen when device and/or current context is invalid
   (0:_) -> []
   (x:y:rest) -> unmarshalContextAttribute (x,y) : toContextAttributes rest
   _ -> error ("toContextAttributes: illegal value " ++ show xs)
