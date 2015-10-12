--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to chapters 2 (OpenAL Operation), 3 (State and State
-- Requests), 4 (Listener and Sources) and 5 (Buffers) of the OpenAL
-- Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL (
   -- * OpenAL Fundamentals
   -- $OpenALFundamentals

   -- * Basic AL Operation
   -- $BasicALOperation

   -- * Time and Frequency
   -- $TimeAndFrequency

   -- * Space and Distance
   -- $SpaceAndDistance

   -- * Coordinate System
   -- $CoordinateSystem

   module Sound.OpenAL.AL.BasicTypes,
   module Sound.OpenAL.AL.Errors,
   module Sound.OpenAL.AL.StringQueries,
   module Sound.OpenAL.AL.Attenuation,
   module Sound.OpenAL.AL.Doppler,
   module Sound.OpenAL.AL.Listener,
   module Sound.OpenAL.AL.Source,
   module Sound.OpenAL.AL.Buffer,
   module Sound.OpenAL.AL.Extensions
) where

import Sound.OpenAL.AL.BasicTypes
import Sound.OpenAL.AL.Errors
import Sound.OpenAL.AL.StringQueries
import Sound.OpenAL.AL.Attenuation
import Sound.OpenAL.AL.Doppler
import Sound.OpenAL.AL.Listener
import Sound.OpenAL.AL.Source
import Sound.OpenAL.AL.Buffer
import Sound.OpenAL.AL.Extensions

--------------------------------------------------------------------------------
-- $OpenALFundamentals
-- OpenAL is concerned with rendering audio into an output buffer and collecting
-- audio data from an input buffer. OpenAL\'s primary use is assumed to be for
-- spatialized audio. There is no support for MIDI.
--
-- OpenAL has three fundamental primitives or objects: 'Buffer's, 'Source's, and
-- a single listener (see "Sound.OpenAL.AL.Listener"). Each object can be
-- changed independently; the setting of one object does not affect the setting
-- of others. The application can also set modes that affect processing. Modes
-- are set, objects specified, and other OpenAL operations performed by sending
-- commands in the form of function or procedure calls.
--
-- Sources store locations, directions, and other attributes of an object in 3D
-- space and have a buffer associated with them for playback. When the program
-- wants to play a sound, it controls execution through a source object. Sources
-- are processed independently from each other.
--
-- Buffers store compressed or uncompressed audio data. It is common to
-- initialize a large set of buffers when the program first starts (or at
-- non-critical times during execution, between levels in a game, for
-- instance). Buffers are referred to by sources. Data (audio sample data) is
-- associated with buffers.
--
-- There is only one listener (per audio context). The listener attributes are
-- similar to source attributes, but are used to represent where the user is
-- hearing the audio from. The influence of all the sources from the perspective
-- of the listener is mixed and played for the user.

--------------------------------------------------------------------------------
-- $BasicALOperation
-- OpenAL can be used for a variety of audio playback tasks, and is an excellent
-- complement to OpenGL for real-time rendering, see
-- "Graphics.Rendering.OpenGL". A programmer who is familiar with OpenGL will
-- immediately notice the similarities between the two APIs in that they
-- describe their 3D environments using similar methods.
-- 
-- For an OpenGL\/OpenAL program, most of the audio programming will be in two
-- places in the code: initialization of the program, and the rendering loop. An
-- OpenGL\/OpenAL program will typically contain a section where the graphics
-- and audio systems are initialized, although it may be spread into multiple
-- functions. For OpenAL, initialization normally consists of creating a
-- context, creating the initial set of buffers, loading the buffers with sample
-- data, creating sources, attaching buffers to sources, setting locations and
-- directions for the listener and sources, and setting the initial values for
-- state global to OpenAL.

--------------------------------------------------------------------------------
-- $TimeAndFrequency
-- By default, OpenAL uses seconds and Hertz as units for time and frequency,
-- respectively. A float or integral value of one for a variable that specifies
-- quantities like duration, latency, delay, or any other parameter measured as
-- time, specifies 1 second. For frequency, the basic unit is 1\/second, or
-- Hertz. In other words, sample frequencies and frequency cut-offs or filter
-- parameters specifying frequencies are expressed in units of Hertz.

--------------------------------------------------------------------------------
-- $SpaceAndDistance
-- OpenAL does not define the units of measurement for distances. The
-- application is free to use meters, inches, or parsecs. OpenAL provides means
-- for simulating the natural attenuation of sound according to distance, and to
-- exaggerate or reduce this effect.  However, the resulting effects do not
-- depend on the distance unit used by the application to express source and
-- listener coordinates. OpenAL calculations are scale invariant.  The
-- specification assumes Euclidean calculation of distances, and mandates that
-- if two sources are sorted with respect to the Euclidean metric, the distance
-- calculation used by the implementation has to preserve that order.

--------------------------------------------------------------------------------
-- $CoordinateSystem
-- OpenAL - like OpenGL - uses a right-handed Cartesian coordinate system (RHS),
-- where in a frontal default view X (thumb) points right, Y (index finger)
-- points up, and Z (middle finger) points towards the viewer\/camera. To switch
-- from a left handed coordinate system (LHS) to a right handed coordinate
-- systems, flip the sign on the Z coordinate.
