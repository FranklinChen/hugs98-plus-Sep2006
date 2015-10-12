-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.Constants
-- Copyright   :  (c) Sven Panne 2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This purely internal module defines all AL\/ALC constants, which have been
-- figured out by configure. In contrast to OpenGL and GLUT, these constants
-- varied on different platforms in the past and have evolved quite a bit.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.Constants where

import Sound.OpenAL.Config (
   ALboolean, ALint, ALenum, ALCboolean, ALCint, ALCenum )

--------------------------------------------------------------------------------

#include "HsOpenALConfig.h"

--------------------------------------------------------------------------------

al_FALSE, al_TRUE :: ALboolean
al_FALSE                            = CONST_AL_FALSE
al_TRUE                             = CONST_AL_TRUE

al_NO_ERROR, al_INVALID_NAME, al_INVALID_ENUM, al_INVALID_VALUE,
   al_INVALID_OPERATION, al_OUT_OF_MEMORY :: ALenum
al_NO_ERROR                         = CONST_AL_NO_ERROR
al_INVALID_NAME                     = CONST_AL_INVALID_NAME
al_INVALID_ENUM                     = CONST_AL_INVALID_ENUM
al_INVALID_VALUE                    = CONST_AL_INVALID_VALUE
al_INVALID_OPERATION                = CONST_AL_INVALID_OPERATION
al_OUT_OF_MEMORY                    = CONST_AL_OUT_OF_MEMORY

--------------------------------------------------------------------------------

al_DISTANCE_MODEL, al_DOPPLER_FACTOR, al_SPEED_OF_SOUND :: ALenum
al_DISTANCE_MODEL                   = CONST_AL_DISTANCE_MODEL
al_DOPPLER_FACTOR                   = CONST_AL_DOPPLER_FACTOR
al_SPEED_OF_SOUND                   = CONST_AL_SPEED_OF_SOUND

al_VERSION, al_RENDERER, al_VENDOR, al_EXTENSIONS :: ALenum
al_VERSION                          = CONST_AL_VERSION
al_RENDERER                         = CONST_AL_RENDERER
al_VENDOR                           = CONST_AL_VENDOR
al_EXTENSIONS                       = CONST_AL_EXTENSIONS

al_NONE, al_INVERSE_DISTANCE, al_INVERSE_DISTANCE_CLAMPED, al_LINEAR_DISTANCE,
   al_LINEAR_DISTANCE_CLAMPED, al_EXPONENT_DISTANCE,
   al_EXPONENT_DISTANCE_CLAMPED :: ALenum
al_NONE                             = CONST_AL_NONE
al_INVERSE_DISTANCE                 = CONST_AL_INVERSE_DISTANCE
al_INVERSE_DISTANCE_CLAMPED         = CONST_AL_INVERSE_DISTANCE_CLAMPED
al_LINEAR_DISTANCE                  = CONST_AL_LINEAR_DISTANCE
al_LINEAR_DISTANCE_CLAMPED          = CONST_AL_LINEAR_DISTANCE_CLAMPED
al_EXPONENT_DISTANCE                = CONST_AL_EXPONENT_DISTANCE
al_EXPONENT_DISTANCE_CLAMPED        = CONST_AL_EXPONENT_DISTANCE_CLAMPED

--------------------------------------------------------------------------------

al_POSITION, al_VELOCITY, al_GAIN :: ALenum
al_POSITION                         = CONST_AL_POSITION
al_VELOCITY                         = CONST_AL_VELOCITY
al_GAIN                             = CONST_AL_GAIN

al_ORIENTATION :: ALenum
al_ORIENTATION                      = CONST_AL_ORIENTATION

al_SOURCE_RELATIVE, al_SOURCE_TYPE, al_LOOPING, al_BUFFER, al_BUFFERS_QUEUED,
   al_BUFFERS_PROCESSED, al_MIN_GAIN, al_MAX_GAIN, al_REFERENCE_DISTANCE,
   al_ROLLOFF_FACTOR, al_MAX_DISTANCE, al_PITCH, al_DIRECTION,
   al_CONE_INNER_ANGLE, al_CONE_OUTER_ANGLE, al_CONE_OUTER_GAIN, al_SEC_OFFSET,
   al_SAMPLE_OFFSET, al_BYTE_OFFSET, al_SOURCE_STATE :: ALenum
al_SOURCE_RELATIVE                  = CONST_AL_SOURCE_RELATIVE
al_SOURCE_TYPE                      = CONST_AL_SOURCE_TYPE
al_LOOPING                          = CONST_AL_LOOPING
al_BUFFER                           = CONST_AL_BUFFER
al_BUFFERS_QUEUED                   = CONST_AL_BUFFERS_QUEUED
al_BUFFERS_PROCESSED                = CONST_AL_BUFFERS_PROCESSED
al_MIN_GAIN                         = CONST_AL_MIN_GAIN
al_MAX_GAIN                         = CONST_AL_MAX_GAIN
al_REFERENCE_DISTANCE               = CONST_AL_REFERENCE_DISTANCE
al_ROLLOFF_FACTOR                   = CONST_AL_ROLLOFF_FACTOR
al_MAX_DISTANCE                     = CONST_AL_MAX_DISTANCE
al_PITCH                            = CONST_AL_PITCH
al_DIRECTION                        = CONST_AL_DIRECTION
al_CONE_INNER_ANGLE                 = CONST_AL_CONE_INNER_ANGLE
al_CONE_OUTER_ANGLE                 = CONST_AL_CONE_OUTER_ANGLE
al_CONE_OUTER_GAIN                  = CONST_AL_CONE_OUTER_GAIN
al_SEC_OFFSET                       = CONST_AL_SEC_OFFSET
al_SAMPLE_OFFSET                    = CONST_AL_SAMPLE_OFFSET
al_BYTE_OFFSET                      = CONST_AL_BYTE_OFFSET
al_SOURCE_STATE                     = CONST_AL_SOURCE_STATE

al_UNDETERMINED, al_STATIC, al_STREAMING :: ALint
al_UNDETERMINED                     = CONST_AL_UNDETERMINED
al_STATIC                           = CONST_AL_STATIC
al_STREAMING                        = CONST_AL_STREAMING

al_INITIAL, al_PLAYING, al_PAUSED, al_STOPPED :: ALint
al_INITIAL                          = CONST_AL_INITIAL
al_PLAYING                          = CONST_AL_PLAYING
al_PAUSED                           = CONST_AL_PAUSED
al_STOPPED                          = CONST_AL_STOPPED

--------------------------------------------------------------------------------

al_FREQUENCY, al_SIZE, al_BITS, al_CHANNELS :: ALenum
al_FREQUENCY                        = CONST_AL_FREQUENCY
al_SIZE                             = CONST_AL_SIZE
al_BITS                             = CONST_AL_BITS
al_CHANNELS                         = CONST_AL_CHANNELS

al_FORMAT_MONO8, al_FORMAT_MONO16, al_FORMAT_STEREO8,
   al_FORMAT_STEREO16 :: ALenum
al_FORMAT_MONO8                     = CONST_AL_FORMAT_MONO8
al_FORMAT_MONO16                    = CONST_AL_FORMAT_MONO16
al_FORMAT_STEREO8                   = CONST_AL_FORMAT_STEREO8
al_FORMAT_STEREO16                  = CONST_AL_FORMAT_STEREO16

--------------------------------------------------------------------------------

alc_FALSE, alc_TRUE :: ALCboolean
alc_FALSE                           = CONST_ALC_FALSE
alc_TRUE                            = CONST_ALC_TRUE

alc_FREQUENCY, alc_REFRESH, alc_SYNC, alc_MONO_SOURCES,
   alc_STEREO_SOURCES :: ALCint
alc_FREQUENCY                       = CONST_ALC_FREQUENCY
alc_REFRESH                         = CONST_ALC_REFRESH
alc_SYNC                            = CONST_ALC_SYNC
alc_MONO_SOURCES                    = CONST_ALC_MONO_SOURCES
alc_STEREO_SOURCES                  = CONST_ALC_STEREO_SOURCES

alc_NO_ERROR, alc_INVALID_DEVICE, alc_INVALID_CONTEXT, alc_INVALID_ENUM,
   alc_INVALID_VALUE, alc_INVALID_OPERATION, alc_OUT_OF_MEMORY :: ALCenum
alc_NO_ERROR                        = CONST_ALC_NO_ERROR
alc_INVALID_DEVICE                  = CONST_ALC_INVALID_DEVICE
alc_INVALID_CONTEXT                 = CONST_ALC_INVALID_CONTEXT
alc_INVALID_ENUM                    = CONST_ALC_INVALID_ENUM
alc_INVALID_VALUE                   = CONST_ALC_INVALID_VALUE
alc_INVALID_OPERATION               = CONST_ALC_INVALID_OPERATION
alc_OUT_OF_MEMORY                   = CONST_ALC_OUT_OF_MEMORY

alc_DEFAULT_DEVICE_SPECIFIER, alc_DEVICE_SPECIFIER, alc_EXTENSIONS,
   alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER, alc_CAPTURE_DEVICE_SPECIFIER :: ALCenum
alc_DEFAULT_DEVICE_SPECIFIER        = CONST_ALC_DEFAULT_DEVICE_SPECIFIER
alc_DEVICE_SPECIFIER                = CONST_ALC_DEVICE_SPECIFIER
alc_EXTENSIONS                      = CONST_ALC_EXTENSIONS
alc_CAPTURE_DEFAULT_DEVICE_SPECIFIER= CONST_ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER
alc_CAPTURE_DEVICE_SPECIFIER        = CONST_ALC_CAPTURE_DEVICE_SPECIFIER

alc_ATTRIBUTES_SIZE, alc_ALL_ATTRIBUTES, alc_MAJOR_VERSION, alc_MINOR_VERSION,
   alc_CAPTURE_SAMPLES :: ALCenum
alc_ATTRIBUTES_SIZE                 = CONST_ALC_ATTRIBUTES_SIZE
alc_ALL_ATTRIBUTES                  = CONST_ALC_ALL_ATTRIBUTES
alc_MAJOR_VERSION                   = CONST_ALC_MAJOR_VERSION
alc_MINOR_VERSION                   = CONST_ALC_MINOR_VERSION
alc_CAPTURE_SAMPLES                 = CONST_ALC_CAPTURE_SAMPLES
