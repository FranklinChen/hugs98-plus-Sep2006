--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- A convenience module, combining the Haskell bindings for AL and ALC.
--
--------------------------------------------------------------------------------

module Sound.OpenAL (
   -- * A Brief History of OpenAL
   -- $ABriefHistoryOfOpenAL

   -- * What is the OpenAL Audio System?
   -- $WhatIsTheOpenALAudioSystem

   -- * Different Views of OpenAL

   -- ** Programmer\'s View of OpenAL
   -- $ProgrammersViewOfOpenAL

   -- ** Implementor\'s View of OpenAL
   -- $ImplementorsViewOfOpenAL

   -- ** The Specification\'s View of OpenAL
   -- $TheSpecificationsViewOfOpenAL

   -- * Legal stuff
   -- $LegalStuff

     module Sound.OpenAL.AL
   , module Sound.OpenAL.ALC

   -- * Convenience Re-exports from the OpenGL Package
   , module Graphics.Rendering.OpenGL.GL.StateVar
   , ObjectName(..), Vector3(..), Vertex3(..)
) where

import Sound.OpenAL.AL
import Sound.OpenAL.ALC

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Vector3(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex3(..) )

--------------------------------------------------------------------------------
-- $ABriefHistoryOfOpenAL
-- The first discussions about implementing OpenAL as an audio API complimentary
-- to OpenGL started around 1998. There were a few aborted attempts at creating
-- the headers and a specification, but by late 1999 Loki Entertainment Software
-- was in need for an API of exactly this type and pursued both a specification
-- and a Linux implementation. At around that time, Loki started talking with
-- Creative Labs about standardizing the API and expanding platform support. The
-- OpenAL 1.0 specification was released in early 2000 and compliant OpenAL
-- libraries were released in the same year for Linux, MacOS 8\/9, Windows, and
-- BeOS. Loki Entertainment also shipped several games using OpenAL in 2000:
-- Heavy Gear 2 and Heretic 2 (both under Linux). In 2001, Creative Labs
-- released the first hardware-accelerated OpenAL libraries. The libraries
-- supported the SoundBlaster Live on MacOS 8\/9 and Windows.
--
-- Since 2001, there has been continuous improvement in OpenAL. Some platforms
-- are less relevant than in 2000 (BeOS and MacOS 8\/9 for instance), but more
-- platforms have been added as well (BSD, Solaris, IRIX, Mac OS X, and the
-- popular console gaming platforms). Hardware support is enabled for many
-- Creative and NVIDIA audio devices under Windows as well.
--
-- In terms of product support, OpenAL has been used in a large number of titles
-- over the years, on many platforms (for a list of many of the titles, see
-- <http://www.openal.org/titles.html>).

--------------------------------------------------------------------------------
-- $WhatIsTheOpenALAudioSystem
-- OpenAL (for /Open Audio Library/) is a software interface to audio hardware.
-- The interface consists of a number of functions that allow a programmer to
-- specify the objects and operations in producing high-quality audio output,
-- specifically multichannel output of 3D arrangements of sound sources around a
-- listener.
--
-- The OpenAL API is designed to be cross-platform and easy to use. It resembles
-- the OpenGL API in coding style and conventions. OpenAL uses a syntax
-- resembling that of OpenGL where applicable. For more information on OpenGL,
-- see <http://www.opengl.org/> and the "Graphics.Rendering.OpenGL" module.
--
-- OpenAL is foremost a means to generate audio in a simulated three-dimensional
-- space.  Consequently, legacy audio concepts such as panning and left\/right
-- channels are not directly supported. OpenAL does include extensions
-- compatible with the IA-SIG 3D Level 1 and Level 2 rendering guidelines to
-- handle sound-source directivity and distancerelated attenuation and Doppler
-- effects, as well as environmental effects such as reflection, obstruction,
-- transmission, reverberation. For more information on IA-SIG 3D, see
-- <http://www.iasig.org/wg/closed/3dwg/3dwg.shtml>.
--
-- Like OpenGL, the OpenAL core API has no notion of an explicit rendering
-- context, and operates on an implied current OpenAL Context. Unlike the OpenGL
-- specification, the OpenAL specification includes both the core API (the
-- actual OpenAL API, see "Sound.OpenAL.AL") and the operating system bindings
-- of the ALC API (the /Audio Library Context/, see "Sound.OpenAL.ALC"). Unlike
-- OpenGL\'s GLX, WGL and other OS-specific bindings, the ALC API is portable
-- across platforms as well.

--------------------------------------------------------------------------------
-- $ProgrammersViewOfOpenAL
-- To the programmer, OpenAL is a set of commands that allow the specification
-- of sound sources and a listener in three dimensions, combined with commands
-- that control how these sound sources are rendered into the output buffer. The
-- effect of OpenAL commands is not guaranteed to be immediate, as there are
-- latencies depending on the implementation, but ideally such latency should
-- not be noticeable to the user.
--
-- A typical program that uses OpenAL begins with calls to open a sound device
-- which is used to process output and play it on attached hardware (speakers or
-- headphones). Then, calls are made to allocate an AL context and associate it
-- with the device. Once an AL context is allocated, the programmer is free to
-- issue AL commands. Some calls are used to render sources (point and
-- directional sources, looping or not), while others affect the rendering of
-- these sources including how they are attenuated by distance and relative
-- orientation.

--------------------------------------------------------------------------------
-- $ImplementorsViewOfOpenAL
-- To the implementor, OpenAL is a set of commands that affect the operation of
-- CPU and sound hardware. If the hardware consists only of an addressable
-- output buffer, then OpenAL must be implemented almost entirely on the host
-- CPU. In some cases audio hardware provides DSP-based and other acceleration
-- in various degrees. The OpenAL implementor\'s task is to provide the CPU
-- software interface while dividing the work for each AL command between the
-- CPU and the audio hardware. This division should be tailored to the available
-- audio hardware to obtain optimum performance in carrying out AL calls.
--
-- OpenAL maintains a considerable amount of state information. This state
-- controls how the sources are rendered into the output buffer. Some of this
-- state is directly available to the user: he or she can make calls to obtain
-- its value. Some of it, however, is visible only by the effect it has on what
-- is rendered. One of the main goals of the OpenAL specification is to make
-- OpenAL state information explicit, to elucidate how it changes, and to
-- indicate what its effects are.

--------------------------------------------------------------------------------
-- $TheSpecificationsViewOfOpenAL
-- The OpenAL specification (see <http://www.openal.org/documentation.html>)
-- views OpenAL as a state machine that controls a multichannel processing
-- system to synthesize a digital stream, passing sample data through a chain of
-- parametrized digital audio signal processing operations. This model should
-- engender a specification that satisfies the needs of both programmers and
-- implementors. It does not, however, necessarily provide a model for
-- implementation. Any proper implementation must produce results conforming to
-- those produced by the methods specified in the OpenAL specification, but
-- there may be ways to carry out a particular computation that are more
-- efficient than the one specified.

--------------------------------------------------------------------------------
-- $LegalStuff
-- The documentation is more or less based upon the OpenAL 1.1 Specification and
-- Reference, which is in turn based upon the older OpenAL Specification and
-- Reference (1.0), published in June 2000. Both copyright notices are presented
-- below:
--
-- Version 1.1: Published June 2005, Copyright (c) 2005 by authors
--
-- Version 1.0 Draft Edition: Published June 2000, Copyright (c) 1999-2000 by
-- Loki Software
--
-- Permission is granted to make and distribute verbatim copies of this manual
-- provided the copyright notice and this permission notice are preserved on all
-- copies.  Permission is granted to copy and distribute translations of this
-- manual into another language, under the above conditions for modified
-- versions, except that this permission notice may be stated in a translation
-- approved by the copyright owners.
--
-- BeOS is a trademark of PalmSource, Inc. Linux is a trademark of Linus
-- Torvalds. Macintosh and Apple are trademarks of Apple Computer, Inc. OpenAL
-- is a trademark of Creative Labs, Inc. OpenGL is a trademark of Silicon
-- Graphics, Inc. UNIX is a trademark of X\/Open Group. Windows is a trademark
-- of Microsoft Corp. X Window System is a trademark of X Consortium, Inc. All
-- other trademarks are property of their respective owners.
