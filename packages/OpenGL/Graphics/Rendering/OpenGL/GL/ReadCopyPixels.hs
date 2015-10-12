--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ReadCopyPixels
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 4.3 (Drawing, Reading, and Copying Pixels)
-- of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ReadCopyPixels (
   -- * Reading Pixels
   readPixels, readBuffer,

   -- * Copying Pixels
   PixelCopyType(..), copyPixels
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLint, GLsizei )
import Graphics.Rendering.OpenGL.GL.BufferMode (
   marshalBufferMode, unmarshalBufferMode )
import Graphics.Rendering.OpenGL.GL.Framebuffer ( BufferMode(..) )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.PixelData ( withPixelData )
import Graphics.Rendering.OpenGL.GL.PixelRectangles ( PixelData )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetReadBuffer), getEnum1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal ( recordInvalidValue )

--------------------------------------------------------------------------------

readPixels :: Position -> Size -> PixelData a -> IO ()
readPixels (Position x y) (Size w h) pd =
   withPixelData pd $ glReadPixels x y w h

foreign import CALLCONV unsafe "glReadPixels" glReadPixels ::
   GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

readBuffer :: StateVar BufferMode
readBuffer =
   makeStateVar
      (getEnum1 unmarshalBufferMode GetReadBuffer)
      (maybe recordInvalidValue glReadBuffer . marshalBufferMode)

foreign import CALLCONV unsafe "glReadBuffer" glReadBuffer :: GLenum -> IO ()

--------------------------------------------------------------------------------

data PixelCopyType =
     CopyColor
   | CopyDepth
   | CopyStencil
   deriving ( Eq, Ord, Show )

marshalPixelCopyType :: PixelCopyType -> GLenum
marshalPixelCopyType x = case x of
   CopyColor -> 0x1800
   CopyDepth -> 0x1801
   CopyStencil -> 0x1802

--------------------------------------------------------------------------------

copyPixels :: Position -> Size -> PixelCopyType -> IO ()
copyPixels (Position x y) (Size w h) t =
   glCopyPixels x y w h (marshalPixelCopyType t)

foreign import CALLCONV unsafe "glCopyPixels" glCopyPixels ::
   GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> IO ()
