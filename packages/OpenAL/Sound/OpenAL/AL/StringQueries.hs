--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.AL.StringQueries
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.1.2 (String Queries) of the OpenAL
-- Specification and Reference (version 1.1).
--
--------------------------------------------------------------------------------

module Sound.OpenAL.AL.StringQueries (
   alVendor, alRenderer
) where

import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Sound.OpenAL.AL.QueryUtils ( StringName(..), getString )

--------------------------------------------------------------------------------

-- | Contains the name of the vendor.

alVendor :: GettableStateVar String
alVendor = makeGettableStateVar (getString Vendor)

-- | Contains information about the specific renderer.

alRenderer :: GettableStateVar String
alRenderer = makeGettableStateVar (getString Renderer)
