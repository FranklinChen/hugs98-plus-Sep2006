--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.OpenAL.ALC.BasicTypes
-- Copyright   :  (c) Sven Panne 2003-2005
-- License     :  BSD-style (see the file libraries/OpenAL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to the introductory parts of chapter 6 (AL Contexts
-- and the ALC API) of the OpenAL Specification and Reference (version 1.1).
--
-- The context API makes use of ALC types which are defined separately from the
-- AL types - there is an 'ALCboolean', 'ALCchar', etc.
--
--------------------------------------------------------------------------------

module Sound.OpenAL.ALC.BasicTypes (
   ALCboolean, ALCchar, ALCbyte, ALCubyte, ALCshort, ALCushort, ALCint, ALCuint,
   ALCsizei, ALCenum, ALCfloat, ALCdouble
) where

import Sound.OpenAL.Config
