-----------------------------------------------------------------------------
-- Dummy module to import all of the Hugs libraries; programmers should
-- normally be more selective than this when it comes to specifying the
-- modules that a particular program depends on.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module HugsLibs where

import StdLibs
import Trace
import Number
import ParseLib
import Interact
import AnsiScreen
import AnsiInteract
import IOExtensions
import ListUtils
import Dynamic

-----------------------------------------------------------------------------
