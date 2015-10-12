-----------------------------------------------------------------------------
-- Library of functions for writing interactive programs with screen-oriented
-- I/O (assumes Ansi screen).
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module AnsiInteract(
	module AnsiInteract,
	module Interact,
	module AnsiScreen
	) where

import AnsiScreen
import Interact

-- Screen oriented input/output functions:

clearScreen       :: Interact -> Interact
writeAt           :: Pos -> String -> Interact -> Interact
moveTo            :: Pos -> Interact -> Interact
readAt            :: Pos                  ->  -- Start coordinates
                     Int                  ->  -- Maximum input length
                     (String -> Interact) ->  -- How to use entered string
                     Interact
defReadAt         :: Pos                  ->  -- Start coordinates        
                     Int                  ->  -- Maximum input length     
                     String               ->  -- Default string value     
                     (String -> Interact) ->  -- How to use entered string
                     Interact
promptReadAt      :: Pos                  -> -- Start coordinates        
                     Int                  -> -- Maximum input length     
                     String               -> -- Prompt
                     (String -> Interact) -> -- How to use entered string
                     Interact
defPromptReadAt   :: Pos                  -> -- Start coordinates        
                     Int                  -> -- Maximum input length     
                     String               -> -- Prompt
                     String               -> -- Default string value
                     (String -> Interact) -> -- How to use entered string
                     Interact

clearScreen        = writeStr cls
writeAt (x,y) s    = writeStr (goto x y ++ s)
moveTo  (x,y)      = writeStr (goto x y)


readAt pt l use    = writeAt pt (replicate l '_') (moveTo pt (loop 0 ""))
 where loop n s    = readChar (return s) (\c ->
                     case c of '\BS'         -> delete n s
                               '\DEL'        -> delete n s
                               '\n'          -> return s
                               c | n < l     -> writeChar c (loop (n+1) (c:s))
                                 | otherwise -> ringBell (loop n s))
       delete n s  = if n>0 then writeStr "\BS_\BS" (loop (n-1) (tail s))
                            else ringBell (loop 0 "")
       return s    = use (reverse s)


defReadAt (x,y) l def use
                   = writeAt (x,y) (take l (def++repeat '_')) (
                     readChar (use def) (\c ->
                     if c=='\n' then use def
                                else unreadChar c (readAt (x,y) l use)))

promptReadAt (x,y) l prompt use
                   = writeAt (x,y) prompt (readAt (x+length prompt,y) l use)

defPromptReadAt (x,y) l prompt def use
                   = writeAt (x,y) prompt (
                     defReadAt (x+length prompt,y) l def use)

-----------------------------------------------------------------------------
