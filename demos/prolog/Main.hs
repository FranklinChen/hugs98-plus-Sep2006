-- Prolog interpreter top level module
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module Main where

import CombParse
import Prolog
import Interact
import Subst
import StackEngine
import List(nub)

--- Command structure and parsing:

data Command = Fact Clause | Query [Term] | Show | Error | Quit | NoChange

command :: Parser Command
command  = just (sptok "bye" `orelse` sptok "quit") `pam` (\quit->Quit)
               `orelse`
           just (okay NoChange)
               `orelse`
           just (sptok "??") `pam` (\show->Show)
               `orelse`
           just clause `pam` Fact
               `orelse`
           just (sptok "?-" `pseq` termlist) `pam` (\(q,ts)->Query ts)
               `orelse`
           okay Error

--- Main program read-solve-print loop:

signOn           :: String
signOn            = "Mini Prolog Version 1.5g (" ++ version ++ ")\n\n"

main             :: IO ()
main              = do putStr signOn
                       putStr ("Reading " ++ stdlib)
		       clauses <- readLibrary stdlib
                       interpreter clauses

readLibrary lib   = do is <- readFile lib
                       let parse   = map clause (lines is)
                           clauses = [ r | ((r,""):_) <- parse ]
                           reading = ['.'| c <- clauses] ++ "done\n"
                       putStr reading
		       return clauses
		    `catch` \err ->
                    do putStr "...not found\n"
                       return []

stdlib           :: String
stdlib            = "stdlib"

interpreter      :: [Clause] -> IO ()
interpreter lib   = do is <- getContents
                       putStr (loop startDb is)
                    where startDb = foldl addClause emptyDb lib

loop             :: Database -> String -> String
loop db           = readLine "> " (exec db . fst . head . command)

exec             :: Database -> Command -> String -> String
exec db (Fact r)  = loop (addClause db r)
exec db (Query q) = demonstrate db q
exec db Show      = writeStr (show db)                 (loop db)
exec db Error     = writeStr "I don't understand\n"    (loop db)
exec db Quit      = writeStr "Thank you and goodbye\n" end
exec db NoChange  = loop db

--- Handle printing of solutions etc...

solution      :: [Id] -> Subst -> [String]
solution vs s  = [ show (Var i) ++ " = " ++ show v
                                | (i,v) <- [ (i,s i) | i<-vs ], v /= Var i ]

demonstrate     :: Database -> [Term] -> Interact
demonstrate db q = printOut (map (solution vs) (prove db q))
 where vs               = (nub . concat . map varsIn) q
       printOut []      = writeStr "no.\n"     (loop db)
       printOut ([]:bs) = writeStr "yes.\n"    (loop db)
       printOut (b:bs)  = writeStr (doLines b) (nextReqd bs)
       doLines          = foldr1 (\xs ys -> xs ++ "\n" ++ ys)
       nextReqd bs      = writeStr " "
                          (readChar end
                           (\c-> if c==';' then writeStr ";\n" (printOut bs)
                                           else writeStr "\n"  (loop db)))

--- End of Main.hs
