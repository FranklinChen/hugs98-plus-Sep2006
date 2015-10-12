-- This program is a simple example of the kind of thing that you can do with
-- ANSI escape character sequences.  But, of course, it will only work on
-- machines that support those character sequences (xterms and PCs with
-- ansi.sys installed, for example).
--
-- Type `interact program' to run the program.

module AnsiDemo( program ) where
import AnsiInteract

writes  = writeStr . concat
program = writes [ cls,
                   at (17,5)  (highlight "Demonstration program"),
                   at (48,5)  "Version 1.0",
                   at (17,7)  "This program illustrates a simple approach",
                   at (17,8)  "to screen-based interactive programs using",
                   at (17,9)  "the Hugs functional programming system.",
                   at (17,11) "Please press any key to continue ..."
                 ]
          (pressAnyKey
          (promptReadAt (17,15) 18 "Please enter your name: " (\name ->
          (let reply = "Hello " ++ name ++ "!" in
           writeAt (40-(length reply`div` 2),18) reply
          (moveTo (1,23)
          (writeStr "I'm waiting...\n"
          (pressAnyKey
          end)))))))

