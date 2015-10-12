TerminalTest.lhs

> import Test.HUnit.Terminal
> import Test.HUnit

> main :: IO Counts
> main = runTestTT tests

> try :: String -> String -> String -> Test
> try lab inp exp' = lab ~: terminalAppearance inp ~?= exp'

> tests :: Test
> tests = test [
>   try "empty" "" "",
>   try "end in \\n" "abc\ndef\n" "abc\ndef\n",
>   try "not end in \\n" "abc\ndef" "abc\ndef",
>   try "return 1" "abc\ndefgh\rxyz" "abc\nxyzgh",
>   try "return 2" "\nabcdefgh\rijklm\rxy\n" "\nxyklmfgh\n",
>   try "return 3" "\r\rabc\r\rdef\r\r\r\nghi\r\r\n" "def\nghi\n",
>   try "back 1" "abc\bdef\b\bgh\b" "abdgh",
>   try "back 2" "abc\b\b\bdef\b\bxy\b\b\n" "dxy\n"
>   -- \b at beginning of line
>   -- nonprinting char
>  ]
