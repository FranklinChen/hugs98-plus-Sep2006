-----------------------------------------------------------------------------
-- Mersenne.hs                                                 Mark P. Jones
--                                                          February 7, 1995
--
-- Here is a Hugs program to calculate the 30th Mersenne prime using the
-- builtin bignum arithmetic.
--
-- For those who don't know, a Mersenne prime is a prime number of the form:
--
--                               n
--                              2  - 1
--
-- The first few Mersenne primes are for:
--   n = 2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279, 2203,
--       2281, 3217, 4253, 4423, ...
--
-- The thirtieth Mersenne prime occurs for n = 216,091.  In decimal
-- notation, this number has 65050 digits.
--
--
-- A little story about me and this number:
--
-- As I recall, this fact was discovered nearly ten years ago.  I
-- wrote an Intel 8080 assembly language program to calculate this
-- number.  Running on a Z80A based machine, it used a 32K array --
-- more than half of the total memory available -- with each byte
-- containing two binary coded decimal digits.   The array was
-- initialized to contain the number 1 and a loop was used to double
-- the value in the array a total of 216091 times, before the final 1
-- was subtracted.  Using the timings for individual Z80A
-- instructions, I estimated the running time for the program and,
-- when it finished on Thursday April 17, 1986, after running for a
-- little under 18 hours, I was delighted that my predictions were
-- within 10 seconds of the actual running time.  Of course, now I
-- understand a little more about error bounds and tolerances, I realize
-- that this was more by luck than judgement, but at the time, I was
-- delighted!  I don't remember if I knew the O(log n) algorithm for
-- exponentials at the time, but it wouldn't have been easy to apply
-- with the limited amount of memory at my disposal back then.  (Of
-- course, it wouldn't have been O(log n) in practice either because
-- the individual multiplications can hardly be considered O(1)!)
--
-- Now I can run this program, written in Hugs (or to be accurate,
-- written using calls to Hugs primitive functions), on the machine
-- on my desk while I'm editing files and reading mail in other
-- windows, and it still finishes in under 7 minutes.  Of course,
-- it did use 6M of heap (though not all at the same time), but
-- who's counting?  :-)

module Mersenne where
import List( genericLength )

p         :: Integer
p          = 2 ^ 216091 - 1

digitsInP :: Integer
digitsInP  = genericLength (show p)

-- Here are the smaller Mersenne primes listed above:

smallMPindices :: [Int]
smallMPindices  = [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127,
                   521, 607, 1279, 2203, 2281, 3217, 4253, 4423 ]

smallMP  :: [Integer]
smallMP   = [ 2 ^ n - 1 | n <- smallMPindices ]

-- Does an incremental algorithm buy us anything?  Not much, it would seem!

smallMP' :: [Integer]
smallMP'  = map (subtract 1) (scanl (\x i -> x * 2^i) (2^n) ns)
            where (n:ns) = zipWith (-) smallMPindices (0:smallMPindices)

-----------------------------------------------------------------------------
