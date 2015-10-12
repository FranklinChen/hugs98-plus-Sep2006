module Hugs.Bits where

primitive primAndInt        :: Int -> Int -> Int
primitive primOrInt         :: Int -> Int -> Int
primitive primXorInt        :: Int -> Int -> Int
primitive primComplementInt :: Int -> Int
primitive primShiftInt      :: Int -> Int -> Int
primitive primBitInt        :: Int -> Int
primitive primTestInt       :: Int -> Int -> Bool
