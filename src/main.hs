module F1 where

import Data.Char


-- Fibonacci 

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1 
fib n = 2 * fib (n - 2) + fib (n - 3)
--Do it any other way and the time limit will exceed!

--rovarspråk

rovarsprak :: [Char] -> [Char]
rovarsprak (x:xs)
    | elem x vokaler = x : rovarsprak xs
    | otherwise = x : ('o' : (x : (rovarsprak xs)))
    where vokaler = "aeiouy"
rovarsprak [] = []

karpsravor :: [Char] -> [Char]
karpsravor (x:xs)
    | elem x vokaler = x : karpsravor xs
    | otherwise = x : karpsravor (tail (tail xs))
    where vokaler = "aeiouy"
karpsravor [] = []

--Medellängd

antal_bokstaver :: String -> Double
antal_bokstaver [] = 0.0
antal_bokstaver (x:xs) =
    if isAlpha x
    then 1.0 + antal_bokstaver xs
    else antal_bokstaver xs

antal_ord :: String -> Double
antal_ord [] = 0.0
antal_ord (x:xs) =
    if isAlpha x && (null xs || not (isAlpha (head xs)))
    then 1.0 + antal_ord xs
    else antal_ord xs

medellangd :: String -> Double
medellangd [] = 0.0
medellangd s = antal_bokstaver s / antal_ord s

--The shuffler

varannan :: [x] -> [x]
varannan [] = []
varannan [x] = [x]
varannan (x:xs) = x:varannan (tail xs)

skyffla :: [x] -> [x]
skyffla [] = []
skyffla list@(x:xs) = varannan list ++ skyffla (varannan xs)
    

