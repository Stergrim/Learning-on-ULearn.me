# Решето Эратосфена

Напишите функцию, вычисляющую n-ое простое число с помощью [решета Эратосфена](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes), используя бесконечные списки.

Функция `sieve` "просеивает" список, переданный в качестве аргумента, и возвращает другой список, но уже из простых чисел. Работает она так:
- изначально в списке лежат все натуральные числа, начиная с первого простого числа — 2
- на каждом шаге из списка достаётся очередное число, оно всегда простое
- затем список "просеивается" — из него убирают все числа, которые делятся на это число
- запускается та же процедура, но для "просеянного" списка

Именно `sieve` должна работать на бесконечных списках, чтобы с её помощью реализовать `nthPrime`. И, кстати, она отлично будет смотреться с [list comprehension](https://wiki.haskell.org/List_comprehension)!


Все тесты пройдены, задача сдана:
```hs
module SieveOfEratosthenes where

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

nthPrime :: Int -> Integer
nthPrime n = sieve [2..] !! (n - 1)
```
