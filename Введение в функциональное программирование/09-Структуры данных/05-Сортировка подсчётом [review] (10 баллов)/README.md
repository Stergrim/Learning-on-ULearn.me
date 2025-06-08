# Сортировка подсчётом [review]

*Для выполнения этого задания вам нужно хорошо понимать, что такое аппликация типа.*

Вам нужно реализовать [сортировку подсчётом](https://neerc.ifmo.ru/wiki/index.php?title=%D0%A1%D0%BE%D1%80%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%BA%D0%B0_%D0%BF%D0%BE%D0%B4%D1%81%D1%87%D1%91%D1%82%D0%BE%D0%BC) с использованием класса типов `IntArray` из предыдущего задания. В этом же задании вы можете найти пример функции, использующей этот класс типов.

В этом задании вам понадобятся расширения `ScopedTypeVariables` и `TypeApplications`,
о [которых мы уже рассказывали](https://ulearn.me/course/fpintroduction/Nazad_v_Haskell_fb964b55-6b60-4793-a2c8-88fbcf1e77bf).

Tак можно будет запустить функцию `countingSort` с использованием конкретной реализацией массива с помощью `TypeApplications`:

```hs
> :set -XTypeApplications
> countingSort @[Int] [2,2,2,3,3,3,1,1,1]
[1,1,1,2,2,2,3,3,3]
> countingSort @(Map.IntMap Int) [2,2,2,3,3,3,1,1,1]
[1,1,1,2,2,2,3,3,3]
```

**Модуль `Time`**

Вы можете сравнить скорость работы разных реализаций с помощью функции `computeTime`, которая принимает:
- максимальное значение чисел для генерации
- размер списка
- список реализаций (имя, функция)

и печатает время выполнения для каждой реализации в секундах.

```hs
> :load Lecture08/Time.hs
> :set -XTypeApplications
> computeTime 20 (10^7) [("[Int]", countingSort @[Int])]
[Int]: 2.325400635
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, FlexibleInstances #-}

module CountingSort where

import Data.List
import Data.Array
import qualified Data.Array as Array

class IntArray a where
  iaEmpty :: Int -> a
  iaLength :: a -> Int
  iaRead :: a -> Int -> Int
  iaWrite :: a -> Int -> Int -> a
  iaToList :: a -> [Int]

instance IntArray [Int] where
  iaEmpty n = replicate n 0
  iaLength = length
  iaRead xs i = xs !! i
  iaWrite xs i v = take i xs ++ [v] ++ drop (i+1) xs
  iaToList = id

instance IntArray (Array Int Int) where
  iaEmpty n = listArray (0, n-1) (replicate n 0)
  iaLength arr = snd (Array.bounds arr) + 1
  iaRead = (Array.!)
  iaWrite arr i v = arr Array.// [(i, v)]
  iaToList = Array.elems

countingSort :: forall a. IntArray a => [Int] -> [Int]
countingSort [] = []
countingSort xs = concat [replicate cnt (i + minVal) | (i, cnt) <- zip [0..] countsList]
  where
    minVal = minimum xs
    maxVal = maximum xs
    rangeSize = maxVal - minVal + 1
    counts = foldl' count (iaEmpty @a rangeSize) xs
    count arr x = let idx = x - minVal
                  in iaWrite arr idx (iaRead arr idx + 1)
    countsList = iaToList counts
```

Вы можете скачать [тесты для локального запуска](CountingSort.zip).
