# Класс IntArray [review]

Допустим мы реализовали некоторый алгоритм, в котором требуется использование массива. Например, [сортировку подсчётом](https://neerc.ifmo.ru/wiki/index.php?title=%D0%A1%D0%BE%D1%80%D1%82%D0%B8%D1%80%D0%BE%D0%B2%D0%BA%D0%B0_%D0%BF%D0%BE%D0%B4%D1%81%D1%87%D1%91%D1%82%D0%BE%D0%BC).

Теперь мы хотим понять, какой тип массива-заменителя нужно выбрать. Для этого внутри алгоритма мы будем использовать не конкретный тип, а класс типов, а при тестировании — instance'ы этого типа.

**В этом задании вам нужно создать класс типов** `IntArray`, который будет включать типы, хранящие целые числа, и реализовать его instance'ы для каждой из следующих структур:
- `[Int]`
- `Array Int Int`
- Один из `IntMap Int`, `Map Int Int`, `Data.HashMap Int Int` или любой другой разновидности массивов, которых в Haskell [много](https://wiki.haskell.org/Arrays)

**В классе `IntArray` должны быть методы для:**
- создания из списка пар `[(index, value)]`
- преобразования в список пар `[(index, value)]`
- обновления элемента по индексу
- получения элемента по индексу

Вы можете выбрать любые имена для методов (возможно, вам захочется объявить последние два метода как операторы), жёстко закреплено только имя класса `IntArray`.

**Чтобы мы могли убедиться в том, что созданные вами instance'ы работают правильно**, замените имена функций `f1`, `f2`, `f3`, `f4` в функции `test` в соответствии с объявленным вами классом IntArray.

В задаче используются расширения `AllowAmbiguousTypes` и `FlexibleInstances`. Они нужны для того, чтобы создать instace'ы класса `IntArray` для полиморфных типов с несколькими параметрами. Они уже включены в файле с решением и их понимание не нужно для решения задачи.

Вы можете скачать [тесты для локального запуска](CountingSortClasses.zip).


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, ScopedTypeVariables #-}

module CountingSortClasses where

import Data.List
import qualified Data.Array as A
import qualified Data.Map as M

class IntArray a where
  fromList :: [(Int, Int)] -> a
  toList :: a -> [(Int, Int)]
  (!) :: a -> Int -> Int
  update :: a -> Int -> Int -> a

instance IntArray [Int] where
  fromList xs = [ snd pair | pair <- sortOn fst xs ]
  toList arr = zip [0..] arr
  (!) arr i = arr !! i
  update arr i x = take i arr ++ [x] ++ drop (i+1) arr

instance IntArray (A.Array Int Int) where
  fromList xs = A.array (0, maximum (map fst xs)) xs
  toList arr = A.assocs arr
  (!) = (A.!)
  update arr i x = arr A.// [(i, x)]

instance IntArray (M.Map Int Int) where
  fromList = M.fromList
  toList = M.toList
  (!) = (M.!)
  update m i x = M.insert i x m

test :: forall l. IntArray l => [(Int, Int)] -> l -> Int -> [(Int, Int)]
test xs array i = toList newArray
  where
    xsArray :: l
    xsArray = fromList xs

    x :: Int
    x = xsArray ! i

    newArray :: l
    newArray = update array i x
```
