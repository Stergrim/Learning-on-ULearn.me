# data Queue [review]

Реализуйте [очередь с помощью двух стеков](https://neerc.ifmo.ru/wiki/index.php?title=%D0%9E%D1%87%D0%B5%D1%80%D0%B5%D0%B4%D1%8C#.D0.A0.D0.B5.D0.B0.D0.BB.D0.B8.D0.B7.D0.B0.D1.86.D0.B8.D1.8F_.D0.BD.D0.B0_.D0.B4.D0.B2.D1.83.D1.85_.D1.81.D1.82.D0.B5.D0.BA.D0.B0.D1.85), т.е. с помощью двух списков.

В такой реализации иногда нужно выполнить `O(n)` операций, но в среднем каждая функция будет работать за `O(1)`.


Все тесты пройдены, решение ожидает код-ревью:
```hs
module Queue where

data Queue a = Queue { inStack :: [a], outStack :: [a] } deriving (Show)

instance Eq a => Eq (Queue a) where
  q1 == q2 = toList q1 == toList q2
    where
      toList (Queue inSt outSt) = outSt ++ reverse inSt

createQueue :: Queue a
createQueue = Queue [] []

enqueue :: Queue a -> a -> Queue a
enqueue (Queue inSt outSt) x = Queue (x:inSt) outSt

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue inSt []) = dequeue $ Queue [] (reverse inSt)
dequeue (Queue inSt (x:xs)) = (x, Queue inSt xs)

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False
```

Вы можете скачать [тесты для локального запуска](Queue.zip).
