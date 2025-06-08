# data Stack

Стек — одна из самых простых для реализации в функциональном программировании структура данных. Её очень легко реализовать на списках. Именно это мы и предлагаем вам сделать.

[Здесь](https://neerc.ifmo.ru/wiki/index.php?title=%D0%A1%D1%82%D0%B5%D0%BA) вы можете прочитать про стек, если это необходимо. Основные операции стека: `push`, `pop` и `isEmpty`, иногда к ним добавляют `peek` (посмотреть вершину, не удаляя).

Обратите внимание, что все структуры данных неизменяемые (immutable). Это значит, если операция предполагает изменение структуры, то она должна возвращать новую, уже изменённую версию.


Все тесты пройдены, задача сдана:
```hs
module Stack where

data Stack a = Stack [a] deriving (Eq, Show)

createStack :: Stack a
createStack = Stack []

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x:xs)

pop :: Stack a -> Maybe (Stack a)
pop (Stack [])    = Nothing
pop (Stack (_:xs)) = Just (Stack xs)

peek :: Stack a -> Maybe a
peek (Stack [])    = Nothing
peek (Stack (x:_)) = Just x
```
