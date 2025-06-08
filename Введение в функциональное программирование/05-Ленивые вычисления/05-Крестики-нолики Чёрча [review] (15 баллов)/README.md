# Крестики-нолики Чёрча [review]

Вам нужно написать логику для игры в крестики-нолики, используя в качестве структуры, хранящей игровое поле, функцию:

```hs
-- Индексы игрового поля
data Index = First | Second | Third deriving Eq
-- Значения, которые могут лежать в ячейке
data Value = Zero | Cross | Empty deriving Eq
-- Строка игрового поля — функция, которая возвращает значение по индексу
type Row = Index -> Value
-- Игровое поле — функция, которая возвращает строку по индексу
type Field = Index -> Row
```

Другими словами, чтобы получить, что лежит в игровом поле `field` с индексами `1` и `3` нужно написать `field 1 3`.

Вы можете скачать [модуль для локального запуска игры](XsOs.zip). Для запуска игры из ghci загрузите модуль `XsOs` и вызовите функцию `startXsOs`.

Чтобы игра заработала вам нужно хоть как-нибудь реализовать функции `createField` и `getGameState`.

**Пример игры**

```hs
> :load XsOs.hs
> startXsOs

> New Game. Xs go first.

    1 2 3
    _____
  1│. . .
  2│. . .
  3│. . .

set x on i j>1 1

    1 2 3
    _____
  1│x . .
  2│. . .
  3│. . .

set o on i j>1 2

    1 2 3
    _____
  1│x o .
  2│. . .
  3│. . .

set x on i j>2 2

    1 2 3
    _____
  1│x o .
  2│. x .
  3│. . .

set o on i j>2 1

    1 2 3
    _____
  1│x o .
  2│o x .
  3│. . .

set x on i j>3 3

    1 2 3
    _____
  1│x o .
  2│o x .
  3│. . x

Game is over. Xs won. Congratulations!
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
module XsOsLogic where

import XsOsDtos

-- Создание строки поля с тремя значениями
createRow :: Value -> Value -> Value -> Row
createRow x y z = \col -> case col of
  First -> x
  Second -> y
  Third -> z

-- Создание поля из трех строк
createField :: Row -> Row -> Row -> Field
createField row1 row2 row3 = \row -> case row of
  First -> row1
  Second -> row2
  Third -> row3

-- Установка значения в конкретную ячейку строки
setCellInRow :: Row -> Index -> Value -> Row
setCellInRow row col value = \c -> 
  if c == col 
  then value 
  else row c

-- Установка значения в ячейку поля с проверкой занятости
setCell :: Field -> Index -> Index -> Value -> Either String Field
setCell field rowIdx colIdx value =
  let currentValue = field rowIdx colIdx
  in if currentValue /= Empty
     then Left $ "There is '" ++ showValue currentValue ++ "' on " ++ showIndex rowIdx ++ " " ++ showIndex colIdx
     else Right $ \r ->
       if r == rowIdx
       then setCellInRow (field r) colIdx value
       else field r
  where
    showValue Cross = "x"
    showValue Zero  = "o"
    showValue Empty = "."
    
    showIndex First  = "1"
    showIndex Second = "2"
    showIndex Third  = "3"

-- Получение всех линий для проверки (строки, столбцы, диагонали)
getAllLines :: Field -> [[Value]]
getAllLines field =
  let rows = [[field r c | c <- [First, Second, Third]] | r <- [First, Second, Third]]
      cols = [[field r c | r <- [First, Second, Third]] | c <- [First, Second, Third]]
      diag1 = [field i i | i <- [First, Second, Third]]
      diag2 = [field i (toDiag i) | i <- [First, Second, Third]]
  in rows ++ cols ++ [diag1, diag2]
  where
    toDiag First = Third
    toDiag Second = Second
    toDiag Third = First

-- Проверка, заполнена ли линия одним символом
isLineComplete :: [Value] -> Maybe Value
isLineComplete [a, b, c] 
  | a == b && b == c && a /= Empty = Just a
  | otherwise = Nothing
isLineComplete _ = Nothing

-- Определение состояния игры
getGameState :: Field -> GameState
getGameState field =
  case filter (/= Nothing) (map isLineComplete (getAllLines field)) of
    (Just Cross:_) -> XsWon
    (Just Zero:_)  -> OsWon
    _ -> if all (\r -> all (\c -> field r c /= Empty) [First, Second, Third]) [First, Second, Third]
         then Draw
         else InProgress
```
