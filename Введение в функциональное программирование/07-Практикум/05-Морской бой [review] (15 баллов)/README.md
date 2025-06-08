# Морской бой [review]

Всем хорошо известна игра "Морской бой". На поле 10 × 10 клеток расставлены:
- четыре 1-палубных корабля
- три 2-палубных
- два 3-палубных
- и один 4-палубный.

**N-палубный** корабль занимает на поле *n* клеток, расположенных *подряд* по горизонтали или вертикали. Разные корабли не должны пересекаться или касаться друг друга (углами или сторонами своих клеток).

Пусть определены следующие типы для хранения расстановки кораблей:

```hs
-- Клетка — пара целых координат
type Cell = (Int,Int)

-- Корабль — имя и список клеток, занимаемых данным кораблем
-- (в каком-то порядке)
type Ship = (String, [Cell])

-- Флот — список кораблей
type Fleet = [Ship]
```

Напишите функцию `checkFleet :: Fleet -> [Error]`, проверяющую корректность расстановки кораблей и выдающую список **всех** нарушений правил расстановки кораблей. Ошибки могут идти в любом порядке.

**Посмотреть тип Error**

Для описания ошибки определен следующий алгебраический тип:

```hs
data Error
      -- Кораблей больше 10
    = WrongTotal
      -- У указанного корабля больше 4х палуб
    | WrongType String
      -- Не хватает кораблей указанного размера
      -- `TooFew 4` означает, что не хватает 4-палубного корабля
    | TooFew Int
      -- Слишком много кораблей указанного размера
      -- `TooFew 2` означает, что 2-палубных кораблей больше трёх
    | TooMany Int
      -- Указанный корабль неправильно расположен
    | WrongConfiguration String
      -- Указанный корабль выходит за поле
    | OutOfField String
      -- Указанные корабли касаются или пересекаются
      -- Имена кораблей могут быть указаны в любом порядке
    | TooClose String String
    deriving (Show,Eq,Ord)
```

**Посмотреть пример**

Например, для расстановки (`.` - пустая клетка, `o` - клетка корабля)

```hs
..........
.o......oo <- Два касающихся двухпалубных корабля,
......oo.. <- имена "Ship1" и "Ship2"
.ooo......
..........
oo....oo..
..........
.....o..oo
oooo......o <- "кривой" трёхпалубный корабль,
..........     выходящий за границу поля; имя "BadShip"
```

Функция `checkFleet` должна вернуть такой список (или какую-то его перестановку):

```hs
[ WrongTotal
, WrongConfiguration "BadShip"
, OutOfField "BadShip"
, TooFew 1
, TooMany 2
, TooClose "Ship2" "Ship1"
]
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
module SeaBattle where

import Dto
import Data.List (sort, nub)

-- Основная функция проверки
checkFleet :: Fleet -> [Error]
checkFleet fleet = nub $ concat
    [ checkTotal fleet
    , checkShipCounts fleet
    , concatMap checkShipValidity fleet
    , checkAllTooClose fleet
    ]

-- Проверка общего количества кораблей
checkTotal :: Fleet -> [Error]
checkTotal fleet
    | length fleet /= 10 = [WrongTotal]
    | otherwise = []

-- Проверка количества кораблей каждого типа
checkShipCounts :: Fleet -> [Error]
checkShipCounts fleet = 
    let sizes = map (length . snd) fleet
        count n = length $ filter (==n) sizes
        errors = concat
            [ checkCount 1 (count 1) 4
            , checkCount 2 (count 2) 3
            , checkCount 3 (count 3) 2
            , checkCount 4 (count 4) 1
            ]
    in errors
    where
        checkCount size actual expected
            | actual < expected = [TooFew size]
            | actual > expected = [TooMany size]
            | otherwise = []

-- Проверка валидности отдельного корабля
checkShipValidity :: Ship -> [Error]
checkShipValidity (name, cells) = nub $ concat
    [ checkShipSize name cells
    , checkShipConfiguration name cells
    , checkShipOutOfField name cells
    ]

-- Проверка размера корабля
checkShipSize :: String -> [Cell] -> [Error]
checkShipSize name cells
    | length cells > 4 = [WrongType name]
    | otherwise = []

-- Проверка конфигурации корабля
checkShipConfiguration :: String -> [Cell] -> [Error]
checkShipConfiguration name cells
    | not (isValidShip cells) = [WrongConfiguration name]
    | otherwise = []
    where
        isValidShip cells
            | length (nub cells) /= length cells = False  -- дубликаты клеток
            | length cells == 1 = True
            | otherwise =
                let sortedX = sort $ map fst cells
                    sortedY = sort $ map snd cells
                    xLine = all (== head sortedX) (tail sortedX) && isConsecutive sortedY
                    yLine = all (== head sortedY) (tail sortedY) && isConsecutive sortedX
                in xLine || yLine
        
        isConsecutive [] = True
        isConsecutive [_] = True
        isConsecutive (x:y:xs) = x + 1 == y && isConsecutive (y:xs)

-- Проверка выхода за границы поля
checkShipOutOfField :: String -> [Cell] -> [Error]
checkShipOutOfField name cells
    | any (\(x,y) -> x < 1 || x > 10 || y < 1 || y > 10) cells = [OutOfField name]
    | otherwise = []

-- Проверка всех пар кораблей на касание
checkAllTooClose :: Fleet -> [Error]
checkAllTooClose fleet =
    let ships = [(name, nub cells) | (name, cells) <- fleet]  -- удаляем дубликаты клеток
        pairs = [(s1, s2) | s1 <- ships, s2 <- ships, fst s1 < fst s2]
    in nub [TooClose n1 n2 | ((n1,c1), (n2,c2)) <- pairs, areTooClose c1 c2]

-- Проверка касания двух кораблей
areTooClose :: [Cell] -> [Cell] -> Bool
areTooClose cells1 cells2 =
    any (\(x1,y1) -> any (isAdjacent (x1,y1)) cells2) cells1
    where
        isAdjacent (x1,y1) (x2,y2) =
            abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1
```

Вы можете скачать [тесты для локального запуска](SeaBattle.zip).
