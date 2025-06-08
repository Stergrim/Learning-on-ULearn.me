# Обработка CSV [review]

Вам нужно реализовать функцию `processCSV`, которая обрабатывает [csv](https://ru.wikipedia.org/wiki/CSV)-файл, содержащий таблицу с целыми числами. Обработка таблицы заключается в ее считывании, замене указанных значений и добавлении нового столбца SUM, который содержит суммы чисел в строках новой таблицы. Функция `processCSV :: String -> String -> String -> IO()` принимает:
- *Имя файла, который нужно изменить* <br> Первая строка файла содержит заголовки колонок, перечисленных через `,`. В следующих строках записаны целочисленные значения ячеек таблицы, тоже перечисленные через `,`
- *Имя файла с изменениями* <br> В каждой строке описано ровно одно изменение в формате `имя_столбца:номер_строки=новое_значение`, где `новое_значение` — тоже целое число
- *Имя файла, в который нужно записать результат* <br> Отличается от исходного файла новой последней колонкой `SUM` и ячейками, перечисленными в файле с изменениями.


**Посмотреть пример**

`in.csv`

```hs
H1,H2,H3
1,-2,3
4,5,6
7,8,9
```

`changes.txt`

```hs
H2:2=100
H1:3=-10
H2:2=333
```

**Запуск**

```hs
> processCSV "in.csv" "changes.txt" "out.csv"
```

`out.csv`

```hs
H1,H2,H3,SUM
1,-2,3,2
4,333,6,343
-10,8,9,7
```

Гарантируется, что имена колонок не содержат символы `,` и пробел, ни одна колонка не называется `SUM`, а данные и изменения корректны (все строки в файле с таблицей содержат одинаковое количество чисел, а изменения содержат только существующие ячейки).

Одна и та же ячейка может встречаться в списке изменений несколько раз. В итоговую таблицу должно попасть значение, указанное последним.


Все тесты пройдены, решение ожидает код-ревью:
```hs
module CSVManip where

import Data.List
import qualified Data.Map as Map
import Data.Maybe

processCSV :: String -> String -> String -> IO ()
processCSV inputFile changesFile outputFile = do
    content <- readFile inputFile
    let (headerLine:rows) = lines content
    let headers = splitOnComma headerLine
    let table = map (map read . splitOnComma) rows :: [[Int]]
    
    changesContent <- readFile changesFile
    let changes = parseChanges headers (lines changesContent)
    
    let modifiedTable = applyChanges changes table
    let tableWithSum = addSumColumn modifiedTable
    let newHeaderLine = intercalate "," (headers ++ ["SUM"])
    let newRows = map (intercalate "," . map show) tableWithSum
    let newContent = unlines (newHeaderLine : newRows)
    
    writeFile outputFile newContent

splitOnComma :: String -> [String]
splitOnComma = foldr splitHelper []
  where
    splitHelper ',' acc = "" : acc
    splitHelper c (x:xs) = (c:x) : xs
    splitHelper c [] = [[c]]

parseChanges :: [String] -> [String] -> Map.Map (Int, Int) Int
parseChanges headers = foldl parseChange Map.empty
  where
    parseChange acc line =
        let (colPart, valuePart) = break (== '=') line
            valuePart' = drop 1 valuePart
            (colName, rowStr) = break (== ':') colPart
            rowStr' = drop 1 rowStr
            row = read rowStr'
            col = case findIndex' (\h -> h == colName) headers of
                    Just idx -> idx
                    Nothing -> error "Column not found"
            value = read valuePart'
        in Map.insert (row-1, col) value acc

applyChanges :: Map.Map (Int, Int) Int -> [[Int]] -> [[Int]]
applyChanges changes table =
    let maxRow = length table - 1
        maxCol = if null table then 0 else length (head table) - 1
    in [[ fromMaybe (table !! r !! c) (Map.lookup (r, c) changes)
        | c <- [0..maxCol] ]
        | r <- [0..maxRow] ]

addSumColumn :: [[Int]] -> [[Int]]
addSumColumn = map (\row -> row ++ [sum row])

findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' p = go 0
  where
    go _ [] = Nothing
    go n (x:xs)
        | p x       = Just n
        | otherwise = go (n+1) xs
```

Вы можете скачать [тесты для локального запуска](CSVManip.zip).
