# intercalate

С помощью функции `foldr :: (a -> b -> b) -> b -> List a -> b` реализуйте функцию `intercalate`, которая вставляет список элементов между другими списками.

Например, её можно использовать как функцию `string.Join` в C# или `join` в Python.

```hs
  intercalate ", " ["hello", "world"] ~> "hello, world"
```


Все тесты пройдены, задача сдана:
```hs
intercalate :: [a] -> [[a]] -> [a]
intercalate insert [] = []
intercalate insert (x:xs) = x ++ foldr ((++) . (insert ++)) [] xs
```
