# Взять первые n

Напишите функцию `takeFirst n`, которая возвращает первые `n` элементов списка:

```hs
  - takeFirst 2 [1, 2, 3]    ~> [1, 2]
  - takeFirst (-4) [1, 2, 3] ~> []
```


Все тесты пройдены, задача сдана:
```hs
takeFirst :: Integer -> [a] -> [a]
takeFirst _ [] = []
takeFirst n _ | n <= 0 = []
takeFirst n (x:xs) = x : takeFirst (n-1) xs
```
