# uncons

Напишите функцию `uncons`, которая возвращает пару из первого элемента, если он есть, и остатка списка.

```hs
- uncons [] ~> (Nothing, [])
- uncons [1,2,3] ~> (Just 1, [2, 3])
```


Все тесты пройдены, задача сдана:
```hs
uncons :: [a] -> (Maybe a, [a])
uncons [] = (Nothing,[])
uncons (x:xs) = (Just x,xs)
```
