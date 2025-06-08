# inverse [review]

С помощью функции `foldl :: (b -> a -> b) -> b -> List a -> b` реализуйте функцию `inverse`, которая разворачивает список:

```hs
  - inverse [] ~> []
  - inverse ['a', 'b', 'c'] ~> ['c', 'b', 'a']
```

Все тесты пройдены, решение ожидает код-ревью:
```hs
inverse :: [a] -> [a]
inverse xs = foldl (\xs x -> x : xs) [] xs
```
