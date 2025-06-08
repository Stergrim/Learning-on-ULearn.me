# Длина списка [review]

Напишите функцию `getLength xs`, которая возвращает длину списка `xs`:

```hs
  - getLength []        ~> 0
  - getLength [1, 2, 3] ~> 3
  - getLength ['a', 'b']  ~> 2
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
getLength :: [a] -> Integer
getLength [] = 0
getLength (x:xs) = 1 + getLength xs
```
