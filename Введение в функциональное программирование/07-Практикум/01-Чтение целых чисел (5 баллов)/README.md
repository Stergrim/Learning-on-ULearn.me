# Чтение целых чисел

Реализуйте функцию `readIntegers :: String -> [Int]`. В качестве аргумента этой функции передаётся содержимое файла в следующем формате:
- файл — набор строк, в качестве разделителя используется `\n`
- строка состоит из целых чисел, разделённых пробельными символами
- количество чисел в разных строках может быть разным

Например:

```hs
12   0 -1 21234
0

23 5
```

Чтобы преобразовать строку в целое число, вы можете воспользоваться функцией `read`:

```hs
> (read "123") :: Int
123
```

**ВАЖНО**

В задаче запрещается использовать вспомогательные функции и рукописную рекурсию. Используйте встроенные функции.


Все тесты пройдены, задача сдана:
```hs
module IntegersReader where

readIntegers :: String -> [Int]
readIntegers = concatMap (map read . words) . lines
```
