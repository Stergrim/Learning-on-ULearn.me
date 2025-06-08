# Знак числа

Начнём с крошечной функции `tellSign`. Она должна возвращать знак целого числа `n`:
- если n = 0, то "zero"
- если n > 0, то "positive"
- если n < 0, то "negative"


Все тесты пройдены, задача сдана:
```hs
tellSign :: Int -> String
tellSign n =
    if n < 0 then "negative" else 
        if n > 0 then "positive" else "zero"
```

**Компиляция в GHC**

Если вы скопируете код выше в файл и скомпилируете его как есть с помощью GHC, а не загрузите в GHCi, то компилятор напишет ошибку:

```hs
The IO action `main' is not defined in module `Main'
```

Чтобы её поправить, нужно просто добавить в файл функцию `main`. Её можно взять, например, из лекции:

```hs
main = putStrLn "Hello, World!"
```

Не забудьте убрать `main` перед отправкой.
