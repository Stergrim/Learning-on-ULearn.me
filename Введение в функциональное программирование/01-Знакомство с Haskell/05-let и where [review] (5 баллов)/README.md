# let и where [review]

Упростите выражение, считающее корни квадратного уравнения, используя `let` и `where`.

```hs
-- x^2 + 4x + 3 = 0
> getRoots 1 4 3
(-3.0, -1.0)

-- x^2 + x + 1 = 0
> getRoots 1 1 1
(-0.0, -0.0)
```

Все тесты пройдены, решение ожидает код-ревью:
```hs
defaultValue :: (Double, Double)
defaultValue = (0, 0)

getRoots :: Double -> Double -> Double -> (Double, Double)
getRoots a b c =
    if d < 0 then defaultValue else (x1, x2)
    where
        d = b * b - 4 * a * c;
        x1 = (-b - sqrt d) / 2 / a;
        x2 = (-b + sqrt d) / 2 / a
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
