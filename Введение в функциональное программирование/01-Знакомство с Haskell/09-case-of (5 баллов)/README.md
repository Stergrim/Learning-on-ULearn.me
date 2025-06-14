# case-of

На самом деле синтаксис pattern matching в определении функций сам является синтаксическим сахаром над конструкцией `case-of`. Ещё раз перепишем функцию `countdown` из лекции:

```hs
countdown'' :: Int -> Int
countdown'' x = case x of
  0 -> 0
  x -> countdown'' (x - 1)
```

Напишите функцию функцию логического отрицания с использованием `case-of`.

Все тесты пройдены, задача сдана:
```hs
not :: Bool -> Bool
not x
  | x == True  = False
  | x == False = True
```

`Ambiguous occurrence 'not'`

При вызове функции `not` в ghci вы можете столкнуться с ошибкой `Ambiguous occurrence 'not'`. Она возникает потому, что в Haskell уже определена функция `not`. Чтобы избежать этой ошибки, добавьте в начало файла с функцией `not` следующую строчку:

```hs
import Prelude hiding (not)
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
