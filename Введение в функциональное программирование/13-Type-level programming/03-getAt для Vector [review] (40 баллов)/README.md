# getAt для Vector [review]

Выполнение этого задание требует понимание того, что такое `type family` и `GADTs` и как их использовать.

Вам нужно самостоятельно разобраться с тем, как устроен тип списка фиксированной длины `Vector` и что такое синглтон и после этого написать небольшую функцию для `Vector`.

В первой части этого слайда будут описаны условия, т.е. собственно реализация типа `Vector`. А во второй — задание.

**Список фиксированной длины**

Наша задача — создать список заданного размера `Vector`. Результат должен выглядеть примерно так:

```hs
--               Тип элементов и их количество
--                          v     v
Nil                 :: Vector a   0
Cons 2 Nil          :: Vector Int 1
Cons 3 $ Cons 2 Nil :: Vector Int 2
...
```

Наша первая задача — перевести натуральные числа в ADT, чтобы мы могли использовать их в типе `Vector` вместе с `'`.

Определим тип натуральных чисел, используя арифметику Пеано:

```hs
0 = Zero
1 = Succ Zero
2 = Succ $ Succ Zero
...
```

```hs
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- Например, так можно написать функцию конвертации
-- положительных целых чисел в значение типа Nat
fromInt :: Int -> Nat
fromInt 0 = Zero
fromInt n = Succ (fromInt $ n - 1)
```

Пока мы не сделали ничего сложного. Мы создали неполиморфный тип `Nat` с двумя конструкторами `Zero` и `Succ`:

```hs
kinds :   Type
            |
types :    Nat
          /   \
terms : Zero Succ
```

**`data Vector`**

Теперь мы можем объявить тип `Vector` и instance `Show` для него:

```hs
-- ' применяют к конструктору, поэтому мы вынуждены
-- использовать конструктор Nat, а не обычные числа
data Vector a (n :: Nat) where
  Nil :: Vector a 'Zero
  (:|) :: a -> Vector a n -> Vector a ('Succ n)

infixr 5 :|

instance Show a => Show (Vector a n) where
  show Nil = "nil"
  show (x :| xs) = show x ++ " : " ++ show xs
```

```hs
> v = 1 :| 2 :| 3 :| Nil
> v
1 : 2 : 3 : nil
```

Это было совсем несложно! Теперь пришло время написать для `Vector` несколько функций.

**Синглтон `SNat`**

Для этого нам понадобится создать [тип-синглтон](https://stackoverflow.com/questions/16017294/singleton-types-in-haskell).

```hs
-- SNat — синглтон
data SNat :: Nat -> Type where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)
```

Определение `SNat` структурно полностью повторяет `Nat`.

Отличие в том, что `SNat` параметризован типом `Nat`. То есть, `SNat` — это конструктор типа, который на вход принимает типы `'Zero` и `'Succ`, kind которых — `Nat`.

Мы получили следующую картину:

```hs
kinds :    Type         Nat (kind)
            |            /    \
types :    Nat        'Zero 'Succ      SNat ['Zero или 'Succ] (singleton type)
          /   \                            /      \
terms : Zero Succ                       SZero   SSucc
```

По умолчанию, Haskell стирает все типы во время компиляции и в runtime типов нет. Когда мы повышаем терм `Zero` до значения '`Zero` kind'а `Nat`, то он существует только на стадии проверки типов.

Синглтон позволяет нам создать runtime версию типа. Имея терм `SZero`, мы можем вызывать функцию, которая ожидает тип `SNat 'Zero`, и так далее по индукции.

Заметим, что конструкторы `SNat` повторяют конструкторы `Nat`. И для каждого типа `SNat` n (то есть полностью примененного, не ожидающего аргумент), существует только один терм `Nat` (кроме `undefined`):

```hs
Zero <-> SZero
Succ n <-> SSucc n
```

Именно поэтому `SNat` называется синглтоном. Он повторяет структуру `Nat`, а конструкторы `Nat` изоморфны типам, которые параметризуют синглтон:

```hs
Zero <-> 'Zero
Succ <-> 'Succ
```

**`replicateVec`**

Так с помощью синглтона `SNat` можно реализовать аналог функции `replicate` для списков:

```hs
replicateVec :: SNat n -> a -> Vector a n
replicateVec sn x = case sn of
  SZero -> Nil
  (SSucc sn) -> x :| (replicateVec sn x)
```

**Задание**

Реализуйте функцию `getAt`, в качестве аргументов принимает список фиксированной длины `Vector` a n и число `SNat` m и возвращает элемент, находящийся по индексу `m`. Главная особенность этого метода — он не компилируется, если ему передать число больше либо равно длине вектора.

**`SNats`**

Для удобства в модуле `SNats` объявленo 101 первое число `SNat`:

```hs
s0 = SZero
s1 = SSucc (SZero)
s2 = SSucc (SSucc (SZero))
...
s100 = SSucc (SSucc ( SSucc (SSucc ... (SZero))
```

Так их можно использовать для вызова функции `getAt`:

```hs
> getAt (1 :| 2 :| 3 :| Nil) s0
1
> getAt ('a' :| 'b' :| Nil) s1
'b'
> getAt (1 :| 2 :| 3 :| Nil) s3

<interactive>:2:1: error:
    * Couldn't match type *censured* arising from a use of `getAt'
    * In the expression: getAt (1 :| 2 :| 3 :| Nil) s3
      In an equation for `it': it = getAt (1 :| 2 :| 3 :| Nil) s3
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GetAtVector
  ( module GetAtVector
  , module Vector
  , module SNats
  ) where

import Vector
import SNats

getAt :: Vector a n -> SNat m -> a
getAt (x :| _) SZero = x
getAt (_ :| xs) (SSucc n) = getAt xs n
getAt Nil _ = error "Impossible case - type system prevents this"
```

Вы можете скачать [тесты для локального запуска](GetAtVector.zip).
