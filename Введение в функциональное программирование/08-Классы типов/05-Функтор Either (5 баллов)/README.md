# Функтор Either

Реализуйте instance `Functor` для `Either' a`:

```hs
> value = Right 3 :: Either String Int
> error = Left "error message" :: Either String Int
> fmap (+1) value
Right 4
> fmap (+1) error
Left "error message"
```

```hs
data Either' a b = Left' a | Right' b deriving (Eq, Show)
```

Обратите внимание, что класс `Functor` предполагает, что полиморфный тип `f` принимает только одну переменную типа:

```hs
--                    v      v
fmap :: (a -> b) -> f a -> f b
```

А тип `Either'` — две. Поэтому в класс `Functor` нужно добавить не тип `Either'`, а тип `Either' a`, в котором вы как будто закрепили первую переменную типа так же, как мы это делаем для каррированных функций.


Все тесты пройдены, задача сдана:
```hs
instance Functor (Either' a) where
    fmap _ (Left' x)  = Left' x
    fmap f (Right' y) = Right' (f y)
```
