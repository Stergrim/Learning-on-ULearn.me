# Функтор List

Реализуйте instance `Functor` для `List a`

```hs
data List a = Nil | Cons a (List a) deriving (Eq, Show)
```


Все тесты пройдены, задача сдана:
```hs
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
```
