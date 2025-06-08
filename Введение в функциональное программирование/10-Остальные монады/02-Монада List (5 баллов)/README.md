# Монада List

Реализуйте instance класса `Monad` для типа `List`.


Все тесты пройдены, задача сдана:
```hs
module MonadList where

-- Объявим свой тип для списка
data List a = Nil | a :. (List a) deriving (Eq, Show)

-- Зададим приоритет оператора (:.)
infixr 5 :.

-- Вспомогательная функция для объединения списков
concatList :: List a -> List a -> List a
concatList Nil ys = ys
concatList (x :. xs) ys = x :. concatList xs ys

-- Для Monad требуется реализация Functor и Applicative
instance Functor List where
    fmap _ Nil = Nil
    fmap f (x :. xs) = f x :. fmap f xs

instance Applicative List where
    pure x = x :. Nil
    Nil <*> _ = Nil
    (f :. fs) <*> xs = concatList (fmap f xs) (fs <*> xs)

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    (x :. xs) >>= f = concatList (f x) (xs >>= f)
```
