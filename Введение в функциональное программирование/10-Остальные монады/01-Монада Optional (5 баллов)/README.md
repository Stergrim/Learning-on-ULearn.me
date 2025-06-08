# Монада Optional

Реализуйте instance класса `Monad` для `Optional` (`Maybe` другими словами).

Обратите внимание, что instance для `Functor` выведется автоматически с помощью `deriving` и расширений `DeriveFunctor` и `DeriveAnyClass`. Но выглядел бы он примерно так:

```hs
instance Functor Optional where
  fmap f optional = case optional of
    Some x -> Some $ f x
    None -> None
```


Все тесты пройдены, задача сдана:
```hs
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module MonadOptional where

data Optional a = Some a | None deriving (Eq, Show, Functor)

instance Applicative Optional where
  -- pure :: a -> Optional a
  pure x = Some x

  -- (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  f <*> a = case f of
    Some g -> fmap g a
    None   -> None

instance Monad Optional where
  -- (>>=) :: Optional a -> (a -> Optional b) -> Optional b
  a >>= f = case a of
    Some x -> f x
    None   -> None
```
