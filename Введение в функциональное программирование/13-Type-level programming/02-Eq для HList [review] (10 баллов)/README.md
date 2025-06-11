# Eq для HList [review]

Реализуйте `instance Eq` для `HList`

```hs
data HList (xs :: [*]) :: * where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module HListEq where

import HList
import Data.Kind

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  (HCons x xs) == (HCons y ys) = x == y && xs == ys
```
