# Полугруппа списков

Реализуйте instance `Semigroup` для вектора чисел с помощью операции сложения:

```hs
> v = Vec [1,2,3,4]
> u = Vec [-1,2,0,-10,0,0]
> v <> u
Vec {unVec = [0,4,3,-6]}
```

```hs
newtype Vec a = Vec { unVec :: [a] } deriving (Eq, Show)
```


Все тесты пройдены, задача сдана:
```hs
instance Num a => Semigroup (Vec a) where
    Vec xs <> Vec ys = Vec $ zipWith (+) xs ys ++ drop (length xs) ys ++ drop (length ys) xs
```
