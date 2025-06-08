# (Z, +) [review]

Что может быть удобней для проверки свойств, чем математические конструкции!

С помощью `QuickCheck` убедитесь, что целые числа `(Z, +)` — коммутативный моноид (т.е. [моноид](https://neerc.ifmo.ru/wiki/index.php?title=%D0%9C%D0%BE%D0%BD%D0%BE%D0%B8%D0%B4) с коммутативной операцией).


Все тесты пройдены, решение ожидает код-ревью:
```hs
module CommutativeMonoid where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Целые числа с операцией сложения" $ do
    prop "ассоциативность" $ \x y z -> (x + y) + (z :: Int) == (x :: Int) + (y + z)
    prop "наличие нейтрального элемента" $ \x -> x + (0 :: Int) == x && (0 :: Int) + x == x
    prop "коммутативность" $ \x y -> (x :: Int) + (y :: Int) == y + x
```
