# Проверка пароля [review]

В этой задаче вам нужно будет закончить тип, который должен скомбинировать монады `Writer` и `Reader`, и с помощью него написать небольшую функцию.

Скачать задачу можно [здесь](PasswordValidation.zip).

Вернёмся к примеру с лекции и усовершенствуем проверку пароля на надёжность. Для этого реализуйте следующие функции:
- `isCompromised` — проверяет, что пароль не лежит в базе скомпрометированных паролей
- `isValid` — проверяет пароль на надёжность с помощью функций `isCompromised` и `isStrong`.

Главная сложность заключается в том, что в функции `isCompromised` нам нужны сразу две монады. `Reader`, чтобы считать постоянно меняющуюся базу паролей, которая используется в нескольких функциях, а `Writer`, чтобы писать в лог сообщение о том, что мы нашли скомпрометированный пароль.


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PasswordValidation where

import Data.Char
import Control.Monad.Reader
import Control.Monad.Writer

newtype ValidationT a = ValidationT
  { runValidationT :: WriterT [String] (Reader [String]) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [String]
    , MonadReader [String]
    )

isStrong :: String -> Bool
isStrong password = length password >= 8
            && any isAlpha password
            && any isNumber password
            && any isPunctuation password

isCompromised :: String -> ValidationT Bool
isCompromised password = do
    badPasswords <- ask
    let compromised = password `elem` badPasswords
    tell ["Password is " ++ if compromised then "compromised" else "not compromised"]
    return (not compromised)

isValid :: String -> ValidationT Bool
isValid password = do
    strong <- return (isStrong password)
    compromised <- isCompromised password
    return (strong && compromised)

badPasswords = ["qwer", "qwerty", "qwer1234", "1234qwer", "1234", "12345678"]

runValidation :: ValidationT a -> (a, [String])
runValidation validationT = runReader (runWriterT $ runValidationT validationT) badPasswords
```
