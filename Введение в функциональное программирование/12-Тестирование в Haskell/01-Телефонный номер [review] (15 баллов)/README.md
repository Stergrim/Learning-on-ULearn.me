# Телефонный номер [review]

В этом задании вам нужно изучить функцию `parse`, которая проверяет телефонный номер на корректность и приводит его к некоторому стандартному виду, и написать тесты, закрепляющие её работу, с помощью hspec.
Автоматическая проверка

Ваши тесты должны пройти автоматическую проверку. Она устроена следующим образом:
- В первую очередь проверяется, что функция `parse` проходит тесты
- После этого тесты должны показать свою "полезность":
  - Мы приготовили несколько неправильных реализаций функции `parse`
  - Ваши тесты запускаются для этих неправильных реализаций
  - Каждая должна упасть хотя бы на одном тесте.

Ручная проверка

После того, как ваши тесты прошли автоматическую проверку, они попадают прямиком на ревью. На что преподаватели будут обращать внимание:
- Заголовки тестов отражают то, что в них проверяется, чтобы по результатам запуска можно было понять, что не работает
- Тесты структурированы с помощью вложенных блоков `describe`, чтобы результаты запуска было проще читать
- Каждая неправильная реализация падает на наименьшем возможном количестве тестов, причём разные реализации падают на разных тестах. Если вы написали один огромный тест, на котором падают все реализации, значит по результатам запуска невозможно понять, что именно сломалось.

Исходники

Скачать функцию `parse` и файл для тестов можно [здесь](TestsHspec.zip).


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE OverloadedStrings #-}

module TestsHspec where

import Test.Hspec
import PhoneNumberNormalization

spec :: (PhoneNumber -> Either Error PhoneNumber) -> Spec
spec parse = do
  describe "Phone number validation" $ do
    describe "Empty or whitespace input" $ do
      it "should reject empty string" $
        parse "" `shouldBe` Left IsEmptyOrWhitespace
      
      it "should reject whitespace-only string" $
        parse "   " `shouldBe` Left IsEmptyOrWhitespace
      
      it "should reject string with tabs and spaces" $
        parse "\t \t" `shouldBe` Left IsEmptyOrWhitespace

    describe "Invalid characters" $ do
      it "should reject letters" $
        parse "123abc45678" `shouldBe` Left Invalid
      
      it "should reject special symbols" $
        parse "123!456@789" `shouldBe` Left Invalid
      
      it "should reject dots" $
        parse "123.456.789" `shouldBe` Left Invalid

  describe "Valid phone number formats" $ do
    it "should accept plain digits" $
      parse "79001234567" `shouldBe` Right "79001234567"
      
    it "should accept spaces" $
      parse "7 900 123 45 67" `shouldBe` Right "79001234567"
      
    it "should accept hyphens" $
      parse "7-900-123-45-67" `shouldBe` Right "79001234567"
      
    it "should accept parentheses" $
      parse "7(900)1234567" `shouldBe` Right "79001234567"
      
    it "should accept plus sign" $
      parse "+79001234567" `shouldBe` Right "79001234567"
      
    it "should accept mixed valid symbols" $
      parse "+7 (900) 123-45-67" `shouldBe` Right "79001234567"

  describe "Length validation" $ do
    it "should reject numbers shorter than 11 digits" $
      parse "7900123456" `shouldBe` Left Invalid
      
    it "should reject numbers longer than 11 digits" $
      parse "790012345678" `shouldBe` Left Invalid
      
    it "should reject 11 non-digit symbols" $
      parse "----------" `shouldBe` Left Invalid
      
    it "should accept 11 digits with extra symbols" $
      parse "7-9-0-0-1-2-3-4-5-6-7" `shouldBe` Right "79001234567"

  describe "Edge cases" $ do
    it "should trim leading/trailing whitespace" $
      parse "  79001234567  " `shouldBe` Right "79001234567"
      
    it "should accept multiple valid symbols" $
      parse "++7 (((900))) --123--45--67" `shouldBe` Right "79001234567"
      
    it "should reject all non-digit symbols" $
      parse "-----------" `shouldBe` Left Invalid
```
