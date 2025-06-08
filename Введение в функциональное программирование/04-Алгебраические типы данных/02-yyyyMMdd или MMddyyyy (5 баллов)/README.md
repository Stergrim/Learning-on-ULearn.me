# yyyy/MM/dd или MM/dd/yyyy?

Напишите типы-обертки `Day`, `Month`, `Year` для `Int` с помощью `newtype`, чтобы предотвратить путаницу в предназначении числовых параметров. После этого придётся обновить функцию `showDate`.


Все тесты пройдены, задача сдана:
```hs
newtype Day   = Day   Int deriving (Show)
newtype Month = Month Int deriving (Show)
newtype Year  = Year  Int deriving (Show)

showDate :: Day -> Month -> Year -> String
showDate (Day d) (Month m) (Year y) = show m ++ "/" ++ show d ++ "/" ++ show y
```
