# Китай VS США [review]

В интервью Forbes с Сергеем Гуриевым Андрей Мовчан решил показать, что он тоже в некотором смысле математик, но немного запутался [в рассуждениях о ВВП Китая и США](https://www.youtube.com/watch?t=1782&v=bTnnTeXHp8w&feature=youtu.be).

Помогите математику Андрею как программисты. Используя бесконечные списки, напишите функцию, которая вычислит, через сколько лет ВВП на душу населения Китая догонит ВВП США.
- ВВП Китая $10к на душу населения, растёт на 6% в год
- ВВП США $66к на душу населения, растёт на 2% в год.

Можете воспользоваться функцией [iterate](https://hoogle.haskell.org/?hoogle=iterate). Она возвращает бесконечный список, каждый элемент которого получен из предыдущего применением первого аргумента ко второму:

```hs
iterate f x = [x, f x, f (f x), f (f (f x)), ...]
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
module ChinaVsUSA where

yearGDP :: Double -> Double -> [Double]
yearGDP now percent = iterate (\x -> x * (1 + percent/100)) now

inHowManyYearsChinaWins :: Int
inHowManyYearsChinaWins = length $ takeWhile id $ zipWith (<) chinaGDP usaGDP
  where
    chinaGDP = yearGDP 10000 6
    usaGDP = yearGDP 66000 2
```
