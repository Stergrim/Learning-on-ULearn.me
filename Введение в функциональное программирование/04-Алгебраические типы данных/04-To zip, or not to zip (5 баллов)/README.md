# To zip, or not to zip?

Напишите функцию `zipMaybe`, которая принимает два значения типа `Maybe` и возвращает пару значений, если оба значения не `Nothing`.

```hs
- zipMaybe Nothing (Just 2) ~> Nothing
- zipMaybe (Just "hey") (Just 2) ~> Just ("hey", 2)
```


Все тесты пройдены, задача сдана:
```hs
zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe Nothing _ = Nothing
zipMaybe _ Nothing = Nothing
zipMaybe (Just x) (Just y) = Just (x, y)
```
