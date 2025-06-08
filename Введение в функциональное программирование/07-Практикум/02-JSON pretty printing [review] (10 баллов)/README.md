# JSON pretty printing [review]

Вам необходимо определить ADT `JSON` для описания значений в формате [JSON](https://www.json.org/json-en.html) и функцию `prettyPrint :: JSON -> String`, которая сериализует значение типа `JSON` в одну строку.

Ваш тип должен поддерживать:
- null
- булевы значения
- строки
- объекты
- числа (целые и дробные)
- списки

Чтобы мы могли проверить ваше решения автоматически, вам нужно с помощью вашего типа определить три значения: `john :: JSON`, `todoList :: JSON` и `menuWidget :: JSON`, которые должны отображаться с помощью `prettyPrint` следующим образом:

```hs
> putStrLn $ prettyPrint john
{ "name": "John", "age": 30, "temperature": 36.6, "car": null, "hasCat": true, "isMarried": false }

> putStrLn $ prettyPrint todoList
[ "Wake up", "Write Haskell", "Go to bed" ]

> putStrLn $ prettyPrint menuWidget
{ "menu": { "id": "file", "value": "File", "popup": { "menuitem": [ { "value": "New", "onclick": "CreateNewDoc()" }, { "value": "Open", "onclick": "OpenDoc()" }, { "value": "Close", "onclick": "CloseDoc()" } ] } } }
```

Обратите внимание, что сама по себе `prettyPrint` возвращает строку с символами переноса строки. Чтобы напечатать их необходимо использовать `putStrLn`.


Все тесты пройдены, решение ожидает код-ревью:
```hs
module JsonPrettyPrint where

import Data.List

data JSON = JNull
          | JBool Bool
          | JString String
          | JNumber Double
          | JArray [JSON]
          | JObject [(String, JSON)]
          deriving (Eq, Show)

prettyPrint :: JSON -> String
prettyPrint JNull = "null"
prettyPrint (JBool True) = "true"
prettyPrint (JBool False) = "false"
prettyPrint (JString s) = "\"" ++ s ++ "\""
prettyPrint (JNumber n) = 
  if fromIntegral (round n) == n
    then show (round n)
    else show n
prettyPrint (JArray xs) = "[ " ++ intercalate ", " (map prettyPrint xs) ++ " ]"
prettyPrint (JObject fields) = 
  "{ " ++ intercalate ", " (map printField fields) ++ " }"
  where
    printField (k, v) = "\"" ++ k ++ "\": " ++ prettyPrint v

john :: JSON
john = JObject [
  ("name", JString "John"),
  ("age", JNumber 30),
  ("temperature", JNumber 36.6),
  ("car", JNull),
  ("hasCat", JBool True),
  ("isMarried", JBool False)
  ]

todoList :: JSON
todoList = JArray [
  JString "Wake up",
  JString "Write Haskell",
  JString "Go to bed"
  ]

menuWidget :: JSON
menuWidget = JObject [
  ("menu", JObject [
    ("id", JString "file"),
    ("value", JString "File"),
    ("popup", JObject [
      ("menuitem", JArray [
        JObject [("value", JString "New"), ("onclick", JString "CreateNewDoc()")],
        JObject [("value", JString "Open"), ("onclick", JString "OpenDoc()")],
        JObject [("value", JString "Close"), ("onclick", JString "CloseDoc()")]
        ])
      ])
    ])
  ]
```

Вы можете скачать [тесты для локального запуска](JsonPrettyPrint.zip).
