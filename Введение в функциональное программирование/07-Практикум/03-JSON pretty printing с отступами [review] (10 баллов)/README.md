# JSON pretty printing с отступами [review]

В этом задании требуется добавить поддержку отступов в решение предыдущей задачи.

**Посмотреть формат вывода**

```hs
> putStrLn $ prettyPrint john
{
  "name": "John",
  "age": 30,
  "temperature": 36.6,
  "car": null,
  "hasCat": true,
  "isMarried": false
}
> putStrLn $ prettyPrint todoList
[
  "Wake up",
  "Write Haskell",
  "Go to bed"
]
> putStrLn $ prettyPrint menuWidget
{
  "menu": {
    "id": "file",
    "value": "File",
    "popup": {
      "menuitem": [
        {
          "value": "New",
          "onclick": "CreateNewDoc()"
        },
        {
          "value": "Open",
          "onclick": "OpenDoc()"
        },
        {
          "value": "Close",
          "onclick": "CloseDoc()"
        }
      ]
    }
  }
}
```

Обратите внимание, что сама по себе `prettyPrint` возвращает строку с символами переноса строки. Чтобы напечатать их необходимо использовать `putStrLn`.


Все тесты пройдены, решение ожидает код-ревью:
```hs
module JsonPrettyPrintIndent where

import Data.List

data JSON = JNull
  | JBool Bool
  | JString String
  | JNumber Double
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Eq, Show)

prettyPrint :: JSON -> String
prettyPrint = pp 0
  where
    pp :: Int -> JSON -> String
    pp _ JNull = "null"
    pp _ (JBool True) = "true"
    pp _ (JBool False) = "false"
    pp _ (JString s) = show s
    pp _ (JNumber n)
      | n == fromInteger (round n) = show (round n)
      | otherwise = show n
    pp indent (JArray []) = "[]"
    pp indent (JArray arr) = 
      "[\n" ++ intercalate ",\n" (map (indentStr (indent + 2) . pp (indent + 2)) arr) ++ 
      "\n" ++ indentStr indent "]"
    pp indent (JObject []) = "{}"
    pp indent (JObject obj) = 
      "{\n" ++ intercalate ",\n" (map (ppPair (indent + 2)) obj) ++ 
      "\n" ++ indentStr indent "}"
    
    ppPair :: Int -> (String, JSON) -> String
    ppPair indent (key, val) = 
      indentStr indent (show key ++ ": " ++ pp indent val)
    
    indentStr :: Int -> String -> String
    indentStr n s = replicate n ' ' ++ s

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

Вы можете скачать [тесты для локального запуска](JsonPrettyPrintIndent.zip).
