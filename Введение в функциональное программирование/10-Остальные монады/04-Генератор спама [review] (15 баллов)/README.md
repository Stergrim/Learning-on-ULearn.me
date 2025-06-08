# Генератор спама [review]

Требуется реализовать несколько функция для формирования набора рекламных писем по имеющейся базе данных. Для чтения информации о получателях используйте `Reader`.

Данные о получателях хранятся в значениях типа `Person`:

```hs
data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , surname :: String
  , firstName :: String
  , middleName :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)
```

Вы можете использовать любые функции из стандартных пакетов и модулей (которые не требуют дополнительной установки). Для этого просто подключите их с помощью `import`.

**Посмотреть формат писем и содержимое базы данных**

Для неженатого мужчины письмо должно иметь вид:

```hs
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
```

Для незамужней женщины:

```hs
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
```

Семейным парам нужно послать одна письмо вида:

```hs
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
```

В качестве "базы данных" используйте список получателей:

```hs
persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]
```


Все тесты пройдены, решение ожидает код-ревью:
```hs
{-# LANGUAGE LambdaCase #-}

module Spam where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

import Person
import DataBase

-- Поиск персоны по id
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do
  persons <- ask
  return $ find ((== pId) . id) persons

-- Формирует письмо для неженатого человека
processSingle :: Person -> String
processSingle p =
  (if sex p == Male then "Уважаемый " else "Уважаемая ") ++ firstName p ++ " " ++ middleName p ++ "!\n" ++
  "Разрешите предложить Вам наши услуги."

-- Формирует письмо для семейной пары
processPair :: Person -> Person -> String
processPair husband wife = concat
  [ "Уважаемые ", firstName husband, " ", middleName husband, " и "
  , firstName wife, " ", middleName wife, "!\n"
  , "Разрешите предложить вам наши услуги."
  ]

-- Формирует письмо для человека с personId
processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = findById pId >>= \case
  Nothing -> pure Nothing
  Just p -> case marriedBy p of
    Nothing -> return $ Just $ processSingle p
    Just marriedByP -> do
      p' <- fromJust <$> findById marriedByP
      return $ Just $ processPair p p'

-- Возвращает список сформированных писем по списку получателей
processPersons :: [PersonId] -> [Maybe String]
processPersons personIds = runReader (forM personIds processPerson) persons
```

Вы можете скачать [тесты для локального запуска](Spam.zip).
