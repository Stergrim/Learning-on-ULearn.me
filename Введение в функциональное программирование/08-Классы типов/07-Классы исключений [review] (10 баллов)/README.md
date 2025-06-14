# Классы исключений [review]

В этом задании вам необходимо придумать и написать иерархию исключений с помощью классов типов на основе набора требований:
1. Базовый класс `Exception` с возможностью получения сообщения ошибки
2. Класс для ошибок API. Должен уметь возвращать
   - ошибку в формате `JSON`
   - уровень severity (debug, info, error, warn)
3. Класс для ошибок при работе с базой данных.
   - дополнительно возвращает сообщение об ошибке от базы данных
4. Класс для ошибок доменной логики
   - дополнительно возвращает контекст с данными

Нужно только объявить классы типов и все используемые в них типы (если вам нужны новые типы).


Все тесты пройдены, решение ожидает код-ревью:
```hs
-- Базовый тип для уровня серьезности ошибки
data Severity = Debug | Info | Warn | Error
  deriving (Show, Eq)

-- Базовый класс исключений с возможностью получения сообщения
class Exception e where
  message :: e -> String

-- Класс для ошибок API
class Exception e => ApiError e where
  toJson :: e -> String  -- Возвращает ошибку в формате JSON
  severity :: e -> Severity  -- Уровень серьезности

-- Класс для ошибок базы данных
class Exception e => DatabaseError e where
  dbMessage :: e -> String  -- Сообщение об ошибке от базы данных

-- Класс для ошибок доменной логики
class Exception e => DomainError e where
  context :: e -> String  -- Контекст с данными
```
