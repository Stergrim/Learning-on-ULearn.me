# Проверка ошибок [review]

В это задании вам нужно переписать в do-notation функцию `normalize`:

```hs
normalize :: RegistrationRequest -> ValueOrError RegistrationRequest
```

Эта функция принимает запрос `RegistrationRequest`, проверяет его на корректность и приводит его поля в некоторую нормальную форму. Если запрос был корректным и нормализация прошла успешно, то она возвращает запрос с нормализованными полями, и ошибку — в противном случае.

Тип запроса, функции нормализации и функции проверок находится в модуле `RegistrationRequest`.

В качестве возвращаемого значения, которое позволяет вернуть либо ошибку, либо значение, функция `normalize` использует тип `ValueOrError`. По сути `ValueOrError` — самописный аналог `Either` и, конечно, `ValueOrError` — монада.

Используя то, что `ValueOrError` — монада, перепишите функцию `normalize` в do-notation. Использование `case-of`, guards, `if-then-else` и pattern matching по конструкторам `ValueOrError` запрещены.


Все тесты пройдены, решение ожидает код-ревью:
```hs
module ValueOrErrorMonad where

import ValueOrError
import RegistrationRequest

normalize :: RegistrationRequest -> ValueOrError RegistrationRequest
normalize (RegistrationRequest name surname age motto) = do
  normalizedName <- normalizeStringField name
  normalizedSurname <- normalizeStringField surname
  normalizedMotto <- normalizeStringField motto
  _ <- validateAge age
  _ <- checkUniqueMotto normalizedMotto
  return $ RegistrationRequest normalizedName normalizedSurname age normalizedMotto
```

Вы можете скачать [тесты для локального запуска](ValueOrErrorMonad.zip).
