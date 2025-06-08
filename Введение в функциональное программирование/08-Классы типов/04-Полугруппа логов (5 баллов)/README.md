# Полугруппа логов

Реализуйте instance `Semigroup` для типа для логгирования с помощью операции конкатенации:

```hs
> LogEntry "Message," <> LogEntry "AnotherMessage"
LogEntry {unLogEntry = "Message,AnotherMessage"}
```

```hs
newtype LogEntry = LogEntry { unLogEntry :: String } deriving (Eq, Show)
```


Все тесты пройдены, задача сдана:
```hs
instance Semigroup LogEntry where
    (LogEntry a) <> (LogEntry b) = LogEntry (a ++ b)
```
