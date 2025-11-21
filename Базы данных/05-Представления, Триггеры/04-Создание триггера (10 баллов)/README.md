# Создание триггера

Необходимо реализовать функциональность для уведомления приложения об изменении данных в таблице `flights`, для сброса кэша. Функция `notify_on_data_change` уже есть в БД, нужно только настроить ее вызов при любых модификациях таблицы `flights`.

```pgsql
-- Метод уведомления приложения
CREATE OR REPLACE FUNCTION notify_on_data_change() RETURNS trigger
AS $BODY$
BEGIN
    PERFORM pg_notify('data_changed', TG_TABLE_NAME);
    RAISE NOTICE 'Сообщение об изменении данных успешно отправлено';
    RETURN NEW;
END
$BODY$
LANGUAGE 'plpgsql';
```

**Все тесты пройдены, задача сдана:**
```pgsql
Create Trigger flights_change_trigger
    After Insert Or Update Or Delete On flights
    For Each Row
    Execute Function notify_on_data_change();
```
