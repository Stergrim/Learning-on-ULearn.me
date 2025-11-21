# Неизменяемые данные в транзакции Read Committed

Ваша транзакция выполняется на уровне изоляции `READ COMMITTED`. Сделай так, чтобы на протяжении всей транзакции можно было получать неизменяемые данные из таблицы boarding_passes.

**Все тесты пройдены, задача сдана:**
```pgsql
Begin Transaction Isolation Level Read Committed;

Lock Table boarding_passes In Share Mode;

Select * From boarding_passes Order by ticket_no Limit 10;

Commit;
```
