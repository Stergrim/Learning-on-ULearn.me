# Рекомендательные блокировки

Посмотри в [статье из первого слайда](https://postgrespro.ru/docs/postgresql/13/explicit-locking#ADVISORY-LOCKS) и [здесь](https://postgrespro.ru/docs/postgresql/13/functions-admin#FUNCTIONS-ADVISORY-LOCKS) как управлять рекомендательными блокировками.


**Задание**
1. Получи исключительную рекомендательную блокировку сеансового уровня, **если она доступна** на максимальный номер билета (не используй подзапросы) в таблице tickets.
2. Освободи все блокировки сеансного уровня

**Примечание**

Функции рекомендуемых блокировок принимают параметром bigint. Используй `::bigint` для преобразования максимального номера билета в bigint.

**Все тесты пройдены, задача сдана:**
```pgsql
Select pg_try_advisory_lock(Max(ticket_no)::bigint)
From tickets;

Select pg_advisory_unlock_all();
```
