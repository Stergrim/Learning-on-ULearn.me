# Sum

Какой доход компании принес рейс PG0405 от 14 августа 2017? Все нужные данные можно найти в таблицах `flights` и `ticket_flights`.

**Все тесты пройдены, задача сдана:**
```pgsql
Select Sum(amount)
From flights Join ticket_flights Using (flight_id)
Where flight_no = 'PG0405'
And scheduled_departure
Between '2017-08-14 00:00:00'::timestamp And '2017-08-15 00:00:00'::timestamp
```
