# Group

Как называется и сколько содержит мест самый вместительный самолёт?

**Формат вывода:** Название самолета | К-во мест.

Все нужные данные можно найти в представлении `aircrafts` и в `таблицеseats`.

**Все тесты пройдены, задача сдана:**
```pgsql
Select model->>'ru', Count(*) as seats
From aircrafts_data Join seats Using (aircraft_code)
Group by aircraft_code
Order by seats Desc
Limit 1
```
