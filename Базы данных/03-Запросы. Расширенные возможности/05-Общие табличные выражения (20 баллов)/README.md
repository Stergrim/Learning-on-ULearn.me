# Общие табличные выражения

 > Познакомься с типом данных "Массив" в [документации](https://postgrespro.ru/docs/postgresql/13/arrays) и в главе 4.5 книги [PostgreSQL. Основы языка SQL](https://edu.postgrespro.ru/sql_primer.pdf)
 > Разберись как пользоваться общими табличными выражениями в [документации](https://postgrespro.ru/docs/postgrespro/13/queries-with) и в главе 6.4 [книги](https://edu.postgrespro.ru/sql_primer.pdf)

Требуется найти маршрут с минимальным количеством пересадок из Ижевска в Стрежевой.

Формат вывода: Массив кодов аэропортов в порядке следования по маршруту.

**Все тесты пройдены, задача сдана:**
```pgsql
With Recursive flight_routes as (
Select
    Array[airport_code] as route,
    airport_code as current_airport,
    city as current_city,
    0 as transfers
From airports_data
Where city->>'ru' = 'Ижевск'

Union All

Select Distinct On (airport_code)
    (route || airport_code)::Char(3)[],
    airport_code,
    city,
    transfers + 1
From
flight_routes as fr
Join flights as f On f.departure_airport = fr.current_airport
Join airports_data as a On f.arrival_airport = a.airport_code
Where 
    airport_code != All(route)
    And current_city->>'ru' != 'Стрежевой'
)
Select
    route as "Массив кодов аэропортов в порядке следования по маршруту"
From flight_routes
Where current_city->>'ru' = 'Стрежевой'
Order by transfers
Limit 1
```
