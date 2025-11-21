# Материализованное представление

Необходимо создать представление `aircraft_traffic`, в котором для каждого аэропорта (`arrival_airport`) будет информация об общем количестве принятых им самолетов (статус которых `Arrived`). Столбец с аэропортом нужно назвать `airport_code` — обрати внимание, что это можно сделать разными способами.

Ожидается очень большая нагрузка на чтение к этому представлению, поэтому предлагается сделать его материализованным. Обновление данных в таблице будет организовано чуть позже.

Таблица перелетов **flights**

| **Столбец**         | **Тип**     | **Модификаторы** | **Описание**                |
|:--------------------|:------------|:-----------------|:----------------------------|
| flight_id           | serial      | not null         | Идентификатор рейса         |
| flight_no           | char(6)     | not null         | Номер рейса                 |
| scheduled_departure | timestamptz | not null         | Время вылета по расписанию  |
| scheduled_arrival   | timestamptz | not null         | Время прилёта по расписанию |
| departure_airport   | char(3)     | not null         | Аэропорт отправления        |
| arrival_airport     | char(3)     | not null         | Аэропорт прибытия           |
| status              | varchar(20) | not null         | Статус рейса                |
| aircraft_code       | char(3)     | not null         | Код самолета, IATA          |
| actual_departure    | timestamptz |                  | Фактическое время вылета    |
| actual_arrival      | timestamptz |                  | Фактическое время прилёта   |

**Все тесты пройдены, задача сдана:**
```pgsql
Create Materialized View aircraft_traffic as
Select 
    arrival_airport as airport_code,
    Count(*) as arrived_flights_count
From flights
Where status = 'Arrived'
Group by arrival_airport;

Select *
From aircraft_traffic
Where airport_code = 'DME';
```
