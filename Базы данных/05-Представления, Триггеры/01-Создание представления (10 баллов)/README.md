# Создание представления

Необходимо создать представление `scheduled_flights` на основе таблицы перелетов, которая будет выводить только запланированные рейсы (статус которых `Scheduled`), отсортированные по времени вылета по расписанию в прямом порядке и содержать поля:
- ID рейса
- номер рейса
- время вылета по расписанию
- аэропорт отправления
- аэропорт назначение

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
Create View scheduled_flights as
Select 
    flight_id,
    flight_no,
    scheduled_departure,
    departure_airport,
    arrival_airport
From flights
Where status = 'Scheduled'
Order by scheduled_departure;

Select *
From scheduled_flights
Where departure_airport = 'SVX'
Limit 3;
```
