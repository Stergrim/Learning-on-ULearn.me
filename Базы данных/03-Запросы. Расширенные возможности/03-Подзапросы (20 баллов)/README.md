# Подзапросы

 > Перед выполнением этого задания будет полезно ознакомиться с [условными выражениями](https://postgrespro.ru/docs/postgresql/9.5/functions-conditional)

Найти номера рейсов всех самолетов отсортированные в алфавитном порядке, которые:
1. в данный момент находятся в воздухе
2. с максимальной вместимостью 30 человек и более
3. заполнены менее чем на 25% включительно

Формат вывода: Номер рейса | Код самолета | Вместимость самолета | К-во пассажиров.

**Все тесты пройдены, задача сдана:**
```pgsql
Select
flight_no as "Номер рейса",
aircraft_code as "Код самолета",
capacity as "Вместимость самолета",
Count(ticket_no) as "К-во пассажиров"
From
flights
Join (Select
      aircraft_code,
      Count(*) as capacity
      From seats
      Group by aircraft_code) Using(aircraft_code)
Left Join ticket_flights Using(flight_id)
Where
    status = 'Departed'
    And capacity >= 30
Group by
    flight_no, 
    aircraft_code, 
    capacity
Having (Count(ticket_no)::decimal / capacity) <= 0.25
Order by flight_no
```
