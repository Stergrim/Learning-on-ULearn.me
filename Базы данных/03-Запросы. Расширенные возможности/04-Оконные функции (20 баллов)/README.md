# Оконные функции

 > Перед выполнением следующего задания внимательно изучи тему [оконные функции](https://postgrespro.ru/docs/postgresql/13/tutorial-window)

Для каждого перелета с начала текущего дня с кодом самолета '319' и '773' вывести накопленное на момет посадки количество перевезённых пассажиров с начала текущего дня. Отсортировать данные по коду самолета в алфавитном порядке, а затем по дате посадки в порядке убывания.

Формат вывода: Код самолета | Дата-время посадки | Накопленное количество пассажиров, перевезенных этим самолётом.

 > За текущее время нужно взять значение, возвращаемое функцией bookings.now()

**Все тесты пройдены, задача сдана:**
```pgsql
Select
aircraft_code as "Код самолета",
actual_arrival as "Дата-время посадки",
Sum(passenger_count) Over (
        Partition by aircraft_code 
        Order by actual_arrival
    ) as "Накопленное количество пассажиров, перевезенных этим самолётом"
From (
Select 
    aircraft_code,
    actual_arrival,
    Count(ticket_no) as passenger_count
From flights Left Join ticket_flights Using(flight_id)
Where
    aircraft_code In ('319', '773')
    And actual_arrival >= Date(bookings.now())
Group by aircraft_code, actual_arrival
)
Order by aircraft_code, actual_arrival Desc
```
