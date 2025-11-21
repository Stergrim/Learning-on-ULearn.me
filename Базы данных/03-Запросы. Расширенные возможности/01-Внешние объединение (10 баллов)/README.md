# Внешние объединение

Получите номера 5 ближайших не задержанных рейсов, доступные для регистрации и где есть непришедшие на рейс пассажиры.

Можно считать, что пассажир не пришел на рейс, если у него нет посадочного талона.

**Все тесты пройдены, задача сдана:**
```pgsql
Select flight_no
From flights
Join ticket_flights Using(flight_id)
Left Outer Join boarding_passes as bp Using(ticket_no, flight_id)
Where status = 'On Time'
And bp.ticket_no Is Null
Group by flight_no, scheduled_departure
Order by scheduled_departure
Limit 5
```
