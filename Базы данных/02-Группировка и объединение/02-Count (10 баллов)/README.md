# Count

Сколько самолетов принял аэропорт "Пулково" за последние 30 дней?
 > За текущее время нужно взять значение, возвращаемое функцией bookings.now()

**Все тесты пройдены, задача сдана:**
```pgsql
Select Count(*)
From flights
Where arrival_airport = 'LED'
And Status = 'Arrived'
And scheduled_arrival
Between bookings.now() - Interval '30 day' And bookings.now()
```
