# Distinct

Сколько различных номеров рейсов принял аэропорт "Домодедово" за последние 7 дней?
 > За текущее время нужно взять значение, возвращаемое функцией bookings.now()

**Все тесты пройдены, задача сдана:**
```pgsql
Select Count(Distinct flight_no)
From flights
Where arrival_airport = 'DME'
And Status = 'Arrived'
And scheduled_arrival
Between bookings.now() - Interval '7 day' And bookings.now()
```
