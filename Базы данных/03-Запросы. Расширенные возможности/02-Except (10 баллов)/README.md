# Except

В какие города нельзя добраться на самолете ни из Санкт-Петербурга, ни из Москвы?

**Все тесты пройдены, задача сдана:**
```pgsql
Select Distinct city
From airports

Except

Select Distinct arrival.city
From flights as f
Join airports as departure On f.departure_airport = departure.airport_code
Join airports as arrival On f.arrival_airport = arrival.airport_code
Where departure.city In ('Москва', 'Санкт-Петербург')
```
