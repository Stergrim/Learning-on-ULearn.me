# Обновление материализованного представления

В задании "Материализованное представление" мы создали 'aircraft_traffic', но не настроили его обновление. Давайте настроим его при любых модификациях таблицы `flights`.

**Все тесты пройдены, задача сдана:**
```pgsql
Create Or Replace Function refresh_aircraft_traffic()
Returns Trigger as $$
Begin
    Refresh MAterialized View aircraft_traffic;
    Raise Notice 'Обновлено';
    Return Null;
End;
$$ Language plpgsql;

Create Trigger refresh_aircraft_traffic_trigger
    After Insert Or Update Or Delete On flights
    For Each Row
    Execute Function refresh_aircraft_traffic();
```
