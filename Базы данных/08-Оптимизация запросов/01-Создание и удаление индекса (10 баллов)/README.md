# Создание и удаление индекса

**Пример создания индекса**

`CREATE INDEX table_f1_f2_index ON table (f1, f2);`

Индекс можно делать сразу на несколько колонок.

Создание индекса может занимать значительное время. Индекс добавляет накладные расходы на его поддержание - замедляет операции изменения и удаления записей, увеличивают размер БД.

**Пример удаления индекса**

`DROP INDEX table_f1_f2_index;`

**Задание**
1. В таблице `flights` создай общий индекс на колонки `status` и `actual_arrival` с названием `flights_status_actual_arrival_index`.
2. Удали индекс.

**Все тесты пройдены, задача сдана:**
```pgsql
Create Index flights_status_actual_arrival_index On flights (status, actual_arrival);
Drop Index flights_status_actual_arrival_index;
```
