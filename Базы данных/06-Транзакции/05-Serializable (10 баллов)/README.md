# Serializable

Нужно получить номера бронирования, полная сумма которых больше 1 100 000. Важно избежать аномалию только читающей транзакции. Для этого используй `Serializable` изоляцию. Также тебе явно нужно объявить транзакцию только читающей и откладываемой.

Таблица bookings

| **Столбец**  | **Тип**       | **Модификаторы** | **Описание**              |
|:-------------|:--------------|:-----------------|:--------------------------|
| book_ref     | char(6)       | not null         | Номер бронирования        |
| book_date    | timestamptz   | not null         | Дата и время бронирования |
| total_amount | numeric(10,2) | not null         | Полная сумма бронирования |

**Все тесты пройдены, задача сдана:**
```pgsql
Begin Transaction Isolation Level Serializable Read Only Deferrable;

Select book_ref
From bookings
Where total_amount > 1100000;

Commit;
```
