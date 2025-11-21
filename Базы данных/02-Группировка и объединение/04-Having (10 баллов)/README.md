# Having

Требуется найти первые 3 (по алфавиту) номера брони, по которым оформили 5 и более билетов.

**Все тесты пройдены, задача сдана:**
```pgsql
Select book_ref
From tickets
Group by book_ref
Having Count(*) > 4
Order by book_ref
Limit 3
```
