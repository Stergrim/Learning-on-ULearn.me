# Перечисления. ENUM

В PostgreSQL есть специальный тип для хранения перечислений. Применяя его не нужно отдельно создавать ограничение на варианты значений. Давайте создадим перечисление и применим для колонки published_status.

| **Описание**  | **Название**     | **Тип**                                     | **Обязательность** | **Комментарий**              |
|:--------------|:-----------------|:--------------------------------------------|:-------------------|:-----------------------------|
| Опубликовано  | published_status | Варианты: deleted, not_published, published | +                  | По умолчанию - not_published |

**Задание**
1. [Освежи знания по документации](https://postgrespro.ru/docs/postgresql/13/datatype-enum).
2. Удали старое ограничение `published_values`, которое создали в прошлом задании.
3. Создай перечисление `published_values` с перечисленными значениями.
4. Измени тип колонки `published_status` на только что созданное перечисление. Не потеряй данные в текущей таблице.
5. Получи все статьи, отсортированные по ID. Обрати внимание на данные в колонке со статусом.

**Контрольные вопросы**
- Чем решение с Enum лучше решения с Int?
- А чем хуже?
- Значения Enum’ов строковые. Значения повторяются в каждой строке таблицы. Есть ли проблема с увеличением размера таблицы из-за этого?
- Можно ли сортировать по колонкам с Enum и как будут отсортированы значения?
- Если в двух разных перечислениях есть одинаковые значения можно ли их сравнивать?

**Все тесты пройдены, задача сдана:**
```pgsql
-- Удаляем старое ограничение
Alter Table articles Drop Constraint If Exists published_values;

-- Создаем тип перечисления
Create Type published_values as Enum ('deleted', 'not_published', 'published');

-- Удаляем значение по умолчанию
Alter Table articles 
Alter Column published_status Drop Default;

-- Изменяем тип колонки с преобразованием данных
Alter Table articles
Alter Column published_status Type published_values 
Using (
    Case published_status
        When -1 Then 'deleted'
        When 0 Then 'not_published'
        Else 'published'
    End
)::published_values;

-- Устанавливаем значение по умолчанию
Alter Table articles 
Alter Column published_status Set Default 'not_published'::published_values;

-- Проверяем результат
Select * From articles Order by article_id;
```
