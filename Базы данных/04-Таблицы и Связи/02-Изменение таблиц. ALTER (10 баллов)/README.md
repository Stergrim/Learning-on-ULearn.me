# Изменение таблиц. ALTER

Оказалось, что иногда нужно удалять статьи. Чтобы избежать потери данных, вы решили просто добавить статус «Удалён» у существующего поля `published_status`.

| **Описание**  | **Название**        | **Тип** | **Обязательность** | **Комментарий**                 |
|:--------------|:--------------------|:--------|:-------------------|:--------------------------------|
| Опубликовано  | published_status    | Число   | +                  | По умолчанию — Не опубликовано. |

Варианты значений:
- −1 — удалёно
- 0 — не опубликовано
- 1 — опубликовано

**Задание**
1. [Освежи знания по документации](https://postgrespro.ru/docs/postgresql/13/ddl-alter).
2. [Посмотри синтаксис команды ALTER TABLE](https://postgrespro.ru/docs/postgresql/13/sql-altertable).
3. Измени тип колонки `published` так, чтобы в нём можно было хранить 3 разных статуса. Не потеряйте данные в текущей таблице.
4. Добавь ограничение с названием `published_values` на возможные значения.
5. Переименуй колонку.
6. Получи все статьи, отсортированные по ID. Обрати внимание на данные в колонке со статусом.

**Контрольные вопросы**
- Почему в задании предложено для статуса «Удалён» значение -1?
- Какие есть способы не потерять данные при изменении типа колонок?
- Чем плохо было название колонки published?

**Все тесты пройдены, задача сдана:**
```sql
-- Сначала удаляем значение по умолчанию
Alter Table articles
Alter Column published Drop Default;

-- Изменяем тип колонки published с Boolean на Integer
Alter Table articles 
Alter Column published Type Integer 
Using published::integer;

-- Устанавливаем значение по умолчанию
Alter Table articles 
Alter Column published Set Default 0;

-- Добавляем ограничение
Alter Table articles 
Add Constraint published_values 
Check (published In (-1, 0, 1));

-- Переименовываем
Alter Table articles 
Rename Column published To published_status;

-- Получаем все статьи
Select * From articles Order by article_id;
```
