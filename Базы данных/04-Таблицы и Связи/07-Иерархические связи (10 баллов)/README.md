# Иерархические связи

Редактор попросил сделать в журнале комментарии к статьям произвольной вложенности, то есть комментарии, комментарии к комментариям и т.д. Для хранения комментариев нужно сделать таблицу `article_comments`. Для реализации иерархии, в таблице должна быть колонка, ссылающаяся на эту же таблицу.

Надо запрещать удалять родительский комментарий, если есть дочерние. При удалении статьи, можно удалить все связанные с ней комментарии.

**Описание таблицы**

| **Описание**             | **Название**       | **Тип**      | **Обязательность** | **Комментарий**                  |
|:-------------------------|:-------------------|:-------------|:-------------------|:---------------------------------|
| ID                       | article_comment_id | Целое число  | +                  | Первичный ключ с автоинкрементом |
| Родительский комментарий | parent_id          | Целое число  | -                  | Ссылка на таблицу комментариев   |
| Статья                   | article_id         | Целое число  | +                  | Ссылка на таблицу статей         |
| Текст                    | text               | Текст        | +                  |                                  |
| Подпись                  | author             | Строка       | +                  | Не более 50 символов             |
| Дата создания            | created_date       | Дата и время | +                  | По умолчанию - текущее время     |

**Задание**
1. Создай таблицу, для хранения комментариев.
2. Настрой связь со статьями, с учётом требований.
3. Настрой связь с родительским комментарием, с учётом требований.
4. Напиши запрос на выборку двух уровней дерева комментариев для статьи с ID=1.<br> **Формат вывода:** ID комментария | ID родительского комментария | Текст | Подпись

**Тестовые данные**

```pgsql
INSERT INTO article_comments (article_comment_id, parent_id, article_id, text, author)
VALUES
    (1, null, 1, 'Очень хорошо написано.', 'Пушкин'),
    (2, null, 2, 'Хорошо написано.', 'Пушкин'),
    (3, 1, 1, 'Не соглашусь.', 'Дантес'),
    (4, 3, 1, 'Конфликт не избежен.', 'Пушкин');
```

**Все тесты пройдены, задача сдана:**
```pgsql
-- Создаем таблицу для хранения комментариев
Create Table article_comments (
    article_comment_id Serial Primary Key,
    parent_id Integer,
    article_id Integer Not Null,
    text Text Not Null,
    author Varchar(50) Not Null,
    created_date Timestamp Not Null Default Current_Timestamp
);

-- Связь с таблицей статей: каскадное удаление при удалении статьи
Alter Table article_comments 
Add Constraint fk_article_comments_articles 
Foreign Key (article_id) 
References articles(article_id) 
On Delete Cascade;

-- Связь с родительским комментарием: запрет удаления при наличии дочерних
Alter Table article_comments 
Add Constraint fk_article_comments_parent 
Foreign Key (parent_id) 
References article_comments(article_comment_id) 
On Delete Restrict;

-- Вставляем тестовые данные
Insert Into article_comments (article_comment_id, parent_id, article_id, text, author)
Values
    (1, null, 1, 'Очень хорошо написано.', 'Пушкин'),
    (2, null, 2, 'Хорошо написано.', 'Пушкин'),
    (3, 1, 1, 'Не соглашусь.', 'Дантес'),
    (4, 3, 1, 'Конфликт не избежен.', 'Пушкин');

-- Выборка комментариев первого уровня и прямых ответов на них
Select
    article_comment_id as "ID комментария",
    parent_id as "ID родительского комментария", 
    text as "Текст",
    author as "Подпись"
From article_comments 
Where article_id = 1 And (parent_id Is Null Or parent_id = 1)
Order by article_comment_id;
```
