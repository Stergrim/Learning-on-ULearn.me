# Создание функции

Необходимо создать метод `is_email_valid` для проверки корректности введенного Email адреса. Возвращаемый тип функции - `bool`.

Адрес считается корректным если:
- в нем встречается ровно один раз символ `@`
- до и после `@` есть хотя бы один произвольный символ

```pgsql
-- Пример использования функции
select is_email_valid("pushkin@kontur.ru");
```

**Все тесты пройдены, задача сдана:**
```pgsql
Create Function is_email_valid(email_text Text)
Returns Boolean as $$
Begin
    Return email_text Is Not Null And email_text ~ '^[^@]+@[^@]+$';
End;
$$ Language plpgsql;

Select is_email_valid('pushkin@kontur.ru');
```
