# Quiz

1. Что вернет база данных MySQL, если в запрос SELECT * FROM Users WHERE User='{X}' вместо {X} пользователь введет строку «' OR 1=1 --» (без внешних кавычек)? (1 из 1 балла)
   * 🔴 **Ошибка «Неверный синтаксис»**
   * 🔴 **Пустую таблицу**
   * 🟢 **Все строки таблицы Users** (Правильно!)


2. Таблица «Имена» содержит два поля: строка «Фамилия» и строка «Имя». В таблице пять строк. Таблица «Логины» содержит два поля: строка «Фамилия» и строка «Логин». В этой таблице десять строк. Какой результат будет у запроса (SELECT * FROM Имена) UNION (SELECT * FROM Логины)? (1 из 1 балла)
   * 🔴 **Ошибка «Невозможно преобразовать поле “Логин” к полю “Имя”»**
   * 🔴 **Таблица из 5 строк и 4 столбцов**
   * 🟢 **Таблица из 15 строк и 2 столбцов** (Правильно!)
   * 🔴 **Таблица из 50 строк и 2 столбцов.**


3. Если вы знаете, что в таблице не больше 100 столбцов, то за какое минимальное количество запросов вы сможете гарантированно определить количество столбцов в ней? (1 из 1 балла)
   * 🔴 **1**
   * 🟢 **10** (Правильно!)
   * 🔴 **50**
   * 🔴 **100**


4. Когда используется слепая SQL-инъекция? (1 из 1 балла)
   * 🟢 **Мы не видим описание ошибки базы данных** (Правильно!)
   * 🔴 **Сервер фильтрует все пользовательские данные при подстановке в запросы к базе данных**
   * 🔴 **Мы не знаем, где находится база данных**
   * 🔴 **Мы не знаем, какие данные нам нужно достать из базы данных**

   
5. В каких языках возможна Инъекция? (1 из 1 балла)
   * ✅ **CSS** (Правильно!)
   * ✅ **XML** (Правильно!)
   * ✅ **XPath** (Правильно!)
   * ✅ **SQL** (Правильно!)


6. Если SQL-сервер фильтрует символы звездочка (*), кавычки (“ и ‘) и все пробельные символы, то сможем ли мы сделать инъекцию? (1 из 1 балла)
   * 🟢 **Да** (Правильно!)
   * 🔴 **Нет**
