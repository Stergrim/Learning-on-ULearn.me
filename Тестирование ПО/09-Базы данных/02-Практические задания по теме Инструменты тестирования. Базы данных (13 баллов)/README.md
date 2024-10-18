# Практические задания по теме: Инструменты тестирования. Базы данных

Следующие задания нужно выполнять на базе данных w3schools в браузере Chrome, Opera или Safari:<br>
https://www.w3schools.com/sql/trysql.asp?filename=trysql_select_all

*В заданиях, где в качестве ответа нужно написать SQL-запрос, ответ не должен содержать квадратных скобок, [псевдонимов](https://www.schoolsw3.com/sql/sql_alias.php), оператора USE и точки с запятой в конце запроса

⚠️Если при выполнении задания (или при самостоятельном знакомстве) ты меняешь данные в базе, то обязательно нужно восстановить базу, нажав на кнопку "Restore Database"

**Пример ответа: SELECT field FROM table WHERE field='something'**

1. Напиши запрос, который из таблицы Products отобразит все наименования продуктов (ProductName), цена которых равна 15: (1 из 1 балла)
   * 🟢 `SELECT ProductName FROM Products WHERE Price = 15` (Правильный ответ: SELECT ProductName FROM Products WHERE Price = 15)


2. Напиши CustomerName, который заказал продукт Laughing Lumberjack Lager: (2 из 2 балла)
   * 🟢 `Lehmanns Marktstand` (Правильный ответ: Lehmanns Marktstand. Пример запроса: SELECT CustomerName, ProductName FROM Customers INNER JOIN Orders USING(CustomerID) INNER JOIN OrderDetails USING(OrderID) INNER JOIN Products USING(ProductID) WHERE ProductName = 'Laughing Lumberjack Lager')


3. Напиши запрос, который добавит в таблицу Categories новую категорию с названием test, идентификатором 777 и описанием test (1 из 1 балла)
   * 🟢 `INSERT INTO Categories (CategoryID,CategoryName,Description) VALUES (777,'test','test')` (Правильный ответ: Примеры запросов: 1. INSERT INTO Categories VALUES(777,'test','test') 2. INSERT INTO Categories(CategoryID,CategoryName,Description) VALUES(777,'test','test'))


4. Напиши общее количество товаров (Quantity) из таблицы OrderDetails, у которых идентификатор заказа (OrderID) находится между 10299 и 10362 включительно: (1 из 1 балла)
   * 🟢 `4064` (Правильный ответ: 4064. Пример запроса: SELECT Sum(Quantity) FROM OrderDetails WHERE OrderID Between '10299' and '10362')


5. Напиши для таблицы Suppliers запрос, который для поставщиков, телефон которых заканчивается на 2955, изменит город на значение test (2 из 2 балла)
   * 🟢 `UPDATE Suppliers SET City = 'test' WHERE Phone LIKE '%2955'` (Правильный ответ: UPDATE Suppliers SET City = 'test' WHERE phone LIKE '%2955')


6. Напиши количество уникальных городов (City) из таблицы Customers: (2 из 2 балла)
   * 🟢 `69` (Правильный ответ: 69. Пример запроса: SELECT Count(City) FROM (SELECT Distinct City FROM customers))


7. Напиши запрос, который из таблицы Customers удалит все строки, у которых город начинается с буквы L (1 из 1 балла)
   * 🟢 `DELETE FROM Customers WHERE City LIKE 'L%'` (Правильный ответ: DELETE FROM Customers WHERE City LIKE 'L%')


8. Среди всех заказов (OrderID) из таблицы OrderDetails определи тот, в котором находится больше всего товаров (Quantity). В одном заказе может быть несколько товаров. В поле ввода, через запятую, напиши OrderID заказа и количество товаров (Quantity) в этом заказе: (3 из 3 балла)
   * 🟢 `10324,241` (Правильный ответ: 10324, 241. Пример запроса: SELECT OrderID, MAX(QuantitySum) FROM (SELECT OrderID, SUM(Quantity) as QuantitySum FROM OrderDetails GROUP BY OrderID))
