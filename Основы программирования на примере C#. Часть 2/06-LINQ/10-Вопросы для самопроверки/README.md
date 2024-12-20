# Вопросы для самопроверки

1. Что такое LINQ и для чего он нужен?

**Ответ:**

LINQ — это встроенный в C# механизм для удобной работы с коллекциями.

Его можно использовать не только для манипуляций с коллекциями, но и для запросов к базам данных и другим внешним источникам данных. Например, LINQ2SQL — для запросов к SQL базам данных.

2. Из чего состоит LINQ?

**Ответ:**

В основе LINQ лежит интерфейс последовательности IEnumerable. Для IEnumerable в пространстве имен System.Linq определено множество полезных методов расширения, которые и образуют основу LINQ.

3. Что такое ленивый метод?

**Ответ:**

Метод который вместо результата вычисления возвращает объект, при обращении к которому вычисление только запустится.

4. Что такое Method Chaining и зачем он нужен?

**Ответ:**

Несколько последовательных методов можно объединять в одну цепочку вызовов, если каждый метод возвращает промежуточный результат.

Этот прием удобно использовать с Linq:

```cs
list.Where(i => i > 0).Select(i => 2*i).ToArray()
```

5. Что делает метод `Skip`? Является ли он ленивым методом?

**Ответ:**

Обрезает последовательность, пропуская указанное количество элементов. Является ленивым методом.

6. Что делают методы `ToList` и `ToArray`? Являются ли они ленивыми?

**Ответ:**

Преобразуют коллекцию в список или массив. Не являются ленивыми.

7. Что делает метод `SelectMany`? Является ли он ленивым?

**Ответ:**

Для каждого элемента исходной коллекции по переданному в нему правилу делает целую подколлекцию. А затем объединяет все такие подколлекции в одну.

8. Что делают методы `OrderBy` и `OrderByDescending`? Являются ли они ленивыми?

**Ответ:**

`OrderBy` сортирует коллекцию по возрастанию. `OrderByDescending` сортирует коллекцию по убыванию. Являются ленивыми.

*Сортировка по алфавиту от А до Я считается сортировкой по возрастанию.*

9. Что делают методы `ThenBy` и `ThenByDescending`? Являются ли они ленивыми?

**Ответ:**

`ThenBy` и `ThenByDescending` вызываются, если при сортировке коллекции встретились одинаковые по переданному в `OrderBy` или `OrderByDescending` лямбда-выражению элементы, и сортируют их по дополнительному критерию. `ThenBy` сортирует по возрастанию. `ThenByDescending` сортирует по убыванию. Являются ленивыми.

10. Что делает метод `Distinct`? Является ли он ленивым?

**Ответ:**

`Distinct` удаляет повторяющиеся элементы коллекции. Являются ленивыми.

11. Для чего нужен класс `Tuple`? Чем он полезен?

**Ответ:**

Класс кортежей, который объединяет несколько объектов разных типов в один объект.

```cs
var tuple = Tuple.Create("Павел", 40);
```

12. Что делают методы `Min` и `Max`? Являются ли они ленивыми?

**Ответ:**

`Min` возвращает минимальный элемент коллекции.<br>
`Max` возвращает максимальный элементы коллекции.<br>
Не являются ленивыми.

13. Что делает метод `Count`? В чем его особенность? Является ли он ленивым?

**Ответ:**

`Count` вычисляет количество элементов коллекции за **O(n)**. Его особенность заключается в том, что, благодаря оптимизации, он проверяют коллекцию на наличие реализации интерфейса `ICollection`, и если этот интерфейс реализован, возвращает Count за **O(1)**. Не является ленивым.

14. Что делают методы `All` и `Any`? Являются ли они ленивыми?

**Ответ:**

- `Any` проверяет, что хоть один элемент коллекции соответствующий предикату.
- `All` проверяет, что каждый элемент соответствует предикату.

Как и все методы, возвращающие конкретное значение (bool в этом случае), они не могут быть ленивыми.

15. Что делает метод `GroupBy`? Является ли он ленивым?

**Ответ:**

Группирует элементы коллекции по некоторому признаку и возвращает коллекцию групп.

Как и многие методы, возвращающие IEnumerable, он ленивый.

16. Что делают методы `ToDictionary` и `ToLookup`? В чем их отличие? Являются ли они ленивыми?

**Ответ:**

Как и все ToXXX методы, они не ленивые.
- `ToDictionary` создает из каждого элемента коллекции пару (ключ, значение), которую добавляет в результирующий словарь.
- `ToLookup` создает из каждого элемента коллекции пару (ключ, значение), которую добавляет в результирующий мульти-словарь. То есть такой словарь, в котором по ключу хранится список значений.

Кроме того, результат работы ToLookup не имеет методов модификации, а ToDictionary возвращает обычный Dictionary.
