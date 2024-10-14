# Вопросы для самопроверки

1. Возможно ли сложить разные лямбда выражения в лист?

**Ответ:**

Да, возможно.

Пример:

```cs
var list = new List<Action<int>>();

list.Add((x) => Console.WriteLine(x));
```

2. Что делает функция `Where` встроенного языка `Linq`?

**Ответ:**

Фильтрует коллекцию по заданному сравнивателю. Если сравниватель вернул `false`, то элемент коллекции не пропускается в результирующую коллекцию.

Пример:
```cs
var numbers = new List<int> { 1, 2, 3, 4 };
var evenNumbers = numbers
    .Where(num => num % 2 == 0)
    .ToArray();
```

В `evenNumbers` будет массив из четных чисел: `2, 4`.

Пример реализации `Where`:

```cs
public static IEnumerable<T> Where<T> (IEnumerable<T> source, Func<T, bool> predicate)
{
    foreach (var item in source)
    {
        if (predicate(item))
            yield return item;
    }
}
```

3. Что делает функция `Select` встроенного языка `Linq`?

**Ответ:**

Применяет к элементу коллекции переданную функцию и возвращает её результат. В итоге получается преобразование элемента коллекции в новый элемент.

Пример:

```cs
var numbers = new List<int> { 2, 4, 6, 8 };
var halfNumbers = numbers
    .Select(num => num / 2)
    .ToArray();
```

В `halfNumbers` будут храниться числа деленные на 2: `1, 2, 3, 4`.

Пример реализации `Select`:

```cs
public static IEnumerable<T> Select<T> (IEnumerable<T> source, Func<T, T> selector)
{
    foreach (var item in source)
    {
        yield return selector(item);
    }
}
```

4. Что делает функция `FirstOrDefault` встроенного языка `Linq`? И чем она отличается от функции `First`?

**Ответ:**

`FirstOrDefault` берет первый элемент коллекции и возвращает его. Если коллекция пуста, то возвращает *default-значение* типа коллекции. Отличие от функции `First` заключается в том, что в случае если в коллекции отсутствует первый элемента, `FirstOrDefault` возвращает значение по умолчанию, а `First` в этому случае выдает исключение.

Примеры:

```cs
var students= new List<Student>();
var firstStudent = students
    .FirstOrDefault();       //firstStudent  = null


var students2= new List<Student>();
var firstStudent2 = students2
    .First();                //InvalidOperationException!
```

5. Что делает функция `Take` встроенного языка `Linq`?

**Ответ:**

Берет из коллекции указанное количество первых элементов.

Пример:

```cs
var numbers = new List<int>{ 1, 2, 3, 4 };
var oneNumber = numbers
    .Take(1)
    .ToList();      //new List<int>{ 1 }

var twoNumbers = numbers
    .Take(2)
    .ToList();      //new List<int> { 1, 2 }
```

Обратите внимание, что даже если мы берем всего один элемент, у нас по итогу все равно получается коллекция, но только из одного элемента.

6. Что такое чистая функция?

**Ответ:**

Чистые функции детерминированы и не имеют побочных эффектов.

Для одного и того же набора входных значений функция возвращает одинаковый результат.

Функция взаимодействует с внешнми миром только путем вызова других чистых функций и возврата своего результата.
