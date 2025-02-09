# Вопросы для самопроверки

1. Что такое generic?

**Ответ:**

Конструкция языка программирования C# для обобщения системы типов, позволяющее писать код, который будет одинаково работать с различными типами данных с сохранением строгой типизации языка.

2. В чем сложность реализации очереди на массиве?

**Ответ:**

В эффективной реализации Dequeue. Он должен удалять элемент из начала списка, а это линейная операция.

3. Как вернуть несколько значений?

**Ответ:**

- Использовать ref и out параметры метода
- Создать свой тип с несколькими полями
- Вернуть кортеж (Tuple, или ValueTuple)


4. В чем отличие параметра ref от параметра out?

**Ответ:**

ref позволяет передавать информацию снаружи в ваш метод и обратно, а out – это односторонний параметр, позволяющий передать значение только из метода наружу.

5. Что делают методы стека `Push` и `Pop`?

**Ответ:**

`Push` вносит элемент в верхушку стека, а `Pop` достает из этой верхушки и возвращает этот элемент. То есть в случае с `Push` элемент добавляется, в случае с `Pop` элемент возвращается и удаляется из стека.

6. Что делают методы очереди `Enqueue` и `Dequeue`?

**Ответ:**

`Enqueue` вносит элемент в очередь, `Dequeue` достает первый элемент из очереди и возвращает его.

7. Как объявить какой-либо класс с универсальным типом? К примеру, чтобы стек мог хранить значения любого типа.

**Ответ:**

Необходимо после имени в угловых скобках указать произвольный универсальный класс.

Пример: `public class MyStack<T>` — в данном случае тип `T` будет универсальным типом, который можно использовать в коде.

8. Каким образом мы можем явно указать, что тип `T` должен реализовывать интерфейс `IComparable`?

**Ответ:**

Указать это ограничение с помощью ключевого слова `where`.

Пример:

```cs
class SortedList<T>
    where T : IComparable
```

9. Можем ли мы принимать в качестве аргумента метода объект универсального типа? Если да, то что нужно для этого сделать?

**Ответ:**

Да, можем. Для этого необходимо после имени метода в угловых скобках указать универсальный тип метода.

Пример: `public static void Sort<T>(T[] array)`

10. Каким способами мы можем создать объект класса `Tuple`?

**Ответ:**

Существует два способа: использовать оператор `new` или воспользоваться методом `Create`.

Пример:

```cs
new Tuple<T1,T2>(someObject1, someObject2); //1 способ
Tuple.Create(someObject1, someObject2);     //2 способ
```

11. Зачем нужен класс `Nullable<T>`?

**Ответ:**

Класс `Nullable<T>` нужен для того, чтобы дать возможность не nullable типам принимать значение `null`.

12. Каким образом, используя сокращенный синтаксис, мы можем сделать `int` nullable типом и возвращать его в качестве результата метода `GetNullableInt`?

**Ответ:**

Указать после `int` знак `?`, тем самым указав компилятору, что мы хотим использовать тип `Nullable<int>`.
