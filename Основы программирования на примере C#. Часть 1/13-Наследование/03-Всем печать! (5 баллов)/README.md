# Всем печать!

Напишите метод, который печатает все, что угодно, через запятую.

```cs
public static void Main()
{
    Print(1, 2);
    Print("a", 'b');
    Print(1, "a");
    Print(true, "a", 1);
}
```

Все тесты пройдены, задача сдана:
```cs
public static void Print(params object[] array)
{
    for (var i = 0; i < array.Length; i++)
    {
        if (i > 0) Console.Write(", ");
        Console.Write(array.GetValue(i));
    }
    Console.WriteLine();
}
```

Вывод программы:
```cs
1, 2
a, b
1, a
True, a, 1
```
