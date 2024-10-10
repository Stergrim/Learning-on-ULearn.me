# Снова среднее трех

Помните задачу "Среднее трех"? Пришло время повторить ее для общего случая!

```cs
public static void Main()
{
    Console.WriteLine(MiddleOfThree(2, 5, 4));
    Console.WriteLine(MiddleOfThree(3, 1, 2));
    Console.WriteLine(MiddleOfThree(3, 5, 9));
    Console.WriteLine(MiddleOfThree("B", "Z", "A"));
    Console.WriteLine(MiddleOfThree(3.45, 2.67, 3.12));
}
```

Все тесты пройдены, задача сдана:
```cs
static IComparable MiddleOfThree(IComparable a, IComparable b, IComparable c)
{
    if (a.CompareTo(b) > 0)
    {
        if (b.CompareTo(c) > 0) return b;
        else if (a.CompareTo(c) > 0) return c;
        else return a;
    }
    else
    {
        if (a.CompareTo(c) > 0) return a;
        else if (b.CompareTo(c) > 0) return c;
        else return b;
    }
}
```

Вывод программы:
```cs
4
2
5
B
3.12
```
