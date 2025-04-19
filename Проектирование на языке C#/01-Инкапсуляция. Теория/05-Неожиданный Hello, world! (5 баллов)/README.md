# Неожиданный Hello, world!

Сделайте так, чтобы эта программа вывела на консоль Hello, world!

```cs
public static void Main()
{
    var a = new A();
    a.Number = 1;
}
```

Все тесты пройдены, задача сдана:
```cs
class A 
{
    public int Number { get; set; }
    public A() => Console.WriteLine("Hello, world!");
}
```

Вывод программы:
```cs
Hello, world!
```
