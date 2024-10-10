# Создание методов расширения

И снова сделайте так, чтобы код заработал!

```cs
public static void Main()
{
    var arg1 = "100500";
    Console.Write(arg1.ToInt() + "42".ToInt()); // 100542
}
```

Все тесты пройдены, задача сдана:
```cs
public static class ToIntExtensions
{
    public static int ToInt(this String str) { return int.Parse(str); }
}
```

Вывод программы:
```cs
100542
```
