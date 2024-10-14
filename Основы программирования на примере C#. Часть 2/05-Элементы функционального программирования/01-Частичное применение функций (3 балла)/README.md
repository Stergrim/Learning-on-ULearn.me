# Частичное применение функций

```cs
private static Func<int, int, int> Apply1(Func<int, int, int, int> func, int arg)
{
    return (x, y) => func(x, arg, y);
}

private static Func<int, int> Apply2(Func<int, int, int> func, int arg)
{
    return x => func(arg, x);
}

public static void Main()
{
    Func<int, int, int, int>f = (x, y, z) => x * y + z;
    var x0 = f(1, 2, 3);
    var x1 = Apply1(f, 100)(1, 11);
            
    var g = Apply2(Apply1(f, 10), 5);
    var x2 = g(3);
}
```

1. Чему равен x0? (1 из 1 балла)
   * 🟢 `5` (Правильный ответ: 5)


2. Чему равен x1?  (1 из 1 балла)
   * 🟢 `111` (Правильный ответ: 111)


3. Чему равен x2? (1 из 1 балла)
   * 🟢 `53` (Правильный ответ: 53)
