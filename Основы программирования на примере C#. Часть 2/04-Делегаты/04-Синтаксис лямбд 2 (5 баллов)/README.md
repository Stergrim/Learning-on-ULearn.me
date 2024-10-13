# Синтаксис лямбд 2

Напишите функцию Combine, комбинирующую действие двух своих аргументов, чтобы ее можно было использовать вот так:

```cs
Func<int, string> toString = x => x.ToString("X"); // hex
Func<double, int> round = x => (int)Math.Round(x);
var f1 = Combine(round, toString);
Console.WriteLine(f1(3.14)); // 3
Console.WriteLine(f1(10.9)); // B 

Func<int, int> doubleValue = x => 2 * x;
Func<int, int> minusOne = x => x - 1;
var f2 = Combine(minusOne, doubleValue);
Console.WriteLine(f2(2)); // 2
Console.WriteLine(f2(5)); // 8
```


Все тесты пройдены, задача сдана:
```cs
static Func<T1, T3> Combine<T1, T2, T3>(Func<T1, T2> f, Func<T2, T3> g) { return x => g(f(x)); }
```

Вывод программы:
```cs
3
B
2
8
```
