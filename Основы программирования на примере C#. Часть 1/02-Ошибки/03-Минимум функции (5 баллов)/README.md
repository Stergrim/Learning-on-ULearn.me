# Минимум функции

*Эта задача потребует знания конструкции if-else. Если вы с ней знакомы из других языков программирования, это не составит проблем. Если не знакомы совсем, то лучше пропустить эту задачу и сначала пройти следующий модуль.*

Реализуйте метод `GetMinX` для нахождения такого числа ***x***, при котором кривая, заданная уравнением

$$ \displaystyle\ y(x) = a*x^2 $$

принимает минимальное значение.

Метод должен принимать неотрицательный коэффициент ***a***, а также произвольные коэффициенты ***b*** и ***c***, и, если решение существует, возвращать строковое представление искомого ***x***, а иначе — строку `Impossible`.

```cs
public static void Main()
{
    Console.WriteLine(GetMinX(1, 2, 3));
    Console.WriteLine(GetMinX(0, 3, 2));
    Console.WriteLine(GetMinX(1, -2, -3));
    Console.WriteLine(GetMinX(5, 2, 1));
    Console.WriteLine(GetMinX(4, 3, 2));
    Console.WriteLine(GetMinX(0, 4, 5));
    
    // А в этих случаях решение существует:
    Console.WriteLine(GetMinX(0, 0, 2) != "Impossible");
    Console.WriteLine(GetMinX(0, 0, 0) != "Impossible");
}
```

Все тесты пройдены, задача сдана:
```cs
private static string GetMinX(int a, int b, int c)
{
    if (a > 0) return (-1.0*b/(2*a)).ToString();
	if ((a == 0) && (b == 0) )  return "Also";
	else return "Impossible";
}
```

Вывод программы:
```cs
-1
Impossible
1
-0.2
-0.375
Impossible
True
True
```
