# Дробь

Проведите рефакторинг класса `Ratio`. В результате:
- `Numerator`, `Denominator` и `Value` должны остаться полями класса `Ratio`.
- После создания объекта `Ratio` не должно быть возможности его изменить, то есть поменять поля `Numerator`, `Denominator` или `Value`.
- После создания объекта `Ratio` знаменатель всегда должен быть больше нуля. Бросайте исключение `ArgumentException` при попытке установить неверное значение знаменателя.


```cs
public static void Check(int num, int den)
{
    var ratio = new Ratio(num, den);
    Console.WriteLine("{0}/{1} = {2}",
        ratio.Numerator, ratio.Denominator,
        ratio.Value.ToString(CultureInfo.InvariantCulture));
}
```

Все тесты пройдены, задача сдана:
```cs
public class Ratio
{
    public readonly int Numerator;
    public readonly int Denominator;
    public readonly double Value;
    
    public Ratio(int num, int den)
    {
        if (den <= 0) throw new ArgumentException();
        Numerator = num;
        Denominator = den;
        Value = (double)num/den;
    }
}
```

Вывод программы:
```cs
1/2 = 0.5
-10/5 = -2
ArgumentException
ArgumentException
```
