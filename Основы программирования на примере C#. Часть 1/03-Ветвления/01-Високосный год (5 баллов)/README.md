# Високосный год

Вы с Васей решили посчитать количество дней между двумя солнечными затмениями. Сначала казалось, что все просто, пока вы не вспомнили про високосные года.

Вася написал первую версию функции, определяющей, является ли год високосным, но в реальности все чуть сложнее:

- год, номер которого кратен 400 — високосный;
- остальные года, номера которых кратны 100 — невисокосные;
- остальные года, номера которых кратны 4 — високосные.

Поразите Васю краткостью и лаконичностью — напишите решение в одно логическое выражение, без использования готовых функций.

```cs
public static void Main()
{
    Console.WriteLine(IsLeapYear(2014));
    Console.WriteLine(IsLeapYear(1999));
    Console.WriteLine(IsLeapYear(8992));
    Console.WriteLine(IsLeapYear(1));
    Console.WriteLine(IsLeapYear(14));
    Console.WriteLine(IsLeapYear(400));
    Console.WriteLine(IsLeapYear(600));
    Console.WriteLine(IsLeapYear(3200));
    FinalTesting(); // Тестирование решения на секретных тестах
}
```

Все тесты пройдены, задача сдана:
```cs
public static bool IsLeapYear(int year)
{
    return (year % 400 == 0)||((year % 100 != 0)&&(year % 4 == 0));
}
```

Вывод программы:
```cs
False
False
True
False
False
True
False
True
```
