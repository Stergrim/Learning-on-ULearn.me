# В поисках степени двойки

Найдите минимальную степень двойки, превосходящую заданное число.

Более формально: для заданного числа ***n*** найдите минимальное целое ***x > n***, такое, что ***x = 2^k*** для некоторого целого, неотрицательного ***k***.

Решите эту задачу с помощью цикла while.

```cs
public static void Main()
{
    Console.WriteLine(GetMinPowerOfTwoLargerThan(2)); // => 4
    Console.WriteLine(GetMinPowerOfTwoLargerThan(15)); // => 16
    Console.WriteLine(GetMinPowerOfTwoLargerThan(-2)); // => 1
    Console.WriteLine(GetMinPowerOfTwoLargerThan(-100));
    Console.WriteLine(GetMinPowerOfTwoLargerThan(100));

}
```

Все тесты пройдены, задача сдана:
```cs
private static int GetMinPowerOfTwoLargerThan(int number)
{
    int result = 1;
    while (result <= number)
        result = result*2;
    return result;
}
```

Вывод программы:
```cs
4
16
1
1
128
```
