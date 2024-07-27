# Ошибки преобразования типов

Исправьте все ошибки преобразования типов. Читайте и переводите с английского имена переменных, чтобы понять, что от вас требуется.

Все тесты пройдены, задача сдана:
```
public static void Main()
{
    double pi = Math.PI;
    int tenThousand = 10000;
    double tenThousandPi = pi*tenThousand;
    int roundedTenThousandPi = (int)Math.Round(tenThousandPi);
    int integerPartOfTenThousandPi = (int)tenThousandPi;
    Console.WriteLine(integerPartOfTenThousandPi);
    Console.WriteLine(roundedTenThousandPi);
}
```

Вывод программы:
```
31415
31416
```