# Генерация последовательности

Ленивые методы полезны тем, что они могу генерировать последовательности до тех пор пока нам это необходимо. В данном упражнении реализуйте функцию, которая лениво генерирует циклическую последовательность через yield return.

Пример: При поочередном вызове метода `GenerateCycle(3)` должна возвращаться последовательность `{ 0, 1, 2, 0, 1, 2, 0, 1...}`

```cs
public static void Main()
{
    foreach (var number in GenerateCycle(4).Take(8))
    {
        Console.WriteLine(number);
    }
    
    SecretTests();
}
```


Все тесты пройдены, задача сдана:
```cs
public static IEnumerable<int> GenerateCycle(int maxValue)
{
    int i = 0;
    while(true)
    {
        for(; i<maxValue; i++)
            yield return i;
        i = 0;
    }
}
```

Вывод программы:
```cs
0
1
2
3
0
1
2
3
Test 1: OK!
Test 2: OK!
Test 3: OK!
```
