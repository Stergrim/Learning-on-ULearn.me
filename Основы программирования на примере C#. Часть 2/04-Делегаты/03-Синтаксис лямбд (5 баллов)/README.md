# Синтаксис лямбд

Поупражняйтесь в создании лямбда-выражений

```cs
public static void Main()
{
    Assert.AreEqual(0, zero());
    
    Assert.AreEqual("42", toString(42));
    Assert.AreEqual("123", toString(123));
        
    Assert.AreEqual(3.14, add(1.1, 2.04));
    Assert.AreEqual(0, add(-1, 1));
        
    print("passed!");
}
```


Все тесты пройдены, задача сдана:
```cs
private static readonly Func<int> zero = () => 0;
private static readonly Func<int, string> toString = str => str.ToString(); 
private static readonly Func<double,double,double> add = (a,b) => a+b;
private static readonly Action<string> print = str => Console.WriteLine(str);
```

Вывод программы:
```cs
passed!
```
