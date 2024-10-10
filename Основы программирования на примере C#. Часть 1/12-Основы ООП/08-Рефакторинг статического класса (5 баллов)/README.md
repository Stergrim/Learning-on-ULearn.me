# Рефакторинг статического класса

Как вы уже догадались... Сделайте так, чтобы код заработал! Для этого сделайте класс SuperBeautyImageFilter не статическим

```cs
public static void Main()
{
    var filter = new SuperBeautyImageFilter();
    filter.ImageName = "Paris.jpg";
    filter.GaussianParameter = 0.4;
    filter.Run();
}
```

Все тесты пройдены, задача сдана:
```cs
public class SuperBeautyImageFilter
{
    public string ImageName;
    public double GaussianParameter;
    public void Run()
    {
        Console.WriteLine("Processing {0} with parameter {1}", 
                          ImageName, 
                          GaussianParameter.ToString(CultureInfo.InvariantCulture));
    }
}
```

Вывод программы:
```cs
Processing Paris.jpg with parameter 0.4
```
