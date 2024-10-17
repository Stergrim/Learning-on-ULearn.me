# Уровень 4

Как вы уже поняли, далеко не все комментарии полезны!

Исходный код:
```cs
void Main(string[] args)
{
    try
    {
        var linesCount = 0;
        var charsCount = 0;
        var lines = File.ReadAllLines(args[0]);
        foreach(var line in lines)
        {
            linesCount++;
            charsCount += line.Length;
        } //foreach
        Console.WriteLine("linesCount = " + linesCount);
        Console.WriteLine("charsCount = " + charsCount);
    } //try
    catch (Exception e)
    {
        Console.WriteLine("Error: " + e.Message);
    } //catch
}
```

Исправленный код:
```cs
void Main(string[] args)
{
    try
    {
        var linesCount = 0;
        var charsCount = 0;
        var lines = File.ReadAllLines(args[0]);
        foreach(var line in lines)
        {
            linesCount++;
            charsCount += line.Length;
        }
        Console.WriteLine("linesCount = " + linesCount);
        Console.WriteLine("charsCount = " + charsCount);
    }
    catch (Exception e)
    {
        Console.WriteLine("Error: " + e.Message);
    }
}
```

Объяснения:
- Комментарии вида 'конец цикла', 'конец функции' и подобные бессмысленны.
- Для коротких функций такие комментарии не нужны, а длинные функции лучше разбить на несколько более коротких.
- Современные среды разработки и программистские редакторы умеют подсвечивать парные скобки. Это надежнее таких комментариев.
