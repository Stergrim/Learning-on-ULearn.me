# Применение ref

В этой задаче демонстрируется один из редких случаев, когда уместно использовать `ref`.

`SkipSpaces` пропускает все пробельные символы в `text`, начиная с позиции `pos`. В конце `pos` оказывается установлен в первый непробельный символ.

```cs
public static void SkipSpaces(string text, ref int pos)
{
    while (pos < text.Length && char.IsWhiteSpace(text[pos]))
        pos++;
}
```

`ReadNumber` пропускает все цифры в `text`, начиная с позиции `pos`, а затем возвращает все пропущенные символы. В конце `pos` оказывается установлен в первый символ не цифру.

```cs
public static string ReadNumber(string text, ref int pos)
{
    var start = pos;
    while (pos < text.Length && char.IsDigit(text[pos]))
        pos++;
    return text.Substring(start, pos - start);
}
```

Допишите функцию, которая читает все числа из строки и выводит их, разделяя единственным пробелом.


Все тесты пройдены, задача сдана:
```cs
public static void WriteAllNumbersFromText(string text)
{
    int pos = 0;
    while (true)
    {
        SkipSpaces(text, ref pos);
        var num = ReadNumber(text, ref pos);
        if (num == "") break;
        Console.Write(num + " ");
    }
    Console.WriteLine();
}
```

Вывод программы:
```cs
1 2 3 
4 
567890 

10 20 30
```
