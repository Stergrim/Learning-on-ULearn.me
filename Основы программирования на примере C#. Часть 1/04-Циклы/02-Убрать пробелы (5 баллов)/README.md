# Убрать пробелы

Враги вставили в начало каждого полезного текста целую кучу бесполезных пробельных символов!

Вася смог справиться с ситуацией, когда такой пробел был один, но продвинуться дальше ему не удается. Помогите ему с помощью цикла while.

```cs
public static void Main()
{
    Console.WriteLine(RemoveStartSpaces("a"));
    Console.WriteLine(RemoveStartSpaces(" b"));
    Console.WriteLine(RemoveStartSpaces(" cd"));
    Console.WriteLine(RemoveStartSpaces(" efg"));
    Console.WriteLine(RemoveStartSpaces(" text"));
    Console.WriteLine(RemoveStartSpaces(" two words"));
    Console.WriteLine(RemoveStartSpaces("  two spaces"));
    Console.WriteLine(RemoveStartSpaces("	tabs"));
    Console.WriteLine(RemoveStartSpaces("		two	tabs"));
    Console.WriteLine(RemoveStartSpaces("                             many spaces"));
    Console.WriteLine(RemoveStartSpaces("               "));
    Console.WriteLine(RemoveStartSpaces("\n\r line breaks are spaces too"));
}
```

Все тесты пройдены, задача сдана:
```cs
public static string RemoveStartSpaces(string text)
{
    int a = 0;
    while (a < text.Length)
        if (char.IsWhiteSpace(text[a])) a = a + 1;
        else break;
    return text.Substring(a);
}
```

Вывод программы:
```cs
a
b
cd
efg
text
two words
two spaces
tabs
two	tabs
many spaces

line breaks are spaces too
```
