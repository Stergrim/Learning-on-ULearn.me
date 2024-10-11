# Разыскиваются методы!

Напишите тело метода так, чтобы он возвращал вторую половину строки `text`, из которой затем удалены пробелы. Можете считать, что `text` всегда четной длины.

Всю информацию о доступных методах класса `String` вы можете прочитать в [официальной документации .NET](https://learn.microsoft.com/ru-ru/dotnet/api/system.string?view=net-8.0&redirectedfrom=MSDN)

```cs
public static void Main()
{
	Console.WriteLine(GetLastHalf("I love CSharp!"));
	Console.WriteLine(GetLastHalf("1234567890"));
	Console.WriteLine(GetLastHalf("до ре ми фа соль ля си"));
}
```

Все тесты пройдены, задача сдана:
```cs
static string GetLastHalf(string text)
{
    return text.Substring(text.Length/2).Replace(" ", null);
}
```

Вывод программы:
```cs
CSharp!
67890
сольляси
```
