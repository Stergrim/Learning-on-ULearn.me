# Свойство вместо поля

Превратите поле Title в свойство самостоятельно. Это совсем не сложно!

```cs
public static void Check()
{
    var book = new Book();
    book.Title = "Structure and interpretation of computer programs";
    Console.WriteLine(book.Title);
}
```

Все тесты пройдены, задача сдана:
```cs
public class Book { public string Title { get; set; } }
```

Вывод программы:
```cs
Structure and interpretation of computer programs
```
