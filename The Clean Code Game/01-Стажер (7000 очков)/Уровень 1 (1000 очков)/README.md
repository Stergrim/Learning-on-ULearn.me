# Уровень 1

Найди и исправь все стилевые ошибки в коде. Кликай мышкой по ошибкам.<br>
Каждая найденная ошибка: +1 балл.

Исходный код:
```cs
void Main()
{
    Console.WriteLine("Enter your name: ");
    var veryBadVariableName_clickIt = Console.ReadLine();
    Console.WriteLine("Hello, " + veryBadVariableName_clickIt);
}
```

Исправленный код:
```cs
void Main()
{
    Console.WriteLine("Enter your name: ");
    var name = Console.ReadLine();
    Console.WriteLine("Hello, " + name);
}
```

Объяснения:
- Длинное и ужасное имя переменной!
