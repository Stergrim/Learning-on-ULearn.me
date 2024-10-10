# Рамочка

Вы решили помочь Васе с разработкой его игры и взяли на себя красивый вывод сообщений в игре.

Напишите функцию, которая принимает на вход строку текста и печатает ее на экран в рамочке из символов +, - и |. Для красоты текст должен отделяться от рамки слева и справа пробелом.

Например, текст Hello world должен выводиться так:

```cs
+-------------+
| Hello world |
+-------------+

public static void Main()
{
    WriteTextWithBorder("Menu:");
    WriteTextWithBorder("");
    WriteTextWithBorder(" ");
    WriteTextWithBorder("Game Over!");
    WriteTextWithBorder("Select level:");
}
```

Все тесты пройдены, задача сдана:
```cs
private static void WriteTextWithBorder(string text)
{
    int length = text.Length + 2;
    Console.Write("+");
    for (int i = 0; i < length; i++)
        Console.Write("-");
    
    Console.WriteLine("+");
    Console.WriteLine("| " + text + " |");
    Console.Write("+");
    for (int i = 0; i < length; i++)
        Console.Write("-");
    
    Console.WriteLine("+");
}
```

Вывод программы:
```cs
+-------+
| Menu: |
+-------+
+--+
|  |
+--+
+---+
|   |
+---+
+------------+
| Game Over! |
+------------+
+---------------+
| Select level: |
+---------------+
```
