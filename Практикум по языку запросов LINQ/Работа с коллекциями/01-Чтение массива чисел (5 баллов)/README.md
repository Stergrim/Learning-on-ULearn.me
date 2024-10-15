# Чтение массива чисел

`LINQ` удобно использовать для чтения из файла и разбора простых текстовых форматов. Особенно удобно сочетать методы `LINQ` с методами класса `File`: `File.ReadLines(filename)`, `File.WriteLines(filename, lines)`.

На выходе метода ReadAllLines получается массив строк, поэтому часто возникает задача обработки именно массива строк.

Пусть у вас есть файл, в котором каждая строка либо пустая, либо содержит одно целое число. Напишите метод, который вернет массив всех чисел, присутствующих в исходном списке строк.

Ваш код будет проверяться с помощью такого метода `Main`:

```cs
public static void Main()
{
    foreach (var num in ParseNumbers(new[] {"-0", "+0000"}))
        Console.WriteLine(num);
    foreach (var num in ParseNumbers(new List<string> {"1", "", "-03", "0"}))
        Console.WriteLine(num);
}
```

Реализуйте метод `ParseNumbers` в одно `LINQ`-выражение.

Все тесты пройдены, задача сдана:
```cs
public static int[] ParseNumbers(IEnumerable<string> lines)
{
    return lines
           .Where(lines => !lines.Equals(""))
           .Select(lines => int.Parse(lines))
           .ToArray();
}
```

Вывод программы:
```cs
0
0
1
-3
0
```
