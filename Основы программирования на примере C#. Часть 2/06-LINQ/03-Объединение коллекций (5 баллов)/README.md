# Объединение коллекций

Вам дан список всех классов в школе. Нужно получить список всех учащихся всех классов.

Учебный класс определен так:

```cs
public class Classroom
{
    public List<string> Students = new List<string>();
}
```

Без использования `LINQ` решение могло бы выглядеть так:

```cs
var allStudents = new List<string>();
foreach (var classroom in classes)
{
    foreach (var student in classroom.Students)
    {
        allStudents.Add(student);
    }
}
return allStudents.ToArray();
```

Напишите решение этой задачи с помощью `LINQ` в одно выражение.

```cs
public static void Main()
{
    Classroom[] classes =
    {
        new Classroom {Students = {"Pavel", "Ivan", "Petr"},},
        new Classroom {Students = {"Anna", "Ilya", "Vladimir"},},
        new Classroom {Students = {"Bulat", "Alex", "Galina"},}
    };
    var allStudents = GetAllStudents(classes);
    Array.Sort(allStudents);
    Console.WriteLine(string.Join(" ", allStudents));
}
```


Все тесты пройдены, задача сдана:
```cs
public static string[] GetAllStudents(Classroom[] classes)
{
    return classes
           .SelectMany(classroom => classroom.Students)
           .ToArray();
}
```

Вывод программы:
```cs
Alex Anna Bulat Galina Ilya Ivan Pavel Petr Vladimir
```
