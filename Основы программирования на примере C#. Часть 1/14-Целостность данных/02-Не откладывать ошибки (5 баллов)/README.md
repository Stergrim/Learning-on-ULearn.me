# Не откладывать ошибки

Если в свойстве Name окажется null, то с ошибкой завершится метод FormatStudent. Чтобы предотвратить отложенную ошибку, сделайте так, чтобы свойству Name нельзя было присвоить null. При попытке это сделать бросайте исключение оператором `throw new ArgumentException();`

```cs
private static void WriteStudent()
{
    // ReadName считает неизвестно откуда имя очередного студента
    var student = new Student { Name = ReadName() };
    Console.WriteLine("student " + FormatStudent(student));
}

private static string FormatStudent(Student student)
{
    return student.Name.ToUpper();
}
```

Все тесты пройдены, задача сдана:
```cs
public class Student
{
    private string name;
    public string Name
    {
        get { return name; } 
        set { if (value == null) throw new ArgumentException(); name = value; }
    }
}
```

Вывод программы:
```cs
student VIKTOR
student VASILIY
student FEDOR
ArgumentException
```
