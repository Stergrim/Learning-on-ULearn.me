# Последствия boxing

Изучите следующий код:

```cs
public struct S { int A; }
		
public class Program
{
    static void ShowEquals(object o1, object o2) 
    {
        Console.WriteLine(o1 == o2);
    }
    public static void Main() 
    {
        S s = new S();
        ShowEquals(s, s);
    }
}
```

1. Что будет выведено методом Main? (1 из 1 балла)
   * 🔴 **True**
   * 🟢 **False** (Правильно! При передаче структуры в метод ShowEquals случится boxing, как следствие переменной o окажется ссылка на другой объект)
   * 🔴 **Код не скомпилируется**


Изучите следующий код:

```cs
public struct S { int A; }

public class Program
{
    static void Main()
    {
        object[] s = new object[2];
        s[0] = new S();
        s[1] = s[0];
        Console.WriteLine(s[0] == s[1]);
    }
}
```

2. Что будет выведено методом Main? (1 из 1 балла)
   * 🟢 **True** (Правильно! s[0] будет содержать уже ссылку на боксинг-объект, она же будет и в s[1])
   * 🔴 **False**
   * 🔴 **Код не скомпилируется**
