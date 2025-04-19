# Упражнение на операторы

Напишите как можно меньше кода, чтобы программа скомпилировалась.

```cs
public static void Main()
{
    int a = new A() + new A() + new A() * "abc";
}
```

Все тесты пройдены, задача сдана:
```cs
class A
{
    public int Val;
    public static A operator +(A a1, A a2) => new A { Val = a1.Val + a2.Val };
    public static A operator *(A a1, string s1) => new A();
    public static implicit operator int(A a1) => a1.Val;
}
```
