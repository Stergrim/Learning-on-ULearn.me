# Поля классов

```cs
class SomeClass {
  public static int s;
  private int p;
  public int d;
}
```

1. Отметьте все корректные обращения к полям объявленного выше класса SomeClass (1 из 1 балла)
   * ✅ **SomeClass.s = 42** (Правильно!)
   * ❌ **SomeClass.d = 42** (Обращения к динамическим полям имеет смысл только в контексте объект)
   * ❌ **new SomeClass().s = 42** (Чтобы не вводить читающих в заблуждение, в C# обращаться к статическим полям также как к нестатическим запрещено)
   * ✅ **new SomeClass().d = 42** (Правильно! Обратиться к нестатическому полю можно имея ссылку на объект. Например, ссылку на только что созданный оператором new объект.)
   * ❌ **new SomeClass().p = 42** (Доступ к приватным членам класса возможен только изнутри класса, в котором они объявлен)
