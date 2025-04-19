# Fluent API

```cs
void M1()
{
    var settings = new Settings();
    settings.Delimiter = '\t';
    var reader = new XmlReader(settings);
    ReadDocument(reader);
}
```

1. Метод M1 является примером использования Fluent API (1 из 1 балла)
   * 🔴 **Верно**
   * 🟢 **Неверно**


```cs
int[] M2(){
    return ImmutableList<int>.Empty
        .Add(42).Concat(32, 45, 28)
        .Where(n => n % 2 == 0).ToArray();
}
```

2. Метод M2 является примером использования Fluent API (1 из 1 балла)
   * 🟢 **Верно**
   * 🔴 **Неверно**


3. Отметьте все отличительные особенности, характерные для Fluent API (1 из 1 балла)
   * ✅ **Fluent API удобен при изучении за счет подсказок среды разработки** (Правильно!)
   * ✅ **Код использующий Fluent API читается почти как текст на естественном языке** (Правильно!)
   * ❌ **Fluent API активно используют методы с out и ref параметрами**
   * ✅ **Fluent API активно использует технику method chaining** (Правильно!)
   * ✅ **Linq является примером Fluent API** (Правильно!)
