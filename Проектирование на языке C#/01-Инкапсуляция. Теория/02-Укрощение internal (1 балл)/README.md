# Укрощение internal

```cs
namespace MyNamespace
{
    internal class ClassA { }
    
    public class ClassB
    {
        public ClassA Method1() { return null; }
        private ClassB Method2() { return null; }
    }
}
```

1. Перечислите все способы, которыми можно избавиться от ошибок компиляции в этом коде (1 из 1 балла)
   * ✅ **Сменить модификатор доступа ClassB с public на internal** (Правильно!)
   * ✅ **Сменить модификатор доступа Method1 с public на internal** (Правильно!)
   * ✅ **Сменить модификатор доступа ClassA с internal на public** (Правильно!)
   * ✅ **Сменить модификатор доступа Method1 с public на private** (Правильно!)
   * ❌ **Сменить модификатор доступа Method2 с private на public**
   * ❌ **Перенести этот код в другую сборку**
