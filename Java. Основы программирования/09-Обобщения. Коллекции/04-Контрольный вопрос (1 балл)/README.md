# Контрольный вопрос

Предположим, у нас есть две переменных:

```java
Collection<?> collection = ...;
Object object = ...;
```

Какие операции над collection допустимы?

1. Выберите один или несколько вариантов из списка. (1 из 1 балла)
   * ✅ **collection.contains(object)** (Правильно!)
   * ✅ **collection.remove(object)** (Правильно!)
   * ✅ **collection.toArray()** (Правильно!)
   * ✅ **collection.size()** (Правильно!)
   * ❌ **collection.addAll(Arrays.asList(object))**
   * ❌ **collection.add(object)**
   * ✅ **collection.clear()** (Правильно!)
   * ✅ **collection.iterator()** (Правильно!)
