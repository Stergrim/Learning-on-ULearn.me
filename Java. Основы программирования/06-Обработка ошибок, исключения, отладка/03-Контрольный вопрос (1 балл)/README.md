# Контрольный вопрос

Рассмотрим пример кода:

```java
try {
  // код, бросающий исключения
} catch (RuntimeException e) {
  // ...
} catch (Exception e) {
  // ...
} catch (Error e) {
  // ...
} finally {
  // ...
}
```

Предположим, что во время исполнения программы в теле блока try было выброшено исключение типа java.lang.IllegalStateException. Какие блоки catch-finally будут выполнены?

1. Тест — Выберите один или несколько вариантов из списка (1 из 1 балла)
   * ✅ **catch (RuntimeException e)** (Правильно!)
   * ❌ **catch (Error e)**
   * ✅ **finally** (Правильно!)
   * ❌ **catch (Exception e)**
