# Упражнение. Правильный регистр

Необходимо реализовать метод `getRevertString()`, который переворачивает строку и приводит её к нижнему регистру, а первую букву строки к верхнему регистру.

**Все тесты пройдены, задача сдана:**
```java
public static String getRevertString(String str) {
    String reversed = new StringBuilder(str).reverse().toString().toLowerCase();
    return reversed.substring(0, 1).toUpperCase() + reversed.substring(1);
}
```
