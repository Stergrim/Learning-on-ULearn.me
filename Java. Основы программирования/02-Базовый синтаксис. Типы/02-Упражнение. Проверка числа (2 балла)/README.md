# Упражнение. Проверка числа

В метод передается число от 0 до 99999. Программа должна напечатать TRUE (или FALSE в противном случае), если запись этого числа содержит только чётные цифры.

Пример: 6204

Результат: TRUE

**Все тесты пройдены, задача сдана:**
```java
public static boolean check(int number) {
    while (number > 0)
        if (number % 2 == 0) number /= 10;
        else return false;
    return true;
}
```
