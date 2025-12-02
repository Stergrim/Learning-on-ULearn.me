## [Java. Основы программирования](https://ulearn.me/course/java-rtf/fb1ff654-a3aa-4945-9c7f-7ef835477eb6)

Курс рассчитан на людей, прошедших курсы «Основы программирования на примере C#», знакомит с основами синтаксиса Java и стандартными пакетами, учит применять полученные знания на практических задачах.


```java
public static boolean check(int number) {
    while (number > 0)
        if (number % 2 == 0) number /= 10;
        else return false;
    return true;
}
```