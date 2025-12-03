# Практика. Простой калькулятор

В этой практике вам будет необходимо написать свой собственный калькулятор, у которого будет 3 публичных метода и 2 приватных:
- Разберёмся с методами `calculate()`, — есть один публичный метод, который может принять строку, где может быть как **строка операция строка**, так и **число операция число**. Обратите внимание, по формату они разделены пробелами. Затем уже в зависимости от типа полученной строки, мы получаем результат из наших приватных методов.
- метод `getNumbers()`, — публичный метод, который складывает два числа и возвращает кол-во чётных цифр в их сумме.
- и последний метод `getMinimalType()`, — принимает число и возвращает минимальный целочисленный тип, к которому его можно привести.

Примеры работы метода `getMinimalType()`:

Ввод: **5**, Вывод: "Byte"

Ввод: **-35000**, Вывод: "Int"

Ввод: **512**, Вывод: "Short"

Ввод: **2500000000**, Вывод: "Long"

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Calculator {
    public static String calculate(String input) {
        String[] parts = input.split(" ");
    
        if (parts.length != 3)
            throw new IllegalArgumentException("Неверный формат ввода. Ожидается: <аргумент> <операция> <аргумент>");
    
        String firstArg = parts[0];
        String operation = parts[1];
        String secondArg = parts[2];
    
        if (isNumber(firstArg) && isNumber(secondArg))
            return String.valueOf(calculate(Double.parseDouble(firstArg), Double.parseDouble(secondArg), operation));
        else if (!isNumber(firstArg) && !isNumber(secondArg))
            return calculate(firstArg, secondArg, operation);
        else
            throw new IllegalArgumentException("Смешанные типы не поддерживаются");
    }
    
    private static String calculate(String a, String b, String operation) {
        return switch (operation) {
            case "+" -> a + b;
            case "-" -> subtractStrings(a, b);
            default -> throw new IllegalArgumentException("Для строк поддерживаются только операции + и -");
        };
    }
    
    private static String subtractStrings(String a, String b) {
        StringBuilder result = new StringBuilder(a);
    
        for (int i = 0; i < b.length(); i++) {
            char currentChar = b.charAt(i);
            int index = result.indexOf(String.valueOf(currentChar));
    
            if (index == -1)
                throw new IllegalArgumentException("Символ '" + currentChar + "' из строки вычитания не найден в исходной строке");
    
            result.deleteCharAt(index);
        }
    
        return result.toString();
    }
    
    private static double calculate(double a, double b, String operation) {
        return switch (operation) {
            case "+" -> a + b;
            case "-" -> a - b;
            case "*" -> a * b;
            case "/" -> a / b;
            case "%" -> a % b;
            default -> throw new IllegalArgumentException("Неизвестная операция: " + operation);
        };
    }
    
    public static int getNumbers(int a, int b) {
        int num = Math.abs(a + b);
        int count = 0;
    
        while (num > 0) {
            if (num % 2 == 0) count++;
            num /= 10;
        }
    
        return count;
    }
    
    public static String getMinimalType(String input) {
        long number = Long.parseLong(input);
    
        if (number >= Byte.MIN_VALUE && number <= Byte.MAX_VALUE)
            return "Byte";
        else if (number >= Short.MIN_VALUE && number <= Short.MAX_VALUE)
            return "Short";
        else if (number >= Integer.MIN_VALUE && number <= Integer.MAX_VALUE)
            return "Int";
        else
            return "Long";
    }
    
    private static boolean isNumber(String str) {
        try {
            Double.parseDouble(str);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}
```
