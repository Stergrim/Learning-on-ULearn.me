# Упражнение. Handler

1. Создайте 3 собственных класса-исключения: `HandlerException`, `HandlerResultException` и `NullHandlerException`, унаследовав их от `RuntimeException`.
2. В классе `Handler` реализуйте код метода `handleResults`, на вход которого поступает строка в специальном виде (*Пример строки на входе: "12.1.76.0"*).

В зависимости от ее содержания метод либо бросает исключение, либо возвращает оценку результата (*сумма первых двух чисел минус произведение двух последних*).
- Если строка содержит сочетание символов **"error"**, метод должен бросать исключение `HandlerException`.
- Если результат вычислений **меньше нуля**, метод должен бросать исключение `HandlerResultException`.
- Если входящая строка **пуста** или **null**, метод должен бросать исключение `NullHandlerException`.
- Код не должен падать при возникновении ошибок ввода, **несвязанных с предыдущими исключениями**.


**Все тесты пройдены, задача сдана:**
```java
public class Handler {
    public static class NullHandlerException extends RuntimeException {
        public NullHandlerException(String message) {
            super(message);
        }
    }
    
    public static class HandlerException extends RuntimeException {
        public HandlerException(String message) {
            super(message);
        }
    }
    
    public static class HandlerResultException extends RuntimeException {
        public HandlerResultException(String message) {
            super(message);
        }
    }
    
    public int handleResults(String input) {
        if (input == null || input.isEmpty()) {
            throw new NullHandlerException("Входная строка имеет значение null или пустая");
        }
    
        if (input.toLowerCase().contains("error")) {
            throw new HandlerException("Входные данные содержат 'error'");
        }
    
        try {
            String[] parts = input.split("\\.");
    
            if (parts.length < 4) return 0;
    
            int num1 = Integer.parseInt(parts[0]);
            int num2 = Integer.parseInt(parts[1]);
            int num3 = Integer.parseInt(parts[2]);
            int num4 = Integer.parseInt(parts[3]);
            int result = (num1 + num2) - (num3 * num4);
    
            if (result < 0) {
                throw new HandlerResultException("Результат отрицательный: " + result);
            }
            return result;
        } catch (NumberFormatException e) { return 0; }
    }
}
```
