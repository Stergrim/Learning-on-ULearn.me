# Практика. Логгер

Создайте абстрактный класс `AbstractLogger`, а также два класса, которые будут наследовать этот класс и реализовывать методы:
- поля, **calendar**, **name**, (используйте для них наиболее подходящие типы данных),
- методы `debug`, `info`, `warning`, `error`, принимающие строку и возвращающие соответствующий уровень сообщения. Например: `debug` вернет строку типа: "[DEBUG] calendar name message",
- метод `toString`, который возвращает строку типа: "name Class.name",
- метод `setCalendar`, который принимает год, месяц, день, час, минуту, секунду и присваивает это значение полю **calendar**.

Создайте класс `LogManager`, который содержит в себе список объектов класса `Logger`, имеющий методы:
- `addLogger()` — принимает логгер и добавляет его в список,
- `getLogger()` — возвращает логгер по его имени,
- `printLoggers()` — возвращает строку, в которой записаны все строковые представления логгеров (toString), каждое в отдельных строках.

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public abstract class AbstractLogger {
    protected Calendar calendar;
    protected String name;
    
    public AbstractLogger(String name) {
        this.name = name;
        this.calendar = Calendar.getInstance();
    }
    
    public abstract String debug(String message);
    public abstract String info(String message);
    public abstract String warning(String message);
    public abstract String error(String message);
    public abstract String toString();
    
    public void setCalendar(TimeZone timeZone, int year, int month, int day, 
                           int hours, int minutes, int seconds) {
        this.calendar = Calendar.getInstance(timeZone);
        this.calendar.set(year, month, day, hours, minutes, seconds);
    }
    
    protected String formatDate() {
        String[] daysOfWeek = {"Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"};
        String[] months = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
        
        String dayOfWeek = daysOfWeek[calendar.get(Calendar.DAY_OF_WEEK) - 1];
        String month = months[calendar.get(Calendar.MONTH) - 1];
        int day = calendar.get(Calendar.DAY_OF_MONTH);
        int hour = calendar.get(Calendar.HOUR_OF_DAY);
        int minute = calendar.get(Calendar.MINUTE);
        int second = calendar.get(Calendar.SECOND);
        int year = calendar.get(Calendar.YEAR);
        
        String timeZoneID = calendar.getTimeZone().getDisplayName(false, TimeZone.SHORT);
        
        return String.format("%s %s %02d %02d:%02d:%02d %s %d", 
                           dayOfWeek, month, day, hour, minute, second, timeZoneID, year);
    }
}

public class DefaultLogger extends AbstractLogger {
    public DefaultLogger(String name) {
        super(name);
    }
    
    @Override
    public String debug(String message) {
        return String.format("[DEBUG] <%s> %s - %s", formatDate(), name, message);
    }
    
    @Override
    public String info(String message) {
        return String.format("[INFO] <%s> %s - %s", formatDate(), name, message);
    }
    
    @Override
    public String warning(String message) {
        return String.format("[WARNING] <%s> %s - %s", formatDate(), name, message);
    }
    
    @Override
    public String error(String message) {
        return String.format("[ERROR] <%s> %s - %s", formatDate(), name, message);
    }
    
    @Override
    public String toString() {
        return name + " — DefaultLogger";
    }
}

public class InputControlLogger extends AbstractLogger {
    public InputControlLogger(String name) {
        super(name);
    }
    
    @Override
    public String debug(String message) {
        return String.format("[DEBUG] <%s> Обнаружен пользователь: %s", formatDate(), message);
    }
    
    @Override
    public String info(String message) {
        return String.format("[INFO] <%s> Вход пользователя: %s", formatDate(), message);
    }
    
    @Override
    public String warning(String message) {
        return String.format("[WARNING] <%s> Обнаружен неавторизованный доступ в систему: %s", 
                           formatDate(), message);
    }
    
    @Override
    public String error(String message) {
        return String.format("[ERROR] <%s> Не удалось найти данные пользователя: %s", 
                           formatDate(), message);
    }
    
    @Override
    public String toString() {
        return name + " — InputControlLogger";
    }
}

public class LogManager {
    private List<AbstractLogger> loggers = new ArrayList<>();
    
    public void addLogger(AbstractLogger logger) {
        loggers.add(logger);
    }
    
    public AbstractLogger getLogger(String name) {
        for (AbstractLogger logger : loggers) {
            if (logger.name.equals(name)) {
                return logger;
            }
        }
        return new DefaultLogger(name);
    }
    
    public String printLoggers() {
        StringBuilder result = new StringBuilder();
        for (AbstractLogger logger : loggers) {
            result.append(logger.toString())
                  .append("\n");
        }
        return result.toString();
    }
}
```
