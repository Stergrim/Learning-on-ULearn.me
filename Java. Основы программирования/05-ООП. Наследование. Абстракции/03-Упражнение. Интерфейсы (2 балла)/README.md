# Упражнение. Интерфейсы

В этой практике вам нужно:
1. Создать интерфейс `TimeUnit`, который содержит методы `toMillis`, `toSeconds`, `toMinutes`, `toHours`, возвращающие тип **long**.
2. Создать статический класс `TimeUnitUtils` с методами `toMillis`, `toSeconds`, `toHours`, которые принимают любой класс, реализующий интерфейс `TimeUnit`, и возвращают значение в Миллисекундах, Секундах и Часах соответственно.
3. Создать классы `Seconds`, `Minutes`, `Milliseconds`, `Hours`, каждый из которых реализует интерфейс `TimeUnit` и содержит неизменяемое поле значения.


**Все тесты пройдены, задача сдана:**
```java
public interface TimeUnit {
    long toMillis();
    long toSeconds();
    long toMinutes();
    long toHours();
}

public class TimeUnitUtils {
    public static Milliseconds toMillis(TimeUnit unit) {
        return new Milliseconds(unit.toMillis());
    }
    
    public static Seconds toSeconds(TimeUnit unit) {
        return new Seconds(unit.toSeconds());
    }
    
    public static Hours toHours(TimeUnit unit) {
        return new Hours(unit.toHours());
    }
}

public class Seconds implements TimeUnit {
    private final long value;
    
    public Seconds(long value) {
        this.value = value;
    }
    
    @Override
    public long toMillis() {
        return Math.round((double) value * 1000);
    }
    
    @Override
    public long toSeconds() {
        return value;
    }
    
    @Override
    public long toMinutes() {
        return Math.round((double) value / 60);
    }
    
    @Override
    public long toHours() {
        return Math.round((double) value / 3600);
    }
}

public class Minutes implements TimeUnit {
    private final long value;
    
    public Minutes(long value) {
        this.value = value;
    }
    
    @Override
    public long toMillis() {
        return Math.round((double) value * 60 * 1000);
    }
    
    @Override
    public long toSeconds() {
        return Math.round((double) value * 60);
    }
    
    @Override
    public long toMinutes() {
        return value;
    }
    
    @Override
    public long toHours() {
        return Math.round((double) value / 60);
    }
}

public class Milliseconds implements TimeUnit {
    private final long value;
    
    public Milliseconds(long value) {
        this.value = value;
    }
    
    @Override
    public long toMillis() {
        return value;
    }
    
    @Override
    public long toSeconds() {
        return Math.round((double) value / 1000);
    }
    
    @Override
    public long toMinutes() {
        return Math.round((double) value / (1000 * 60));
    }
    
    @Override
    public long toHours() {
        return Math.round((double) value / (1000 * 3600));
    }
}

public class Hours implements TimeUnit {
    private final long value;
    
    public Hours(long value) {
        this.value = value;
    }
    
    @Override
    public long toMillis() {
        return Math.round((double) value * 3600 * 1000);
    }
    
    @Override
    public long toSeconds() {
        return Math.round((double) value * 3600);
    }
    
    @Override
    public long toMinutes() {
        return Math.round((double) value * 60);
    }
    
    @Override
    public long toHours() {
        return value;
    }
}
```
