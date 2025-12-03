# Практика. Airport

В этой задаче вам будет необходимо написать метод `findPlanesLeavingInTheNextTwoHours()`, который должен вернуть список рейсов, вылетающих в ближайшие два часа:
1. Изучите библиотеку Airport перед написанием метода.
2. Используйте Stream API.

[airport.jar](Airport-1.0.jar)

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Main {
    public static List<Flight> findPlanesLeavingInTheNextTwoHours(Airport airport) {
        final int TWO_HOURS = 7_200_000;
        return airport.getTerminals().stream()
                .map(Terminal::getFlights).flatMap(List::stream)
                .filter(flight -> flight.getType() == Flight.Type.DEPARTURE
                        && flight.getDate().getTime() > System.currentTimeMillis()
                        && flight.getDate().getTime() < System.currentTimeMillis() + TWO_HOURS)
                .collect(Collectors.toList());
    }
}
```
