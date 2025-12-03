# Практика. Массивы

В практике «Массивы» мы с вами напишем своё консольное приложение, которое будет симулировать работу больницы и формировать отчёт по пациентам.

Нам потребуется написать класс **Hospital**, а также метод `main()` который будет работать с этим классом через консоль.
- Реализацию метода `main()` можете сделать так, как вам угодно, важно лишь учитывать, что переменные класса **Hospital** неизменяемые и поменять вывод вы можете лишь создав новую больницу.
- В самом классе **Hospital** сложнее, здесь для класса потребуется создать конструктор и несколько методов, которые зависят друг от друга, если какого-то значения нет(нужно проверить перед выполнением), необходимо будет их вычислить перед выполнением. В методе `getReport()` — собираем всё, что получили из других методов и выводим в формате строки.

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
    
        System.out.println("=== СИСТЕМА УЧЕТА ПАЦИЕНТОВ БОЛЬНИЦЫ ===");
    
        int patientsCount = 0;
        while (patientsCount <= 0) {
            System.out.print("Введите количество пациентов: ");
            try {
                patientsCount = scanner.nextInt();
                if (patientsCount <= 0) {
                    System.out.println("Количество пациентов должно быть положительным числом!");
                }
            } catch (Exception e) {
                System.out.println("Ошибка! Введите целое число.");
                scanner.nextLine();
            }
        }
    
        Hospital hospital = new Hospital(patientsCount);
    
        boolean exit = false;
        while (!exit) {
            System.out.println("\n=== МЕНЮ ===");
            System.out.println("1 - Сгенерировать температуры пациентов");
            System.out.println("2 - Показать температуры пациентов");
            System.out.println("3 - Показать среднюю температуру");
            System.out.println("4 - Показать количество здоровых пациентов");
            System.out.println("5 - Полный отчет");
            System.out.println("0 - Выход");
            System.out.print("Выберите действие: ");
    
            int choice;
            try {
                choice = scanner.nextInt();
            } catch (Exception e) {
                System.out.println("Ошибка! Введите число от 0 до 5.");
                scanner.nextLine();
                continue;
            }
    
            switch (choice) {
                case 1:
                    float[] temps = hospital.generatePatientsTemperatures();
                    System.out.println("Температуры пациентов сгенерированы!");
                    break;
    
                case 2:
                    String tempsString = hospital.getTemperaturesToString();
                    System.out.println("Температуры пациентов: " + tempsString);
                    break;
    
                case 3:
                    double avgTemp = hospital.getAverageTemp();
                    System.out.printf("Средняя температура: %.2f°C%n", avgTemp);
                    break;
    
                case 4:
                    int healthyCount = hospital.getCountHealthy();
                    System.out.println("Количество здоровых пациентов: " + healthyCount);
                    break;
    
                case 5:
                    String report = hospital.getReport();
                    System.out.println("\n=== ПОЛНЫЙ ОТЧЕТ ===");
                    System.out.println(report);
                    break;
    
                case 0:
                    exit = true;
                    System.out.println("Выход из системы...");
                    break;
    
                default:
                    System.out.println("Неверный выбор! Введите число от 0 до 5.");
            }
        }
    
        scanner.close();
        System.out.println("Работа системы завершена.");
    }
}

public class Hospital {
    private final int patientsCount;
    private float[] temperatures;
    private Integer healthyCount;
    private String temperaturesString;
    private Double averageTemp;
    
    public Hospital(int patientsCount) {
        if (patientsCount <= 0) {
            throw new IllegalArgumentException("Количество пациентов должно быть положительным числом");
        }
        this.patientsCount = patientsCount;
    }
    
    public float[] generatePatientsTemperatures() {
        if (temperatures == null) {
            temperatures = new float[patientsCount];
            for (int i = 0; i < patientsCount; i++) {
                float temp = 32.0f + (float)(Math.random() * 80) / 10.0f;
                temperatures[i] = temp;
            }
        }
        return temperatures.clone();
    }
    
    public int getCountHealthy() {
        if (healthyCount != null) {
            return healthyCount;
        }
    
        if (temperatures == null) {
            generatePatientsTemperatures();
        }
    
        healthyCount = 0;
        for (float temp : temperatures) {
            if (temp > 36.2f && temp < 36.9f) {
                healthyCount++;
            }
        }
        return healthyCount;
    }
    
    public String getTemperaturesToString() {
        if (temperaturesString != null) {
            return temperaturesString;
        }
    
        if (temperatures == null) {
            generatePatientsTemperatures();
        }
    
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < temperatures.length; i++) {
            sb.append(String.format("%.1f", temperatures[i]));
            if (i < temperatures.length - 1) {
                sb.append(" ");
            }
        }
        temperaturesString = sb.toString();
        return temperaturesString;
    }
    
    public double getAverageTemp() {
        if (averageTemp != null) {
            return averageTemp;
        }
    
        if (temperatures == null) {
            generatePatientsTemperatures();
        }
    
        double sum = 0;
        for (float temp : temperatures) {
            sum += temp;
        }
        averageTemp = sum / temperatures.length;
        return averageTemp;
    }
    
    public String getReport() {
        if (temperatures == null) {
            generatePatientsTemperatures();
        }
        if (healthyCount == null) {
            getCountHealthy();
        }
        if (averageTemp == null) {
            getAverageTemp();
        }
        if (temperaturesString == null) {
            getTemperaturesToString();
        }
    
        return String.format(
                "Температуры пациентов: %s%nСредняя температура: %.2f%nКоличество здоровых: %d",
                temperaturesString,
                averageTemp,
                healthyCount
        );
    }
}
```
