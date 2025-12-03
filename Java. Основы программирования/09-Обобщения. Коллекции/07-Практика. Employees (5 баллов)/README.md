# Практика. Employees

В этой задаче у вас будет возможность попрактиковаться co Stream API:
1. Напишите метод — `findEmployeeWithHighestSalary()`, который должен выделить сотрудников, пришедших в выбранном году, и среди них выявить сотрудника с максимальным значением заработной платы.
2. Напишите метод — `sortEmployee()`, который должен отсортировать сотрудников по выбранной колонке.
3. Напишите класс `Employee`.


**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Main {
    public static ArrayList<Employee> staff;
    
    public static void main(String[] args, String path) {
        staff = Employee.loadStaffFromFile(path);
        
        System.out.println("Сотрудник с максимальной зарплатой за 2018 год:");
        Employee highestPaid = findEmployeeWithHighestSalary(2018);
        if (highestPaid != null) {
            System.out.println(highestPaid);
        }
        
        System.out.println("\nСортировка по имени:");
        ArrayList<Employee> sortedByName = sortEmployee("name");
        sortedByName.forEach(System.out::println);
        
        System.out.println("\nСортировка по зарплате:");
        ArrayList<Employee> sortedBySalary = sortEmployee("salary");
        sortedBySalary.forEach(System.out::println);
        
        System.out.println("\nСортировка по дате:");
        ArrayList<Employee> sortedByDate = sortEmployee("date");
        sortedByDate.forEach(System.out::println);
    }
    
    public static Employee findEmployeeWithHighestSalary(int year) {
        if (year <= 0) {
            throw new IllegalArgumentException("Год должен быть положительным числом");
        }
    
        return staff.stream()
                .filter(employee -> {
                    Calendar cal = Calendar.getInstance();
                    cal.setTime(employee.getWorkStart());
                    return cal.get(Calendar.YEAR) == year;
                })
                .max(Comparator.comparing(Employee::getSalary))
                .orElse(null);
    }
    
    public static ArrayList<Employee> sortEmployee(String column) {
        if (column == null || column.trim().isEmpty()) {
            throw new IllegalArgumentException("Название колонки не может быть пустым");
        }
    
        Comparator<Employee> comparator;
        switch (column.toLowerCase()) {
            case "name":
                comparator = Comparator.comparing(Employee::getName);
                break;
            case "salary":
                comparator = Comparator.comparing(Employee::getSalary);
                break;
            case "date":
                comparator = Comparator.comparing(Employee::getWorkStart);
                break;
            default:
                throw new IllegalArgumentException("Неверное название колонки: " + column);
        }
        return staff.stream()
                .sorted(comparator)
                .collect(ArrayList::new, ArrayList::add, ArrayList::addAll);
    }
}

public class Employee {
    private String name;
    private Integer salary;
    private Date workStart;
    
    private static final SimpleDateFormat dateFormat = new SimpleDateFormat("dd.MM.yyyy");
    
    public Employee(String name, Integer salary, Date workStart) {
        this.name = name;
        this.salary = salary;
        this.workStart = workStart;
    }
    
    public static ArrayList<Employee> loadStaffFromFile(String path) {
        ArrayList<Employee> employees = new ArrayList<>();
    
        try {
            List<String> lines = Files.readAllLines(Paths.get(path));
            for (String line : lines) {
                String[] parts = line.split("\t");
                if (parts.length == 3) {
                    String name = parts[0];
                    Integer salary = Integer.parseInt(parts[1]);
                    Date workStart = dateFormat.parse(parts[2]);
                    employees.add(new Employee(name, salary, workStart));
                }
            }
        } catch (java.io.IOException e) {
            throw new IllegalArgumentException("Ошибка чтения файла: " + e.getMessage(), e);
        } catch (java.text.ParseException e) {
            throw new IllegalArgumentException("Ошибка парсинга даты: " + e.getMessage(), e);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Ошибка парсинга зарплаты: " + e.getMessage(), e);
        }
        return employees;
    }
    
    public String getName() { return name; }
    public Integer getSalary() { return salary; }
    public Date getWorkStart() { return workStart; }
    
    @Override
    public String toString() {
        return name + " — " + salary + " — " + dateFormat.format(workStart);
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        
        Employee employee = (Employee) o;
        return Objects.equals(name, employee.name) &&
               Objects.equals(salary, employee.salary) &&
               Objects.equals(workStart, employee.workStart);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(name, salary, workStart);
    }
}
```
