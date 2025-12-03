# Практика. Parser

В этой практике вы научитесь парсить csv-файлы.

Вам необходимо написать консольное приложение:
- приложение должно парсить csv-файл и преобразовывать в элементы POJO-класса;
- метод `getExpenseSum()` — вычисляющий сумму расходов по всем операциям;
- метод `getIncomeSum()` — вычисляющий сумму доходов по операциям;
- метод `getListOfExpenses()`, который выдает расходы по организациям в виде строк;
- также напишите консольное приложение для взаимодействия с кодом.

[movementList.csv](movementList.csv)

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Parser {
    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Укажите путь к CSV-файлу в качестве аргумента");
            return;
        }
        
        String csvFile = args[0];
        Movements movements = new Movements(csvFile);
        
        System.out.println("=== Анализ банковских операций ===");
        System.out.printf("Сумма доходов: %.2f руб.\n", movements.getIncomeSum());
        System.out.printf("Сумма расходов: %.2f руб.\n", movements.getExpenseSum());
        System.out.println();
        
        System.out.println("=== Детализация расходов по организациям ===");
        ArrayList<String> expenses = movements.getListOfExpenses();
        for (String expense : expenses) {
            System.out.println(expense);
        }
    }
}

public class Movements {
    private ArrayList<BankOperation> operations = new ArrayList<>();
    
    public Movements(String path) {
        parseCSVFile(path);
    }
    
    private void parseCSVFile(String path) {
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            String line;
            boolean isFirstLine = true;
            
            while ((line = br.readLine()) != null) {
                if (isFirstLine) {
                    isFirstLine = false;
                    continue;
                }
                
                String[] fields = parseCSVLine(line);
                if (fields.length >= 8) {
                    BankOperation operation = new BankOperation(
                        fields[0],  // тип счета
                        fields[1],  // номер счета
                        fields[2],  // валюта
                        fields[3],  // дата операции
                        fields[4],  // ссылка операции
                        fields[5],  // описание операции
                        parseDouble(fields[6]),  // доход
                        parseDouble(fields[7])   // расход
                    );
                    operations.add(operation);
                }
            }
        } catch (IOException e) {
            System.out.println("Ошибка чтения файла: " + e.getMessage());
        }
    }
    
    private String[] parseCSVLine(String line) {
        ArrayList<String> fields = new ArrayList<>();
        StringBuilder currentField = new StringBuilder();
        boolean inQuotes = false;
        
        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            
            if (c == '\"') {
                inQuotes = !inQuotes;
            } else if (c == ',' && !inQuotes) {
                fields.add(currentField.toString().trim());
                currentField.setLength(0);
            } else {
                currentField.append(c);
            }
        }
        
        fields.add(currentField.toString().trim());
        return fields.toArray(new String[0]);
    }
    
    private double parseDouble(String value) {
        if (value == null || value.isEmpty()) {
            return 0.0;
        }
        
        String cleanedValue = value.replace("\"", "").replace(",", ".");
        
        try {
            return Double.parseDouble(cleanedValue);
        } catch (NumberFormatException e) {
            return 0.0;
        }
    }
    
    public double getExpenseSum() {
        double sum = 0.0;
        for (BankOperation operation : operations) {
            sum += operation.getExpense();
        }
        return sum;
    }
    
    public double getIncomeSum() {
        double sum = 0.0;
        for (BankOperation operation : operations) {
            sum += operation.getIncome();
        }
        return sum;
    }
    
    public ArrayList<String> getListOfExpenses() {
        Map<String, Double> expenseByOrganization = new HashMap<>();
        
        for (BankOperation operation : operations) {
            String organization = extractOrganization(operation.getDescription());
            expenseByOrganization.merge(organization, operation.getExpense(), Double::sum);
        }
        
        ArrayList<String> result = new ArrayList<>();
        for (Map.Entry<String, Double> entry : expenseByOrganization.entrySet()) {
            result.add(String.format("%s -> %.2f руб.", entry.getKey(), entry.getValue()));
        }
        
        return result;
    }
    
    public static String extractOrganization(String description) {
        int startIndex = Math.max(description.indexOf('/'), description.indexOf('\\'));
        int endIndex = -1;
        int spaceCount = 0;
        for (int i = startIndex; i < description.length(); i++) {
            char c = description.charAt(i);
            if (c == ' ') {
                spaceCount++;
                if (spaceCount >= 2) {
                    endIndex = i - spaceCount + 1;
                    break;
                }
            } else spaceCount = 0;
        }
        return description.substring(startIndex, endIndex).trim();
    }
}

public class BankOperation {
    private String accountType;
    private String accountNumber;
    private String currency;
    private String operationDate;
    private String operationReference;
    private String description;
    private double income;
    private double expense;
    
    public BankOperation(String accountType, String accountNumber, String currency,
                        String operationDate, String operationReference, 
                        String description, double income, double expense) {
        this.accountType = accountType;
        this.accountNumber = accountNumber;
        this.currency = currency;
        this.operationDate = operationDate;
        this.operationReference = operationReference;
        this.description = description;
        this.income = income;
        this.expense = expense;
    }
    
    public String getDescription() { return description; }
    public double getIncome() { return income; }
    public double getExpense() { return expense; }
}
```
