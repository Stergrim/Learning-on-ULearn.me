# Практика. Customers

Необходимо создать класс `Customer`:
- Добавить **приватные неизменяемые** поля `name`, `surname`, `email`, `phone`.
- Добавить один конструктор для всех этих полей.
- Написать методы `getName`, `getSurname`, `getPhone`, `getEmail`, возвращающие соответственно значения этих полей.
- Также добавить метод `toString`, возвращающий строку типа: "`name` - `surname` - `email` - `phone`"

Необходимо создать класс `CustomerStorage` со следующими методами:
- `addCustomer` - принимает строку с данными типа "`name` `surname` `email` `phone`" и создает объект типа `Customer`.
- `listCustomers` - выводит в консоль список всех клиентов.
- `removeCustomer` - принимает на вход строку вида: "`name` `surname`" и удаляет клиента по его имени и фамилии.
- `getCustomer` - принимает на вход строку вида: "`name` `surname`" и возвращает объект класса `Customer` по его имени и фамилии.
- `getCount` - возвращает количество клиентов.

Необходимо создать класс `Main` и в методе **main** написать консольное приложение для взаимодействия с продавцами. Приложение должно принимать команды вида: "*command name surname email phone*";

Команды могут быть следующими:
- `add` - добавить клиента.
- `list` - вывести список всех клиентов.
- `remove` - удалить клиента.
- `count` - подсчитать всех клиентов.
- `help` - выводит пример команды.

*Также позаботьтесь об обработке всех ошибок которые могут возникнуть, обратите внимание — вся проверка должна находится в методах.*

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        CustomerStorage customerStorage = new CustomerStorage();
        System.out.println("Для взаимодействия с консольным приложением используйте следующий набор команд ");
        System.out.println("Команды:\n" +
                "add - добавить клиента\n"+
                "list - вывести список всех клиентов\n"+
                "remove - удалить клиента\n"+
                "count - подсчитать всех клиентов\n"+
                "help - выводит пример команды\n"+
                "exit - завершает работу приложения");
        String command = scanner.nextLine();
        while(!command.equals("exit")){
            try {
                if (command.substring(0, 3).equals("add")) {
                    customerStorage.addCustomer(command.substring(4));
                }
                else if (command.substring(0, 4).equals("list")) {
                    System.out.println(customerStorage.listCustomers());
                }
                else if (command.substring(0, 4).equals("help")) {
                    String helpCom = command.split(" ")[1];
                    if (helpCom.equals("add") || helpCom.equals("remove")) {
                        System.out.println("Пример использования: [name_command] [name] [surname] [email] [phone]");
                    }
                }
                else if (command.substring(0, 5).equals("count")) {
                    System.out.println(customerStorage.getCount());
                }
                else if (command.substring(0, 6).equals("remove")) {
                    customerStorage.removeCustomer(command.split(" ")[1] + " " + command.split(" ")[2]);
                }
            }catch (Exception t){
                System.out.println("Введите корректные значения!");
            }
            command = scanner.nextLine();
        }
    }
}

public class Customer {
    private final String name;
    private final String phone;
    private final String email;
    private final String surname;
    
    public String getSurname(){ return surname; }
    public String getEmail() { return email; }
    public String getPhone() { return phone; }
    public String getName() { return name; }
    
    public Customer(String name,String surname,String phone, String email) {
        this.name = name;
        this.phone = phone;
        this.email = email;
        this.surname = surname;
    }
    
    public String toString(){
        return String.format("%s - %s - %s - %s",name,surname,email,phone);
    }
}

public class CustomerStorage {
    private ArrayList<Customer> customers = new ArrayList<>();
    
    public void addCustomer(String input){
        try{
            String[] values = input.split(" ");
            if (values.length!=4
                    || !isCorrectPhone(values[3])
                    || !isCorrectEmail(values[2]))
                throw new IllegalArgumentException();
            customers.add(new Customer(values[0],values[1],values[3],values[2]));
        }catch (Exception e){
            throw new IllegalArgumentException();
        }
    }
    
    public String listCustomers(){
        StringBuilder text = new StringBuilder();
        customers.stream().forEach(x->text.append(x.toString()+"\n"));
        return text.toString();
    }
    
    public void removeCustomer(String input){
        String[] values = input.split(" ");
        for(Customer customer : customers){
            if (customer.getName().equals(values[0]) & customer.getSurname().equals(values[1])){
                customers.remove(customer);
                return;
            }
        }
        throw new IllegalArgumentException();
    }
    
    public Customer getCustomer(String input){
        String[] values = input.split(" ");
        for(Customer customer : customers){
            if (customer.getName().equals(values[0]) & customer.getSurname().equals(values[1])){
                return customer;
            }
        }
        throw new IllegalArgumentException();
    }
    
    public int getCount(){
        return customers.size();
    }
    
    public boolean isCorrectPhone(String phone) {
        return phone.matches("((\\+7)?||8?)+\\d{10}");
    }
    
    public boolean isCorrectEmail(String email){
        return email.matches("^[A-Za-z0-9+_.-]+@(.+)$");
    }
}
```
