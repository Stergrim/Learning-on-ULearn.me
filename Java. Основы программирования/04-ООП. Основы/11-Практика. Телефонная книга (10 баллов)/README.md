# Практика. Телефонная книга

1. Напишите программу, которая будет работать как телефонная книга: Если вводим новое имя, программа просит ввести номер телефона и запоминает его. Если новый номер телефона — просит ввести имя и также запоминает. Если вводим существующее имя или номер телефона, программа выводит телефон(ы) или имя абонента соответственно. При вводе команды **LIST** программа печатает в консоль список всех абонентов в алфавитном порядке с номерами.
2. Определяйте имя и телефон с помощью регулярных выражений. Подумайте, что выбрать в качестве ключа и значения для **Map**, и выберите лучший, по вашему мнению, вариант.
3. Для работы с данными телефонной книги в проекте находится класс **PhoneBook**, который должен отвечать за хранение и работу с абонентами. Реализуйте все методы. Вы можете добавлять дополнительные методы в класс.

*Команды вводятся пользователем в консоль одной строкой. Примеры работы с телефонной книгой (жирным шрифтом выделен ввод пользователя)*

Введите номер, имя или команду:

**Алина**

Такого имени в телефонной книге нет.

Введите номер телефона для абонента “Алина”:

**79001678904**

Контакт сохранен!

- Введите номер, имя или команду:

**89223224567**

Такого номера нет в телефонной книге.

Введите имя абонента для номера “89223224567”:

**Алина**

Контакт сохранен!

- Введите номер, имя или команду:

**LIST**

Алина - 79001678904, 89223224567

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        PhoneBook phoneBook = new PhoneBook();
    
        while (true) {
            System.out.println("Введите номер, имя или команду:");
            String input = scanner.nextLine().trim();
    
            if (input.equalsIgnoreCase("LIST")) {
                Set<String> allContacts = phoneBook.getAllContacts();
                if (allContacts.isEmpty()) {
                    System.out.println("Телефонная книга пуста");
                } else {
                    for (String contact : allContacts) {
                        System.out.println(contact);
                    }
                }
            }
            else if (phoneBook.isName(input)) {
                Set<String> phones = phoneBook.getPhonesByName(input);
                if (phones.isEmpty()) {
                    System.out.println("Такого имени в телефонной книге нет.");
                    System.out.println("Введите номер телефона для абонента \"" + input + "\":");
                    String phone = scanner.nextLine().trim();
                    if (phoneBook.isPhone(phone)) {
                        phoneBook.addContact(phone, input);
                        System.out.println("Контакт сохранен!");
                    } else {
                        System.out.println("Неверный формат номера телефона");
                    }
                } else {
                    System.out.println(input + " - " + phoneBook.getNumbersToString(phones));
                }
            }
            else if (phoneBook.isPhone(input)) {
                String name = phoneBook.getNameByPhone(input);
                if (name.isEmpty()) {
                    System.out.println("Такого номера нет в телефонной книге.");
                    System.out.println("Введите имя абонента для номера \"" + input + "\":");
                    String newName = scanner.nextLine().trim();
                    if (phoneBook.isName(newName)) {
                        phoneBook.addContact(input, newName);
                        System.out.println("Контакт сохранен!");
                    } else {
                        System.out.println("Неверный формат имени");
                    }
                } else {
                    System.out.println(name + " - " + input);
                }
            }
            else {
                System.out.println("Неверный формат ввода");
            }
        }
    }
}

public class PhoneBook {
    private Map<String, String> phoneToName;
    private Map<String, Set<String>> nameToPhones;
    
    public PhoneBook() {
        phoneToName = new HashMap<>();
        nameToPhones = new HashMap<>();
    }
    
    public void addContact(String phone, String name) {
        if (!isCorrect(name, phone)) {
            return;
        }
        
        if (phoneToName.containsKey(phone)) {
            String oldName = phoneToName.get(phone);
            nameToPhones.get(oldName).remove(phone);
            if (nameToPhones.get(oldName).isEmpty()) {
                nameToPhones.remove(oldName);
            }
        }
    
        phoneToName.put(phone, name);
        if (!nameToPhones.containsKey(name)) {
            nameToPhones.put(name, new HashSet<>());
        }
        nameToPhones.get(name).add(phone);
    }
    
    public String getNameByPhone(String phone) {
        String name = phoneToName.get(phone);
        return name != null ? name : "";
    }
    
    public Set<String> getPhonesByName(String name) {
        return nameToPhones.getOrDefault(name, new HashSet<>());
    }
    
    public StringBuilder getNumbersToString(Set<String> numbers) {
        StringBuilder result = new StringBuilder();
    
        for (String number : numbers) {
            if (!result.isEmpty()) {
                result.append(", ");
            }
            result.append(number);
        }
        return result;
    }
    
    public Set<String> getAllContacts() {
        Set<String> contacts = new TreeSet<>();
        for (Map.Entry<String, Set<String>> entry : nameToPhones.entrySet()) {
            String name = entry.getKey();
            Set<String> phones = entry.getValue();
            contacts.add(name + " - " + getNumbersToString(phones));
        }
        return contacts;
    }
    
    public boolean checkContacts(String name) {
        return nameToPhones.containsKey(name);
    }
    
    public boolean isCorrect(String name, String phone) {
        return isName(name) && isPhone(phone);
    }
    
    public boolean isName(String input) {
        return input != null && input.matches("[А-Яа-яA-Za-z\\s]+");
    }
    
    public boolean isPhone(String input) {
        return input != null && input.matches("^(79\\d{9}|89\\d{9}|\\+79\\d{9}|9\\d{9})$");
    }
}
```
