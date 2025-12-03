# Практика. Список дел

Разработайте программу — список дел, который управляется командами в консоли. Команды: LIST, ADD, EDIT, DELETE. Для работы с данными списка дел в проекте находится класс TodoList, который должен отвечать за хранение и работу со списком дел. Реализуйте все методы. В классе Main напишите код для реализации взаимодействия с пользователем через ввод команд в консоль.

Принцип работы команд:
- **LIST** — выводит дела с их порядковыми номерами;
- **ADD** — добавляет дело в конец списка или дело на определенное место сдвигая остальные дела вперёд, если указать номер;
- **EDIT** — заменяет дело с указанным номером, если указан несуществующий индекс - ничего не делать;
- **DELETE** — удаляет дело из списка, если указан несуществующий индекс - ничего не делать.


**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class TodoList {
    private ArrayList<String> todos = new ArrayList<>();
    
    public static void main(String[] args) {
        TodoList todoList = new TodoList();
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Добро пожаловать в список дел!");
        System.out.println("Доступные команды: LIST, ADD, EDIT, DELETE, EXIT");
        
        while (true) {
            System.out.print("Введите команду: ");
            String input = scanner.nextLine().trim();
            
            if (input.equalsIgnoreCase("EXIT")) {
                System.out.println("До свидания!");
                break;
            }
            
            String[] parts = input.split("\\s+", 3);
            String command = parts[0].toUpperCase();
            
            try {
                switch (command) {
                    case "LIST":
                        ArrayList<String> todos = todoList.getTodos();
                        if (todos.isEmpty()) {
                            System.out.println("Список дел пуст");
                        } else {
                            for (int i = 0; i < todos.size(); i++) {
                                System.out.println((i + 1) + ". " + todos.get(i));
                            }
                        }
                        break;
                        
                    case "ADD":
                        if (parts.length < 2) {
                            System.out.println("Ошибка: укажите дело для добавления");
                            break;
                        }
                        
                        if (parts.length == 2) {
                            todoList.add(parts[1]);
                            System.out.println("Дело добавлено");
                        } else {
                            try {
                                int index = Integer.parseInt(parts[1]);
                                todoList.add(index - 1, parts[2]);
                                System.out.println("Дело добавлено на позицию " + index);
                            } catch (NumberFormatException e) {
                                System.out.println("Ошибка: некорректный номер");
                            }
                        }
                        break;
                        
                    case "EDIT":
                        if (parts.length < 3) {
                            System.out.println("Ошибка: укажите номер и новое дело");
                            break;
                        }
                        
                        try {
                            int index = Integer.parseInt(parts[1]);
                            todoList.edit(parts[2], index - 1);
                            System.out.println("Дело обновлено");
                        } catch (NumberFormatException e) {
                            System.out.println("Ошибка: некорректный номер");
                        }
                        break;
                        
                    case "DELETE":
                        if (parts.length < 2) {
                            System.out.println("Ошибка: укажите номер дела для удаления");
                            break;
                        }
                        
                        try {
                            int index = Integer.parseInt(parts[1]);
                            todoList.delete(index - 1);
                            System.out.println("Дело удалено");
                        } catch (NumberFormatException e) {
                            System.out.println("Ошибка: некорректный номер");
                        }
                        break;
                        
                    default:
                        System.out.println("Неизвестная команда. Доступные команды: LIST, ADD, EDIT, DELETE, EXIT");
                }
            } catch (Exception e) {
                System.out.println("Произошла ошибка: " + e.getMessage());
            }
        }
        
        scanner.close();
    }
    
    public void add(String todo) {
        todos.add(todo);
    }
    
    public void add(int index, String todo) {
        if (index >= 0 && index <= todos.size()) {
            todos.add(index, todo);
        } else {
            System.out.println("Ошибка: невозможно добавить дело на позицию " + (index + 1));
        }
    }
    
    public void edit(String todo, int index) {
        if (index >= 0 && index < todos.size()) {
            todos.set(index, todo);
        } else {
            System.out.println("Ошибка: дело с номером " + (index + 1) + " не существует");
        }
    }
    
    public void delete(int index) {
        if (index >= 0 && index < todos.size()) {
            todos.remove(index);
        } else {
            System.out.println("Ошибка: дело с номером " + (index + 1) + " не существует");
        }
    }
    
    public ArrayList<String> getTodos() {
        return new ArrayList<>(todos);
    }
}
```
