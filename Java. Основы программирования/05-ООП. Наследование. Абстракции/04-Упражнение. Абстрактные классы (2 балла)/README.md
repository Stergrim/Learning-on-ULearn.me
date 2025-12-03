# Упражнение. Абстрактные классы

1. Создайте абстрактный класс `Animal`, который содержит:
   - приватные поля **name** и **age**,
   - методы `toString`, `getName`, `getAge`, возвращающие значение полей **name** и **age**,
   - конструктор принимающий **name** и **age**.
2. Создайте классы `Lion` и `Monkey`, наследующие класс `Animal` и имеющие дополнительные поля и методы:
   - `Monkey`: **color** и **getColor**
   - `Lion`: **bodyLength** и **getBodyLength**
   - Также каждый из этих классов добавляет свой параметр в строке возвращаемой методом `toString()` *Например*: Класс `Lion` добавит к строке "*, bodyLength= 5*".
3. Создайте класс `Zoo`, содержащий в себе:
   - список животных,
   - метод `add`, принимающий любой класс, который наследует `Animal`,
   - метод `getSize`, возвращающий количество животных,
   - метод `getReport`, возвращающий номер каждого животного и его строковое представление в новой строке.


**Все тесты пройдены, задача сдана:**
```java
public abstract class Animal {
    private String name;
    private int age;
    
    public Animal(String name, int age) {
        this.name = name;
        this.age = age;
    }
    
    public String getName() {
        return name;
    }
    
    public int getAge() {
        return age;
    }
    
    @Override
    public String toString() {
        return "'" + name + "', age= " + age;
    }
}

public class Lion extends Animal {
    private double bodyLength;
    
    public Lion(String name, int age, double bodyLength) {
        super(name, age);
        this.bodyLength = bodyLength;
    }
    
    public double getBodyLength() {
        return bodyLength;
    }
    
    @Override
    public String toString() {
        return super.toString() + ", bodyLength= " + bodyLength;
    }
}

public class Monkey extends Animal {
    private String color;
    
    public Monkey(String name, int age, String color) {
        super(name, age);
        this.color = color;
    }
    
    public String getColor() {
        return color;
    }
    
    @Override
    public String toString() {
        return super.toString() + ", color= " + color;
    }
}

public class Zoo {
    private List<Animal> animals;
    
    public Zoo() {
        this.animals = new ArrayList<>();
    }
    
    public void add(Animal animal) {
        animals.add(animal);
    }
    
    public int getSize() {
        return animals.size();
    }
    
    public String getReport() {
        StringBuilder report = new StringBuilder();
        for (int i = 0; i < animals.size(); i++) {
            report.append(i + 1)
                  .append(" ")
                  .append(animals.get(i).toString())
                  .append("\n");
        }
        return report.toString();
    }
}
```
