# Практика. Классы

**Необходимо реализовать следующие классы:**
- Класс `Human` — родитель классов **Pupil** и **Teacher**, который будет содержать общие для каждого из них поля и методы.
- Класс `Pupil` — поле имени, фамилии ученика, год поступления в школу, геттеры и конструктор, метод toString, имя и фамилия должны быть **final**.
- Класс `Teacher` указать ФИ преподавателя, предмет, который он преподает и его стаж работы, конструктор, метод toString.
- Класс `School` реализовать методы прибытия ученика/учителя в школу, ухода из школы, функции печати в консоль, учеников и учителей находящихся в школе.


**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class School {
    private List<Human> peopleInSchool = new ArrayList<>();
    
    public String getPeoplesInSchool() {
        StringBuilder result = new StringBuilder("В школе:\n");
        
        for (Human human : peopleInSchool) {
            if (human.isInSchool()) {
                result.append(human.toString());
                result.append("\n");
            }
        }
        
        return result.toString();
    }
    
    public void add(Human human) {
        peopleInSchool.add(human);
    }
}

public abstract class Human {
    private String firstName;
    private String lastName;
    private boolean inSchool = false;
    
    public Human(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }
    
    public String getName() {
        return firstName;
    }
    
    public String getSurname() {
        return lastName;
    }
    
    public void goInSchool() {
        this.inSchool = true;
    }
    
    public void outFromSchool() {
        this.inSchool = false;
    }
    
    public boolean isInSchool() {
        return inSchool;
    }
}

public class Pupil extends Human {
    private int admissionYear;
    
    public Pupil(String firstName, String lastName, int admissionYear) {
        super(firstName, lastName);
        this.admissionYear = admissionYear;
    }
    
    public int getYear() {
        return admissionYear;
    }
    
    @Override
    public String toString() {
        return getName() + " " + getSurname() + " " + admissionYear;
    }
}

public class Teacher extends Human {
    private String subject;
    private int experience;
    
    public Teacher(String firstName, String lastName, String subject, int experience) {
        super(firstName, lastName);
        this.subject = subject;
        this.experience = experience;
    }
    
    public String getSubject() {
        return subject;
    }
    
    public int getExperience() {
        return experience;
    }
    
    @Override
    public String toString() {
        return getName() + " " + getSurname() + " " + subject + " " + experience;
    }
}
```
