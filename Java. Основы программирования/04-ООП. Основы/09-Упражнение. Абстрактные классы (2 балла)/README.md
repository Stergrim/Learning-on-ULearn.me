# Упражнение. Абстрактные классы

Напишите классы абстрактный класс `Client` с методами `getAmount()`, `put()` и `take()`. А также классы наследники:
- У **PhysicalPerson** пополнение и списание происходит без комиссии. Если передать в метод пополнения отрицательное значение, сумма на счёте не должна измениться. При попытке снять сумму больше, чем есть на счете, сумма не списывается со счёта, сумма на счёте не изменяется.
- У **LegalPerson** — все условия **PhysicalPerson** и дополнительно снятие с комиссией 1%.
- У **IndividualBusinessman** — все условия **PhysicalPerson** и дополнительно, пополнение с комиссией 1%, если сумма меньше 1 000 рублей. И пополнение с комиссией 0,5%, если сумма больше либо равна 1 000 рублей.


**Все тесты пройдены, задача сдана:**
```java
public abstract class Client {
    protected double amount;
    
    public Client() {
        this.amount = 0.0;
    }
    
    public abstract void put(double amount);
    public abstract void take(double amount);
    
    public double getAmount() {
        return amount;
    }
}

public class PhysicalPerson extends Client {
    
    @Override
    public void put(double amount) {
        if (amount > 0) {
            this.amount += amount;
        }
    }
    
    @Override
    public void take(double amount) {
        if (amount > 0 && amount <= this.amount) {
            this.amount -= amount;
        }
    }
}

public class LegalPerson extends Client {
    
    @Override
    public void put(double amount) {
        if (amount > 0) {
            this.amount += amount;
        }
    }
    
    @Override
    public void take(double amount) {
        if (amount > 0 && amount <= this.amount) {
            double commission = amount * 0.01;
            this.amount -= (amount + commission);
        }
    }
}

public class IndividualBusinessman extends Client {
    
    @Override
    public void put(double amount) {
        if (amount > 0) {
            double commission;
            if (amount < 1000) {
                commission = amount * 0.01;
            } else {
                commission = amount * 0.005;
            }
            this.amount += (amount - commission);
        }
    }
    
    @Override
    public void take(double amount) {
        if (amount > 0 && amount <= this.amount) {
            this.amount -= amount;
        }
    }
}
```
