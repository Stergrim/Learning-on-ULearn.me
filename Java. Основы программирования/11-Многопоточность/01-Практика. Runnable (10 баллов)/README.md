# Практика. Runnable

В банке (класс `Bank`) есть счета (класс `Account`) с двумя полями — **money** и **accNumber**.

Все счета хранятся внутри банка. Множество клиентов банка могут одновременно переводить деньги между счетами и запрашивать баланс по своему счёту. Всё происходит в многопоточной среде.

При этом транзакции на суммы > 50000 отправляются на проверку в службу безопасности. Можно считать, что таких транзакций не более 5% от всех. За проверку отвечает отдельный и уже реализованный метод `Bank.isFraud()`.

Служба безопасности не может обрабатывать более одной транзакции одновременно. Проверка занимает 1000 мс.

Если служба безопасности обнаружила мошенничество, необходимо заблокировать оба счёта, то есть запретить любые изменения остатков в дальнейшем.

Что нужно сделать:
- Создайте метод `transfer()` класса `Bank`, который переводит деньги с одного счёта на другой. Если сумма транзакции > 50000 — транзакция отправляется на проверку службе безопасности: вызывается метод `isFraud()`. Если возвращается **true**, то счета блокируются (как – на ваше усмотрение).
- Создайте метод `getBalance()` класса `Bank`, который возвращает остаток на счёте по переданной строке номера аккаунта.
- Создайте метод `getSumAllAccounts()` класса `Bank`, который возращает остаток со всех счетов, которые находятся в банке.
- Создайте метод `setAccounts()` класса `Bank`, который генерирует случайные аккаунты, с **accNumber** равным его индексу и начальным **cash** = 200000.
- Реализуйте интерфейс `Runnable` в классе `TransferRun`, который будет брать случайный аккаунт из пула аккаунтов `Bank` и производить операцию на другой случайный аккаунт.


**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Bank {
    private Map<String, Account> accounts;
    private final Random random = new Random();
    private final java.util.concurrent.locks.Lock fraudCheckLock = new java.util.concurrent.locks.ReentrantLock();
    
    public Bank() {
        this.accounts = new java.util.concurrent.ConcurrentHashMap<>();
    }
    
    public synchronized boolean isFraud(String fromAccountNum, String toAccountNum, long amount)
            throws InterruptedException {
        Thread.sleep(1000);
        return random.nextBoolean();
    }
    
    public void transfer(String fromAccountNum, String toAccountNum, long amount) {
        if (fromAccountNum == null || toAccountNum == null ||
                fromAccountNum.equals(toAccountNum) || amount <= 0) {
            return;
        }
    
        Account fromAccount = accounts.get(fromAccountNum);
        Account toAccount = accounts.get(toAccountNum);
    
        if (fromAccount == null || toAccount == null) return;
    
        Account firstLock = fromAccount;
        Account secondLock = toAccount;
    
        if (fromAccountNum.compareTo(toAccountNum) > 0) {
            firstLock = toAccount;
            secondLock = fromAccount;
        }
    
        synchronized (firstLock) {
            synchronized (secondLock) {
                if (fromAccount.isBlocked() || toAccount.isBlocked()) return;
                if (fromAccount.getMoney() < amount) return;
    
                fromAccount.setMoney(fromAccount.getMoney() - amount);
                toAccount.setMoney(toAccount.getMoney() + amount);
    
                if (amount > 50000) {
                    fraudCheckLock.lock();
                    try {
                        boolean isFraud = false;
                        try {
                            isFraud = isFraud(fromAccountNum, toAccountNum, amount);
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
    
                        if (isFraud) {
                            fromAccount.setBlocked(true);
                            toAccount.setBlocked(true);
                            fromAccount.setMoney(fromAccount.getMoney() + amount);
                            toAccount.setMoney(toAccount.getMoney() - amount);
                        }
                    } finally { fraudCheckLock.unlock(); }
                }
            }
        }
    }
    
    public long getBalance(String accountNum) {
        Account account = accounts.get(accountNum);
        if (account == null) return 0;
        synchronized (account) {
            return account.getMoney();
        }
    }
    
    public long getSumAllAccounts() {
        long total = 0;
        List<Account> accountList = new ArrayList<>(accounts.values());
        accountList.sort(Comparator.comparing(Account::getAccNumber));
    
        for (Account account : accountList) {
            synchronized (account) {
                total += account.getMoney();
            }
        }
        return total;
    }
    
    public void setAccounts(int count) {
        accounts.clear();
        for (int i = 0; i < count; i++) {
            String accNumber = String.valueOf(i);
            Account account = new Account();
            account.setAccNumber(accNumber);
            account.setMoney(200000);
            account.setBlocked(false);
            accounts.put(accNumber, account);
        }
    }
    
    public String getRandomAccountNumber() {
        if (accounts.isEmpty()) {
            return null;
        }
        List<String> keys = new ArrayList<>(accounts.keySet());
        return keys.get(random.nextInt(keys.size()));
    }
}

public class Account {
    private long money;
    private String accNumber;
    private boolean blocked;
    
    public long getMoney() { return money; }
    public void setMoney(long money) { this.money = money; }
    public String getAccNumber() { return accNumber; }
    public void setAccNumber(String accNumber) { this.accNumber = accNumber; }
    public boolean isBlocked() { return blocked; }
    public void setBlocked(boolean blocked) { this.blocked = blocked; }
}

public class TransferRun implements Runnable {
    private static Bank bank;
    private int count;
    private int operationsCount;
    
    public TransferRun(Bank bank, int count, int operationsCount) {
        TransferRun.bank = bank;
        this.count = count;
        this.operationsCount = operationsCount;
    }
    
    @Override
    public void run() {
        Random random = new Random();
    
        for (int i = 0; i < operationsCount; i++) {
            String fromAcc = bank.getRandomAccountNumber();
            String toAcc = bank.getRandomAccountNumber();
    
            while (toAcc != null && toAcc.equals(fromAcc)) {
                toAcc = bank.getRandomAccountNumber();
            }
    
            if (fromAcc != null && toAcc != null) {
                long amount = random.nextInt(100000) + 1;
                bank.transfer(fromAcc, toAcc, amount);
                try {
                    Thread.sleep(random.nextInt(10));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }
        }
    }
}
```
