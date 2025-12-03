# Практика. Модульные тесты

В этой задаче вам будет необходимо написать модульные тесты для задачи **«11.1 Runnable»** из прошлого модуля.

Напишите не менее 5 различных тестов, которые будут покрывать разные «куски» кода, а также один общий тест, который будет проверять работоспособность кода в целом.

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class UnitTestS02 {
    private Bank bank;
    private static final int ACCOUNT_COUNT = 10;
    private static final long INITIAL_BALANCE = 200000;
    
    @org.junit.Before
    public void setUp() {
        bank = new Bank();
        bank.setAccounts(ACCOUNT_COUNT);
    }
    
    @Test
    public void testAccountCreation() {
        System.out.println("Тест 1: Проверка создания счетов");
        long totalBalance = bank.getSumAllAccounts();
        org.junit.Assert.assertEquals("Общий баланс должен быть равен 10 * 200000",
                ACCOUNT_COUNT * INITIAL_BALANCE, totalBalance);
    
        for (int i = 0; i < ACCOUNT_COUNT; i++) {
            org.junit.Assert.assertEquals("Баланс счета " + i + " должен быть 200000",
                    INITIAL_BALANCE, bank.getBalance(String.valueOf(i)));
        }
    }
    
    @Test
    public void testSimpleTransfer() {
        System.out.println("Тест 2: Простой перевод между счетами");
        long transferAmount = 10000;
        bank.transfer("0", "1", transferAmount);
    
        org.junit.Assert.assertEquals("Баланс счета отправителя должен уменьшиться",
                INITIAL_BALANCE - transferAmount, bank.getBalance("0"));
        org.junit.Assert.assertEquals("Баланс счета получателя должен увеличиться",
                INITIAL_BALANCE + transferAmount, bank.getBalance("1"));
    
        long totalAfterTransfer = bank.getSumAllAccounts();
        org.junit.Assert.assertEquals("Общий баланс должен остаться неизменным",
                ACCOUNT_COUNT * INITIAL_BALANCE, totalAfterTransfer);
    }
    
    @Test
    public void testTransferInsufficientFunds() {
        System.out.println("Тест 3: Перевод на сумму больше баланса");
        long initialBalance0 = bank.getBalance("0");
        long initialBalance1 = bank.getBalance("1");
    
        long transferAmount = INITIAL_BALANCE + 100000;
        bank.transfer("0", "1", transferAmount);
    
        org.junit.Assert.assertEquals("Баланс отправителя не должен измениться при недостатке средств",
                initialBalance0, bank.getBalance("0"));
        org.junit.Assert.assertEquals("Баланс получателя не должен измениться при недостатке средств",
                initialBalance1, bank.getBalance("1"));
    }
    
    @Test
    public void testTransferToSameAccount() {
        System.out.println("Тест 4: Перевод на тот же счет");
        long initialBalance = bank.getBalance("0");
        bank.transfer("0", "0", 10000);
    
        org.junit.Assert.assertEquals("Баланс не должен измениться при переводе на тот же счет",
                initialBalance, bank.getBalance("0"));
    }
    
    @Test(timeout = 10000)
    public void testFullSystemIntegration() throws InterruptedException {
        System.out.println("Общий тест: Полная работоспособность системы");
    
        long initialTotal = bank.getSumAllAccounts();
        org.junit.Assert.assertEquals("Начальный баланс должен быть правильным",
                ACCOUNT_COUNT * INITIAL_BALANCE, initialTotal);
    
        bank.transfer("0", "1", 10000);
        bank.transfer("1", "2", 20000);
        bank.transfer("2", "3", 30000);
        bank.transfer("3", "4", 40000);
        bank.transfer("4", "5", 50000);
    
        long totalAfterSequence = bank.getSumAllAccounts();
        org.junit.Assert.assertEquals("Общий баланс должен сохраняться после последовательных переводов",
                initialTotal, totalAfterSequence);
    
        java.util.concurrent.ExecutorService executor = java.util.concurrent.Executors.newFixedThreadPool(5);
        java.util.concurrent.CountDownLatch latch = new java.util.concurrent.CountDownLatch(5);
    
        for (int i = 0; i < 5; i++) {
            final int threadId = i;
            executor.submit(() -> {
                Random random = new Random();
                for (int j = 0; j < 20; j++) {
                    try {
                        int from = random.nextInt(ACCOUNT_COUNT);
                        int to = random.nextInt(ACCOUNT_COUNT);
                        if (from != to) {
                            long amount = 1000 + random.nextInt(49000);
                            bank.transfer(String.valueOf(from), String.valueOf(to), amount);
                        }
                    } catch (Exception e) { }
                }
                latch.countDown();
            });
        }
    
        latch.await(5, java.util.concurrent.TimeUnit.SECONDS);
        executor.shutdown();
    
        long finalTotal = bank.getSumAllAccounts();
        org.junit.Assert.assertEquals("Общий баланс должен сохраняться после всех операций",
                initialTotal, finalTotal);
    
        for (int i = 0; i < ACCOUNT_COUNT; i++) {
            long balance = bank.getBalance(String.valueOf(i));
            org.junit.Assert.assertTrue("Баланс не может быть отрицательным: счет " + i, balance >= 0);
            org.junit.Assert.assertTrue("Баланс не может превышать общую сумму: счет " + i,
                    balance <= ACCOUNT_COUNT * INITIAL_BALANCE);
        }
    
        System.out.println("Общий тест пройден успешно!");
        System.out.println("Начальный баланс: " + initialTotal);
        System.out.println("Конечный баланс: " + finalTotal);
    }
    
    @org.junit.After
    public void tearDown() {
        bank = null;
    }
}
```
