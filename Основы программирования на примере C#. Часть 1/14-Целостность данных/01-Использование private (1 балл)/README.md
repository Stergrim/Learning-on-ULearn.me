# Использование private

Изучите следующий код:

```cs
class A
{
    private int a;
    public int B;
    public void PublicMethod(){
        a = 1; // строка 1
        B = 2; // строка 2
    }
    private void PrivateMethod(){
        a = 3; // строка 3
        B = 4; // строка 4
    }
}

class B
{
    private A privateA = new A();
    public A PublicA = new A();
   
    public void M(){
        privateA.a = 5; // строка 5
        PublicA.a = 7;  // строка 6
        privateA.B = 6; // строка 7
        PublicA.B = 8;  // строка 8
        PublicA.PublicMethod();   // строка 9
        privateA.PublicMethod();  // строка 10
        PublicA.PrivateMethod();  // строка 11
        privateA.PrivateMethod(); // строка 12
    }
}
```

1. Отметьте корректные строки (номера строк подписаны в комментариях в коде) (1 из 1 балла)
   * ✅ **строка 1** (Правильно!)
   * ✅ **строка 2** (Правильно!)
   * ✅ **строка 3** (Правильно!)
   * ✅ **строка 4** (Правильно!)
   * ❌ **строка 5**
   * ❌ **строка 6**
   * ✅ **строка 7** (Правильно!)
   * ✅ **строка 8** (Правильно!)
   * ✅ **строка 9** (Правильно!)
   * ✅ **строка 10** (Правильно!)
   * ❌ **строка 11**
   * ❌ **строка 12**
