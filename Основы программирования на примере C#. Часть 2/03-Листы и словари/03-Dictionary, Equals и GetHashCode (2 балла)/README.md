# Dictionary, Equals и GetHashCode

1. Что вы можете сказать о словаре Dictionary? (1 из 1 балла)
   * ✅ **Словарь позволяет эффективно проверить, содержит ли он ключ** (Правильно!)
   * ❌ **Словарь позволяет эффективно проверить, содержит ли он значения**
   * ✅ **Для каждого ключа словарь хранит только одно значение** (Правильно!)
   * ❌ **По разным ключам словарь не может хранить одно и то же значение**


Изучите следующий код:

```cs
public class Man {

    public string Surname {get;set;}
    public string Name {get;set;}
    public string Patronymic {get;set;}
    public int Age {get;set;}
   
    public override bool Equals(object obj) {
   
        if (!(obj is Man)) return false;
        var man = obj as Man;
        return Surname == man.Surname && Name == man.Name && Patronymic == man.Patronymic;	
    }
   
    public override int GetHashCode() {
        #вариант ответа#;
    }
}
```

2. Выберите корректные варианты реализации GetHashCode. Считайте, что Name, Surname, Patronymic не могут быть null (1 из 1 балла)
   * ❌ **return new Random().NextInt()**
   * ✅ **return 42** (Правильно!)
   * ❌ **return Age.GetHashCode()**
   * ✅ **return Surname.GetHashCode()** (Правильно!)
   * ✅ **return Surname.GetHashCode() * 31 + Name.GetHashCode()** (Правильно!)
   * ✅ **return (Surname.GetHashCode() * 31 + Name.GetHashCode()) * 31 + Patronymic.GetHashCode()** (Правильно!)
   * ❌ **return Age.GetHashCode() * 31 + Surname.GetHashCode()**
