# Особые случаи yield return

Изучите следующий код:

```cs
IEnumerable<int> GetNumbers(int n){
    if (n < 0) throw new ArgumentException("n < 0");
    if (n == 0) return new int[0];
    yield return 0;
    for(int i = 1; i < n; i++)
        if (i % 10000 == 0) break;
        else yield return i;
    yield break;
}
```

1. Что не так с этим кодом? (1 из 1 балла)
   * ✅ **Нельзя в одном методе использовать и обычный return и yield return** (Правильно!)
   * ✅ **yield break в конце метода не нужен** (Правильно!)
   * ❌ **Нельзя в одном методе смешивать yield return в цикле и yield return вне цикла** 
   * ❌ **Нельзя бросать исключения в методе, использующем yield return** (Можно. Но такое исключение будет выброшено лишь при обращении к очередному элементу коллекции. В данном случае — к первому элементу.)
   * ❌ **Нельзя в одном цикле смешивать yield break и обычный break** 
