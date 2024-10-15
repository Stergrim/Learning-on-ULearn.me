# Бинарный поиск строк

А вот ещё один классический алгоритм — бинарный поиск. В данном случае он запускается на отсортированном массиве строк размера n. Для простоты считайте, что сами строки и префикс также имеют длину не более n.

```cs
string BinSearch(string[] sortedLines, string prefix)
{
   return BinSearchInRange(sortedLines, prefix, 0, sortedLines.Length - 1);
}

string BinSearchInRange(string[] sortedLines, string prefix, int left, int right)
{
    if (left > right)
        return null;
    int middle = left + (right - left) / 2;
    var middleValue = sortedLines[middle];
    if (middleValue.StartsWith(prefix))
        return middleValue;
    if (middleValue.CompareTo(prefix) < 0)
        return BinSearchInRange(sortedLines, prefix, left, middle-1);
    else
        return BinSearchInRange(sortedLines, prefix, middle+1, right);
}
```

1. Если не спешить и подумать, какая сложность у BinSearch? (1 из 1 балла)
   * 🔴 **Θ(log(n))** (Это было бы верно, если бы вместо строк в массиве были числа)
   * 🔴 **Θ(log(n²))**
   * 🟢 **Θ(n log(n))** (Правильно! Важно не забывать, что сравнение строк — линейная операция!)
   * 🔴 **Θ(n²)**
