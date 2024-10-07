# Практика «Частотность N-грамм»

Продолжайте работу в [том же проекте.](TextAnalysis.zip)

N-грамма — это N соседних слов в одном предложении. 2-граммы называют биграммами. 3-граммы — триграммами.

Например, из текста: "She stood up. Then she left." можно выделить следующие биграммы "she stood", "stood up", "then she" и "she left", но не "up then". И две триграммы "she stood up" и "then she left", но не "stood up then".

По списку предложений, составленному в прошлой задаче, составьте словарь самых частотных продолжений биграмм и триграмм. Это словарь, ключами которого являются все возможные начала биграмм и триграмм, а значениями — их самые частотные продолжения. Если есть несколько продолжений с одинаковой частотой, используйте то, которое лексикографически меньше.

Для лексикографического сравнения используйте встроенный в .NET способ сравнения Ordinal, например, с помощью метода string.CompareOrdinal.

Такой словарь назовём N-граммной моделью текста.

Реализуйте этот алгоритм в классе `FrequencyAnalysisTask`.

Все вопросы и детали уточняйте с помощью примера ниже и тестов.

**Пример**

По тексту `a b c d. b c d. e b c a d`. должен быть составлен такой словарь:

```cs
"a": "b"
"b": "c"
"c": "d"
"e": "b"
"a b": "c"
"b c": "d"
"e b": "c"
"c a": "d"
```

Обратите внимание:
- из двух биграмм "a b" и "a d", встречающихся однократно, в словаре есть только пара "a": "b", как лексикографически меньшая.
- из двух встречающихся в тексте биграмм "c d" и "c a" в словаре есть только более частотная пара "c": "d".
- из двух триграмм "b c d" и "b c a" в словаре есть только более частотная "b c": "d".


Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;

namespace TextAnalysis
{
    static class FrequencyAnalysisTask
    {
        public static Dictionary<string, string> GetMostFrequentNextWords(List<List<string>> text)
        {
            var result = new Dictionary<string, string>();
            var temp = new Dictionary<string, Dictionary<string, int>>();
            int length = text.Count;
            
            MakeBigrams(text, result, temp, length);
            MakeTrigrams(text, result, temp, length);

            foreach (var dic in temp)
            {
                int max = 0;
                string kmax = null;
                foreach (var key in dic.Value)
                {
                    if (key.Value > max) { max = key.Value; kmax = key.Key; }
                    if (key.Value == max)
                        if (string.CompareOrdinal(kmax, key.Key) > 0) kmax = key.Key;
                }
                result[dic.Key] = kmax;
            }
            return result;
         }
		
         public static void MakeBigrams(List<List<string>> text, Dictionary<string, string> result, Dictionary<string, Dictionary<string, int>> temp, int length)
         {
            for (int i = 0; i < length; i++)
            {
                int lengthBi = text[i].Count - 1;
                for (int j = 0; j < lengthBi; j++)
                {
                    if (!temp.ContainsKey(text[i][j]))
                        temp[text[i][j]] = new Dictionary<string, int>() { { text[i][j + 1], 1 } };
                    else if (!temp[text[i][j]].ContainsKey(text[i][j + 1]))
                        temp[text[i][j]].Add(text[i][j + 1], 1);
                    else temp[text[i][j]][text[i][j + 1]] += 1;
                }
            }
         }
		
        public static void MakeTrigrams(List<List<string>> text, Dictionary<string, string> result, Dictionary<string, Dictionary<string, int>> temp, int length)
        {
            for (int i = 0; i < length; i++)
            {
                int lengthThe = text[i].Count - 2;
                for (int j = 0; j < lengthThe; j++)
                {
                    if (!temp.ContainsKey(text[i][j] + " " + text[i][j + 1]))
                        temp[text[i][j]+" "+ text[i][j+1]] = new Dictionary<string, int>() { { text[i][j + 2], 1 } };
                    else if (!temp[text[i][j] + " " + text[i][j + 1]].ContainsKey(text[i][j + 2]))
                        temp[text[i][j] + " " + text[i][j + 1]].Add(text[i][j + 2], 1);
                    else temp[text[i][j] + " " + text[i][j + 1]][text[i][j + 2]] += 1;
                }
            }
        }
   }
}
```