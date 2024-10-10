# Практика «Карманный гугл»

[Скачайте проект](PocketGoogle.zip)

В этом проекте вы создадите структуру данных индекса, который используется для быстрого поиска слов в документах.

В файле Indexer.cs реализуйте предложенные методы
- Add. Этот метод должен индексировать все слова в документе. Разделители слов: { ' ', '.', ',', '!', '?', ':', '-','\r','\n' }; Сложность – O(document.Length)
- GetIds. Этот метод должен искать по слову все id документов, где оно встречается. Сложность — O(result), где result — размер ответа на запрос
- GetPositions. Этот метод по слову и id документа должен искать все позиции, в которых слово начинается. Сложность — O(result)
- Remove. Этот метод должен удалять документ из индекса, после чего слова в нем искаться больше не должны. Сложность — O(document.Length)


**Сложность операций с коллекциями**
- Remove, Insert, Contains, IndexOf в List имеют сложность O(n).
- Add в коллекциях Dictionary и List имеет среднюю сложность O(1).
- Доступ по ключу в Dictonary имеет среднюю сложность O(1).
- Доступ по индексу в List имеет сложность O(1).
- Remove, ContainsKey в Dictionary имеют среднюю сложность O(1).


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PocketGoogle
{
    public class Indexer : IIndexer
    {
        private NestedDictionary nestedDictionary = new NestedDictionary() { };
    
        public void Add(int id, string documentText)
        {
            var textWords = documentText.Split(new char[]
                            { ' ', '.', ',', '!', '?', ':', '-', '\r', '\n' });
            int k = 0;
            for (var i = 0; i < textWords.Length; i++)
            {
                nestedDictionary.Add(textWords[i], id, k);
                k += textWords[i].Length + 1;
            }
        }
    
        public List<int> GetIds(string word) { return nestedDictionary.GetIds(word); }
    
        public List<int> GetPositions(int id, string word)
        { return nestedDictionary.GetPositions(id, word); }
    
        public void Remove(int id) { nestedDictionary.Remove(id); }
    }
    
    public class NestedDictionary
    {
        public Dictionary<string, Dictionary<int, List<int>>> Dictionary;
    
        public NestedDictionary()
        { Dictionary = new Dictionary<string, Dictionary<int, List<int>>> { }; }
    
        public void Add(string word, int id, int indexWord)
        {
            if (!Dictionary.ContainsKey(word))
            {
                Dictionary.Add(word, new Dictionary<int, List<int>> { });
                var listIndex = new List<int> { };
                Dictionary[word].Add(id, listIndex);
                Dictionary[word][id].Add(indexWord);
            }
            else if (!Dictionary[word].ContainsKey(id))
            {
                var listIndex = new List<int> { };
                Dictionary[word].Add(id, listIndex);
                Dictionary[word][id].Add(indexWord);
            }
            else Dictionary[word][id].Add(indexWord);
        }
    
        public List<int> GetIds(string word)
        {
            var listId = new List<int> { };
            if (!Dictionary.ContainsKey(word)) return listId;
            foreach (var dict in Dictionary[word])
                listId.Add(dict.Key);
            return listId;
        }
    
        public List<int> GetPositions(int id, string word)
        {
            if (!Dictionary.ContainsKey(word)) return new List<int> { };
            if (!Dictionary[word].ContainsKey(id)) return new List<int> { };
            return Dictionary[word][id];
        }
    
        public void Remove(int id)
        {
            foreach (var word in Dictionary.Keys)
                Dictionary[word].Remove(id); 
        }
    }
}
```
