# Практика «Антиплагиат»

ИТ-компания К. приглашает студентов на летнюю стажировку. Чтобы попасть на стажировку, претенденты решают тестовое задание — задачу на программирование вроде тех, что есть в этом курсе, только сложнее.

Из года в год претенденты присылают несколько сотен решений. Можно ли как-то автоматически найти среди них «списанные» решения, то есть такие, которые слишком сильно похожи друг на друга?

Оказывается расстояние Левенштейна можно использовать для того, чтобы сравнивать листинги программ (или вообще любые документы) друг с другом и находить самые похожие пары. Этим вам и предстоит заняться в данной задаче.

Скачайте проект [Antiplagiarism](Antiplagiarism.zip).

В этой задаче вам необходимо реализовать класс `LevenshteinCalculator`, который получает на вход список документов и возвращает список попарных сравнений каждого документа с каждым другим.

Мы хотим, чтобы разница в пробелах, пустых строках или небольшом переименовании переменных не сбивала наш алгоритм. Поэтому вам нужно реализовать модифицированный алгоритм Левенштейна:

1. Он должен анализировать не последовательности символов, а последовательности **токенов** — лексических единиц. Например, в коде `force = mass * acceleration` 5 токенов: `force`, `=`, `mass`, `*`, `acceleration`. Код разбиения на токены уже реализован и на вход вашему алгоритму поступает список токенов. Один документ представляется типом `DocumentTokens` (который объявлен, как синоним `List<string>`).
2. Если два токена различаются, то будем учитывать ещё степень различия. Стоимость замены одного токена на другой в алгоритме Левенштейна будем вычислять с помощью формулы коэффициента Жаккара. Она тоже реализована за вас в методе `GetTokenDistance` класса `TokenDistanceCalculator`. Стоимость удаления/добавления токена равна единице, как и в оригинальном алгоритме.

Корректность работы проверяйте с помощью имеющихся в проекте модульных тестов.

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using DocumentTokens = System.Collections.Generic.List<string>;

namespace Antiplagiarism;

public class LevenshteinCalculator
{
    public List<ComparisonResult> CompareDocumentsPairwise(List<DocumentTokens> documents)
    {
        var listComparison = new List<ComparisonResult>();
    
        int i = 0;
        int j = i + 1;
        while (i < documents.Count - 1)
        {
            j = i + 1;
            while (j < documents.Count)
            {
                listComparison.Add(GetLevenshteinDistance(documents[i], documents[j]));
                j++;
            }
            i++;
        }
    
        return listComparison;
    }
    
    private ComparisonResult GetLevenshteinDistance(DocumentTokens firstDocument, DocumentTokens secondDocument)
    {
        var opt = new double[firstDocument.Count + 1, secondDocument.Count + 1];
        for (var k = 0; k <= firstDocument.Count; ++k) { opt[k, 0] = k; }
        for (var m = 0; m <= secondDocument.Count; ++m) { opt[0, m] = m; }
    
        for (var k = 1; k <= firstDocument.Count; ++k)
            for (var m = 1; m <= secondDocument.Count; ++m)
            {
                if (firstDocument[k - 1] == secondDocument[m - 1])
                    opt[k, m] = opt[k - 1, m - 1];
                else
                {
                    double delete = opt[k - 1, m] + 1;
                    double additional = opt[k, m - 1] + 1;
                    double replace = opt[k - 1, m - 1] +
                        TokenDistanceCalculator.GetTokenDistance(firstDocument[k-1], secondDocument[m-1]);
    
                    opt[k, m] = Math.Min(Math.Min(delete, additional), replace);
                }
            }
    
        return new ComparisonResult(firstDocument, secondDocument, opt[firstDocument.Count, secondDocument.Count]);
    }
}
```
