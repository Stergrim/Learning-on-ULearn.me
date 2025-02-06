# Практика «Disk Tree»

Хакер Билл случайно потерял всю информацию с жесткого диска его компьютера, и у него нет бэкапа. Он сожалеет даже не столько о потере самих файлов, сколько о элегантной и удобной структуре каталогов, которую он создавал и развивал в течение многих лет работы.

К счастью, у Билла есть множество копий листингов директорий с его жесткого диска, с помощью которых он смог восстановить полные пути (например, "WINNT\SYSTEM32\CERTSRV\CERTCO~1\X86") для некоторых директорий.

Ваша задача - написать программу, которая поможет Биллу восстановить его драгоценную структуру каталогов, предоставляя красиво оформленное дерево каталогов.

Решение выполните в проекте [DiskTree](DiskTree.zip)

На вход ваша программа принимает список строк, соответствующих полным путям директорий. Каждый путь не содержит пробелов, не превосходит по размеру 80 символов, содержится в списке единственный раз и состоит из имени директорий, разделенных символом \.

На выход ваша программа должна возвращать список строк, соответствующих отформатированному дереву каталогов. Перед каждым именем каталога должны стоять пробелы, которые соответствуют степени вложенности каталога. Подкаталоги должны быть перечислены после имени каталога в лексикографическом порядке, и их имена должно предварять на один пробел больше, чем имя родительского каталога. Перед именами каталогов верхнего уровня не должно быть пробелов, а сами они должны быть перечислены в лексикографическом порядке. Изучите тесты для лучшего понимания требуемого формата.

Источник задачи — [acm.timus.ru](https://acm.timus.ru/problem.aspx?num=1067)

Все тесты пройдены, задача сдана:
```cs
using System;
using System.Linq;
using System.Collections.Generic;

namespace DiskTree;

public static class DiskTreeTask
{
    public static List<string> Solve(List<string> input)
    {
        var root = new DiskNode("\\");
    
        foreach (string str in input)
        {
            var currentNode = root;
            var dir = str.Split(new char[] { '\\' }).ToList();
    
            foreach (string name in dir)
                if (!currentNode.Next.Select(x => x.Directory).Contains(name))
                {
                    var newNode = new DiskNode(name);
                    currentNode.Next.Add(newNode);
                    currentNode = newNode;
                }
                else currentNode = currentNode.Next
                        .Select(x => x)
                        .Where(x => x.Directory.Equals(name))
                        .FirstOrDefault();
        }
    
        var result = new List<string>();
        PrintTree(result, root,0);
        return result;
    }
    
    public static void PrintTree(List<string> result, DiskNode curr, int offset)
    {
        curr.Next.Sort((x, y) => string.Compare(x.Directory, y.Directory, StringComparison.Ordinal));
    
        foreach (var node in curr.Next)
        {
            result.Add(new String(' ', offset) + node.Directory);
            PrintTree(result, node, offset + 1);
        }
    }
}

public class DiskNode
{
    public string Directory;
    public List<DiskNode> Next = new List<DiskNode>();
    public DiskNode(string directory) { Directory = directory; }
}
```
