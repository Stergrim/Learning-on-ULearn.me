# Реализация бинарного дерева

Поиск в бинарном дереве — ключевая операция. Все остальные операции выполняются по аналогии.

```cs
public static void Main()
{
    var tree = new TreeNode { Value = 10 };
    Assert.AreEqual(null, Search(tree, 15));
    Assert.AreEqual(null, Search(tree, 5));
    Assert.AreEqual(10, Search(tree, 10).Value);
    
    tree.Left = new TreeNode { Value = 5 };
    tree.Right = new TreeNode { Value = 15 };
    Assert.AreEqual(null, Search(tree, 6));
    Assert.AreEqual(null, Search(tree, 3));
    AssertValueIs(5, Search(tree, 5));
    
    MoreTests(tree);
    CheckNoExtraSearches();
}

public class TreeNode
{
    public int Value;
    public TreeNode Left, Right;
}
```


Все тесты пройдены, задача сдана:
```cs
public static TreeNode Search(TreeNode root, int element)
{
    if (root == null) return null;
    if (element == root.Value) return root;
    return Search(root.Value < element ? root.Right : root.Left, element);
}
```
