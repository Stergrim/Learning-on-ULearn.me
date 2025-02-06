# GetMinValue

Говорят, что в бинарном дереве легко найти минимальный элемент.

Вы на спор решили написать реализацию этой функции в одну строку! Выкручивайтесь теперь...

```cs
public static void Main()
{
    var tree = new TreeNode { Value = 10 };
    Assert.AreEqual(10, GetMinValue(tree));
    
    tree.Left = new TreeNode { Value = 5 };
    tree.Right = new TreeNode { Value = 15 };
    Assert.AreEqual(5, GetMinValue(tree));
    
    tree.Left.Left = new TreeNode { Value = 2 };
    tree.Left.Left.Right = new TreeNode { Value = 3 };
    tree.Left.Right = new TreeNode { Value = 6 };
    Assert.AreEqual(2, GetMinValue(tree));
}

public class TreeNode
{
    public int Value;
    public TreeNode Left, Right;
}
```


Все тесты пройдены, задача сдана:
```cs
public static int GetMinValue(TreeNode root)
{
    return root.Left != null ? GetMinValue(root.Left) : root.Value;
}
```
