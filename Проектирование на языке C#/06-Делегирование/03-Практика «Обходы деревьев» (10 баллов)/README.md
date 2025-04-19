# Практика «Обходы деревьев»

Часто делегаты можно использовать для тонкой настройки алгоритмов, что позволит использовать один и тот же код для решения несколько разных задач.

Скачайте [проект Delegates.TreeTraversal](Delegates.TreeTraversal.zip)

Перед вами три задачи:
1. Дано дерево категорий продуктов, в каждой категории могут быть другие категории и собственно продукты. Вам нужно вывести список продуктов.
2. Дано дерево задач, каждая задача может содержать подзадачи. Вам нужно вывести список таких задач, у которых нет подзадач.
3. Дано бинарное дерево, у которого каждый узел содержит значение. Вам нужно вывести все значения в листьях этого дерева.

Вам нужно написать *один* алгоритм обхода дерева, который бы принимал в качестве аргументов делегаты, объясняющие алгоритму, как обходить дерево и какие величины выводить.

**Понятность кода**

Имейте в виду, что слишком сложные делегаты в сигнатуре метода могут затруднять чтение кода. Постарайтесь решить задачу так, чтобы читатель не запутался в делегатах. В идеале вызов вашего метода обхода дерева должен быть понятен без чтения его кода, только по имени метода и именам и типам его аргументов.

После этого вам нужно написать реализации трех публичных методов в классе `Traversal` так, чтобы все тесты заработали. Если вы все сделали правильно, то эти методы должны оказаться простыми и короткими.

Все тесты пройдены, задача сдана:
```cs
namespace Delegates.TreeTraversal
{
    public static class Traversal
    {
        private static IEnumerable<TOut> MakeTraversal<TIn, TOut>(
            Func<TIn, IEnumerable<TOut>> getValues,
            Func<TIn, IEnumerable<TIn>> getNextElement,
            TIn root
            )
        {
            var queue = new Queue<TIn>();
            var result = new List<TOut>();
            queue.Enqueue(root);
    
            while (queue.Count != 0)
            {
                var current = queue.Dequeue();
                result.AddRange(getValues(current));
    
                foreach (var item in getNextElement(current))
                    queue.Enqueue(item);
            }
    
            return result;
        }
    
        public static IEnumerable<Product> GetProducts(ProductCategory root)
        {
            return MakeTraversal(
                x => x.Products,
                x => x.Categories,
                root
                );
        }
    
        public static IEnumerable<Job> GetEndJobs(Job root)
        {
            return MakeTraversal(
                x => x.Subjobs.Count == 0 ? new[] { x } : Array.Empty<Job>(),
                x => x.Subjobs,
                root
                );
        }
    
        public static IEnumerable<T> GetBinaryTreeValues<T>(BinaryTree<T> root)
        {
            return MakeTraversal(
                x => (x.Left == null && x.Right == null) ? new T[] { x.Value } : Array.Empty<T>(),
                x => new BinaryTree<T>[] { x.Left, x.Right }.Where(y => y != null), 
                root
                );
        }
    }
}
```
