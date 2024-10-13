# Практика «Limited Size Stack»

В этой задаче вам нужно реализовать стек ограниченного размера. Этот стек работает как обычный стек, однако при превышении максимального размера удаляет самый глубокий элемент в стеке. Таким образом в стеке всегда будет ограниченное число элементов.

Вот пример работы такого стека с ограничением в 2 элемента:

```cs
// сначала стек пуст
stack.Push(10); // в стеке 10
stack.Push(20); // в стеке 10, 20
stack.Push(30); // в стеке 20, 30
stack.Push(40); // в стеке 30, 40
stack.Pop(); // возвращает 40, в стеке остаётся 30
stack.Pop(); // возвращает 30, стек после этого пуст
```

Операция Push должна иметь сложность O(1), то есть никак не зависеть от размера стека.

[Скачайте проект LimitedSizeStack](LimitedSizeStack.zip). Реализуйте класс `LimitedSizeStack`.

Отладьте его реализацию с помощью тестов в классе `LimitedSizeStack_should`. Проверьте эффективность операции Push с помощью теста из класса `LimitedSizeStack_PerformanceTest`.

**Эффективность метода Last()**

У каждой коллекции в C# доступен метод расширения Last(). Однако, работает он за O(1) только для коллекций, реализующих интерфейс IList (список с доступом к элементам по индексу). Для остальных коллекций он работает за O(N), перебирая её элементы до конца. Будьте осторожны.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LimitedSizeStack
{
    public class StackItem<T>
    {
        public T Value { get; set; }
        public StackItem<T> Previous { get; set; }
        public StackItem<T> Next { get; set; }
    }
   
    public class LimitedSizeStack<T>
    {
        int limit;
        int count;
        StackItem<T> head;
        StackItem<T> tail;
   
        public LimitedSizeStack(int limit)
        {
            if(limit < 0) throw new NotImplementedException();
            this.limit = limit;
        }
   
        public void Push(T value)
        {
            count++;
            if (head == null)
                tail = head = new StackItem<T> { Value = value, Previous = null, Next = null };
            else
            {
                var item = new StackItem<T> { Value = value, Previous = tail, Next = null};
                tail.Next = item;
                tail = item;
            }
            if (limit == 0) count--;
            else if (count > limit)
            {
                count--;
                head = head.Next;
                head.Previous = null;
            }
        }
   
        public T Pop()
        {
            if (tail == null) throw new InvalidOperationException();
            var result = tail.Value;
            tail = tail.Previous;
            count--;
            if (tail == null)
                head = null;
            return result;
        }
   
        public int Count { get => this.count; }
    }
}
```
