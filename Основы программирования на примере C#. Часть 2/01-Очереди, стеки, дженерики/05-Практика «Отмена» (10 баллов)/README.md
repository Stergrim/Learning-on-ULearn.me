# Практика «Отмена»

Продолжайте работу в [том же проекте LimitedSizeStack](LimitedSizeStack.zip).

Если вы запустите проект на исполнение, то увидите окно приложения, в котором можно добавлять новые дела и удалять уже существующие. Однако кнопка "Отмена" пока не работает. Ваша задача — сделать так, чтобы эта кнопка отменяла последнее действие пользователя.

Изучите класс `ListModel` — в нём реализована логика работы кнопок в приложении.

Реализуйте методы `Undo` и `CanUndo`. Для этого нужно хранить историю последних `undoLimit` действий удаления/добавления. Используйте для этого класс `LimitedSizeStack` из прошлой задачи. Его не нужно включать в отправляемый на проверку файл, считайте, что этот класс уже есть в проекте.
- Метод `Undo` отменяет последнее действие из истории.
- Метод `CanUndo` возвращает `true`, если на данный момент история действий не пуста, то есть если вызов `Undo` будет корректным. Иначе метод должен вернуть `false`.

Проверить корректность своего решения можно на модульных тестах из класса `ListModel_Should` и `ListModel_PerformanceTest`.

Если хотите, можете воспользоваться классическим объектно-ориентированным шаблоном [Команда](https://refactoring.guru/ru/design-patterns/command). Однако для сдачи данной задачи, точно следовать этому шаблону необязательно.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;

namespace LimitedSizeStack
{
    public class ListModel<TItem>
    {
        LimitedSizeStack<Tuple<string, TItem, int>> Stack;
        public List<TItem> Items { get; }
        public int Limit;
    
        public ListModel(int limit)
        {
            Items = new List<TItem>();
            Limit = limit;
            Stack = new LimitedSizeStack<Tuple<string, TItem, int>>(limit);
        }
    
        public void AddItem(TItem item)
        { Stack.Push(Tuple.Create("Добавить", item, Items.Count)); Items.Add(item); }
    
        public void RemoveItem(int index)
        { Stack.Push(Tuple.Create("Удалить", Items[index], index)); Items.RemoveAt(index); }
    
        public bool CanUndo() { return !(Stack.Count == 0); }
    
        public void Undo()
        {
            var operration = Stack.Pop();
            if (operration.Item1 == "Добавить") { Items.RemoveAt(operration.Item3); }
            else if (operration.Item1 == "Удалить") 
                { Items.Insert(operration.Item3, operration.Item2); }
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[LimitedSizeStack Edit](LimitedSizeStack_Edit.zip)
