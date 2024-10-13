# Практика «Циклы Brainfuck»

Продолжайте работу в том же проекте [brainfuck](brainfuck.zip).

В классе BrainfuckLoopCommands реализуйте метод, регистрирующий следующие команды в виртуальную машину:

| **Символ** | **Значение**                                                                                                                                                                              |
|------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [  | (Начало цикла) Перескочить по программе вправо на соответствующий (с учетом вложенности) символ `]`, если текущий байт памяти равен нулю. Продолжать исполнение с этого символа.          |
| ]  | (Конец цикла) Перескочить по списку инструкций влево на соответствующий (с учетом вложенности) символ `[`, если текущий байт памяти НЕ равен нулю. Продолжать исполнение с этого символа. |


Например, программа `++++++++[>++++++++<-]>+.` выводит букву `A` (ASCII-код 65 получается увеличением 8 раз второй ячейки на 8, а потом добавлением ещё единицы).

Детали реализации инструкций восстановите по тестам в классе BrainfuckLoopCommandsTests. Сделайте так, чтобы все тесты в этом файле проходили.



Все тесты пройдены, задача сдана:
```cs
using System.Collections.Generic;

namespace func.brainfuck
{
    public class BrainfuckLoopCommands
    {
        public static Dictionary<int, int> StartAndEnd { get; set; }
        public static Stack<int> StackLoop { get; set; }
    
        public static void RegisterTo(IVirtualMachine vm)
        {
            StartAndEnd = new Dictionary<int, int> { };
            StackLoop = new Stack<int> { };
            for(int i= 0; i < vm.Instructions.Length; i++)
                switch(vm.Instructions[i])
                {
                    case '[': { StackLoop.Push(i); break; }
                    case ']':
                        var start = StackLoop.Pop();
                        StartAndEnd.Add(i, start);
                        StartAndEnd.Add(start, i);
                        break;
                }
            vm.RegisterCommand('[', b => {
                if (vm.Memory[vm.MemoryPointer] > 0) StackLoop.Push(vm.InstructionPointer);
                else vm.InstructionPointer = StartAndEnd[vm.InstructionPointer];
            });
            vm.RegisterCommand(']', b => {
                if (vm.Memory[vm.MemoryPointer] > 0) vm.InstructionPointer =
                                                     StartAndEnd[vm.InstructionPointer];
                else StackLoop.Pop();
            });
        }
    }
}
```

**Проект со всеми внесенными решениями.**
[brainfuck Edit](brainfuck_Edit.zip)
