# Практика «CVS»

[Скачайте проект Clones](Clones.zip)

Клонеры с планеты Камину выращивают клонов, причём отменных. Таких хороших результатов они достигают за счёт того, что тщательно следят за эволюцией своих творений. Сейчас каминуане заняты тем, что разрабатывают новую технологию обучения, позволяющую повысить эффективность клонов. Чтобы было удобней следить за ходом экспериментов, клонеры разработали специальную систему контроля «Clone Version System». Эта система довольно проста в эксплуатации.

В распоряжении каминуан есть некоторый набор программ обучения. Эффективность клона зависит от того, какие программы и в каком порядке он усвоил. Каминуане могут обучить любого клона по одной из программ, если он ещё не усвоил её ранее. После обучения клон приобретает нужные знания, и программа считается усвоенной.

Для удобства проведения экспериментов каминуане предоставили себе возможность откатывать действие последней усвоенной клоном программы. Знания клона в случае отката возвращаются к уровню, когда программа ещё не была усвоена. Тогда этого клона в дальнейшем можно опять обучать по такой программе. Откаты можно совершать до тех пор, пока клон не вернётся к базовым знаниям.

Кроме отката также предусмотрена возможность переусвоения. В случае, если каминуанин по ошибке применил откат, он может его отменить. Система контроля хранит историю откатов каждого клона. При применении очередного отката в историю делается соответствующая запись. При переусвоении — запись стирается. В случае обучения (не переусвоения) вся история откатов данного клона стирается. Переусвоение можно применять до тех пор, пока в истории по клону существуют записи.

И наконец в системе есть возможность клонирования. В случае, если каминуанам нравится текущий вариант клона, они могут его расклонировать. То есть создать нового клона с той же последовательностью усвоенных программ и историей откатов.

Изначально у каминуан есть один клон, имеющий базовые знания. Помогите им с анализом хода экспериментов.

**Оформление решения**

В классе CloneVersionSystem реализуйте метод Execute, принимающий на вход описание команды в виде строки и возвращающий результат в виде строки.

Поддерживаемые команды:
- learn ci pi. Обучить клона с номером ci по программе pi.
- rollback ci. Откатить последнюю программу у клона с номером ci.
- relearn ci. Переусвоить последний откат у клона с номером ci.
- clone ci. Клонировать клона с номером ci.
- check ci. Вернуть программу, которой клон с номером ci владеет и при этом усвоил последней. Если клон владеет только базовыми знаниями, верните "basic".

Выполнение команды check должно возвращать имя программы. Выполнение остальных команд должно возвращать null.

Все команды корректны, в частности, к клону, уже владеющему некоторой программой, learn по ней же применяться не будет. К клону, не владеющему ни одной программой, не применяется rollback. А также relearn возможен только при непустой истории откатов. В запросах может фигурировать только уже существующий клон. Номера клонам присваиваются в порядке их возникновения. Клон, с которого каминуане начали свои эксперименты, имел номер один.

Источник задачи — [acm.timus.ru](https://acm.timus.ru/problem.aspx?num=1992)


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections.Generic;

namespace Clones
{
    public class CloneVersionSystem : ICloneVersionSystem
    {
        List<Clone> ListClones = new List<Clone>();
        public CloneVersionSystem() { ListClones.Add(new Clone(1)); }
    
        public string Execute(string query)
        {
            var stringSwitch = query.Split(' ');
            switch (stringSwitch[0])
            {
                case ("learn"):
                    ListClones[Int32.Parse(stringSwitch[1]) - 1]
                    .Learn(Int32.Parse(stringSwitch[2]));
                    break;
                case ("rollback"):
                    ListClones[Int32.Parse(stringSwitch[1]) - 1].Rollback();
                    break;
                case ("relearn"):
                    ListClones[Int32.Parse(stringSwitch[1]) - 1].Relearn();
                    break;
                case ("clone"):
                    Clone temp = ListClones[Int32.Parse(stringSwitch[1]) - 1];
                    ListClones.Add(temp.CloneCopy(temp.HistoryAdd.Head, temp.HistoryAdd.Tail,
                                                  temp.HistoryDel.Head, temp.HistoryDel.Tail));
                    break;
                case ("check"): return ListClones[Int32.Parse(stringSwitch[1]) - 1].Check();
            }
            return null;
        }
    }
    
    public class Clone
    {
        readonly int Number;
        public Stack HistoryAdd;
        public Stack HistoryDel;
    
        public Clone(int ci)
        { Number = ci; HistoryAdd = new Stack(); HistoryDel = new Stack(); }
    
        public void Learn(int pi) { HistoryAdd.Push(pi); }
    
        public void Rollback() { HistoryDel.Push(HistoryAdd.Pop()); }
    
        public void Relearn() { HistoryAdd.Push(HistoryDel.Pop()); }
    
        public string Check()
        { return HistoryAdd.Tail != null ? HistoryAdd.Tail.Value.ToString() : "basic"; }
    
        public Clone CloneCopy(StackItem headHistoryAdd, StackItem tailHistoryAdd,
                               StackItem headHistoryDel, StackItem tailHistoryDel)
        { return new Clone(headHistoryAdd, tailHistoryAdd, headHistoryDel, tailHistoryDel); }
    
        public Clone(StackItem headHistoryAdd, StackItem tailHistoryAdd,
                     StackItem headHistoryDel, StackItem tailHistoryDel)
        {
            HistoryAdd = new Stack { Head = headHistoryAdd, Tail = tailHistoryAdd };
            HistoryDel = new Stack { Head = headHistoryDel, Tail = tailHistoryDel };
        }
    }
    
    public class StackItem
    {
        public int Value { get; set; }
        public StackItem Previous { get; set; }
    }
    
    public class Stack
    {
        public StackItem Head;
        public StackItem Tail;
    
        public void Push(int value)
        {
            if (Head == null) Tail = Head = new StackItem { Value = value, Previous = null };
            else
            {
                var item = new StackItem { Value = value, Previous = null };
                item.Previous = Tail;
                Tail = item;
            }
        }
    
        public int Pop()
        {
            if (Tail == null) throw new InvalidOperationException();
            var result = Tail.Value;
            Tail = Tail.Previous;
            if (Tail == null)
                Head = null;
            return result;
        }
    }
}
```
