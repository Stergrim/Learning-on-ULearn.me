# Практика «Readonly bytes»

Скачайте проект [readonly-bytes](readonly-bytes.zip).

Иногда есть смысл в качестве ключей в Dictionary или HashSet использовать массивы байт. Однако по умолчанию массивы сравниваются по ссылкам, а не по содержимому, а часто нужно именно по содержимому. В таких случаях можно написать класс-обёртку над массивом, который переопределит Equals и HashCode так, чтобы сравнение происходило по содержимому. В этой задаче вам нужно создать именно такую обёртку.

В файле ReadonlyBytes.cs создайте класс ReadonlyBytes так, чтобы все тесты из файла ReadonlyBytesTests.cs компилировались и проходили.


Все тесты пройдены, задача сдана:
```cs
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace hashes
{
    public class ReadonlyBytes : IEnumerable
    {
        readonly byte[] Massiv;
        public int Length { get; }
        readonly int offsetBasis = unchecked((int)2166136261);
        readonly int Prime = 16777619;
        readonly int Hash;
    
        public ReadonlyBytes(params byte[] massiv)
        {
            if (massiv == null) throw new ArgumentNullException();
            this.Massiv = massiv;
            this.Length = massiv.Length;
            int hash = unchecked((int)offsetBasis);
            foreach (var e in Massiv)
                hash = unchecked((hash ^ e) * Prime);
            Hash = unchecked((int)hash);
        }
    
        public byte this[int index]
        {
            get
            {
                if (index < 0 || index >= Length) throw new IndexOutOfRangeException();
                return Massiv[index];
            }
        }
    
        public override bool Equals(object obj)
        {
            if (obj == null) return false;
            if (obj.GetType() != this.GetType()) return false;
            var temp = obj as ReadonlyBytes;
            if (temp.Length != this.Length) return false;
            for (int i = 0; i < Length; i++)
                if (temp[i] != this[i]) return false;
            return true;
        }
    
        public override int GetHashCode() { return Hash; }
    
        public override string ToString()
        {
            var str = new StringBuilder("[");
            for(int i = 0; i < Length; i++)
            {
                str.Append(Massiv[i].ToString());
                if(i < Length - 1) str.Append(", ");
            }
            str.Append("]");
            return str.ToString();
        }
    
        public IEnumerator<byte> GetEnumerator()
        {
            for (int i = 0; i < Length; i++)
                yield return this[i];
        }
    
        IEnumerator IEnumerable.GetEnumerator() { return GetEnumerator(); }
    }
}
```
