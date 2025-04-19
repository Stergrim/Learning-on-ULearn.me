# Практика «Сжатие»

В этом задании вам нужно написать класс `CustomCompressionStream`, осуществляющий сжатие информации при записи и её восстановление при чтении.

В качестве алгоритма сжатия можете выбрать любой. Бороться за высокое качество сжатия не обязательно: решение тестируется на байтовых последовательностях, в которых часто будут встречаться последовательные повторения одного и того же байта. Поэтому отлично подойдёт, например, простейшая реализация RLE.

Как и в предыдущем задании, при распаковке не следует читать сразу весь исходный поток. Однако, ограничений на размеры чтения нет.

Вам нужно реализовать только методы `int Read(byte[] buffer, int offset, int count)` и `void Write(byte[] buffer, int offset, int count)` и, если понадобится для вашего алгоритма, то ещё и метод `void Flush()`.

Работайте в [проекте Streams.Compression](Streams.Compression.zip)

Рекомендуем открыть тесты в файле CustomCompressionStream_should и последовательно заставлять тесты работать. Как обычно, тесты служат документацией и полнее раскрывают дополнительные требования к вашему решению.

Все тесты пройдены, задача сдана:
```cs

namespace Streams.Compression
{
    public class CustomCompressionStream : Stream
    {
        private readonly bool read;
        private readonly Stream baseStream;
        private int counter;
        private int valueByteStock = -1;
        private int repeatByteStock = -1;
    
        public CustomCompressionStream(Stream baseStream, bool read)
        {
            this.read = read;
            this.baseStream = baseStream;
        }
    
        public override int Read(byte[] buffer, int offset, int count)
        {
            int subCounter;
            for (subCounter = 0; subCounter < repeatByteStock; subCounter++)
                buffer[subCounter] = (byte)valueByteStock;
            
            valueByteStock = -1;
            repeatByteStock = -1;
    
            return Decompress(buffer, offset, count, subCounter);
        }
    
        private int Decompress(byte[] buffer, int offset, int count, int subCounter)
        {
            for (counter = subCounter; counter < count + offset;)
            {
                var valueByte = baseStream.ReadByte();
                var repeatByte = baseStream.ReadByte();
    
                if (repeatByte == -1)
                    return valueByte == -1 ? counter : throw new InvalidOperationException();
    
                for (int i = 0; i < repeatByte; i++)
                {
                    if (counter + offset == buffer.Length)
                    {
                        valueByteStock = valueByte;
                        repeatByteStock = repeatByte - i;
                        if (offset > 0) buffer[buffer.Length - 1] = 0;
                        return counter - offset;
                    }
                    buffer[counter++ + offset] = (byte)valueByte;
                }
            }
    
            return counter;
        }
    
        public override void Write(byte[] buffer, int offset, int count)
        {
            for (var i = offset; i < count; i++)
            {
                byte countRepeatByte = 1;
                while (i < buffer.Length - 1 && buffer[i] == buffer[i + 1] && countRepeatByte < byte.MaxValue)
                {
                    countRepeatByte++;
                    i++;
                }
                baseStream.WriteByte(buffer[i]);
                baseStream.WriteByte(i == buffer.Length - 1 ? (byte)(countRepeatByte - offset) : countRepeatByte);
            }
        }
    
        public override bool CanRead => read;
        public override bool CanSeek => false;
        public override bool CanWrite => !read;
        public override long Length => throw new NotSupportedException();
        
        public override long Position
        {
            get => throw new NotSupportedException();
            set => throw new NotSupportedException();
        }
    
        public override void Flush() => baseStream.Flush();
        public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();
        public override void SetLength(long value) => throw new NotSupportedException();
    }
}
```
