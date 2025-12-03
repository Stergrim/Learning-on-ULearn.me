# Упражнение. Buffer

Вам нужно реализовать метод `RefactorFile` в классе `BufferTask`.
- Метод должен найти файл с именем "File.txt", с помощью `ReadableByteChannel` и `WritableByteChannel` прочитать данные из него, модифицировать полученные данные и записать их в новый файл с именем "newFile.txt".
- В качестве данных в файле находится текст перемешанный с цифрами, вам нужно просто избавиться от них (*должны остаться только буквы и пробелы*).


**Все тесты пройдены, задача сдана:**
```java
public class BufferTask {
    public static void refactorFile() {
        try (FileInputStream fis = new FileInputStream("File.txt");
             ReadableByteChannel inputChannel = Channels.newChannel(fis);
             FileOutputStream fos = new FileOutputStream("newFile.txt");
             WritableByteChannel outputChannel = Channels.newChannel(fos)) {
            
            ByteBuffer buffer = ByteBuffer.allocate(8192);
            StringBuilder contentBuilder = new StringBuilder();
            
            while (inputChannel.read(buffer) != -1) {
                buffer.flip();
                byte[] data = new byte[buffer.remaining()];
                buffer.get(data);
                contentBuilder.append(new String(data));
                buffer.clear();
            }
            
            String originalContent = contentBuilder.toString();
            String filteredContent = originalContent.replaceAll("[^a-zA-Z\\s]", "");
            
            ByteBuffer outputBuffer = ByteBuffer.wrap(filteredContent.getBytes());
            outputChannel.write(outputBuffer);
            
        } catch (FileNotFoundException e) {
            System.err.println("Файл File.txt не найден: " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Ошибка ввода-вывода: " + e.getMessage());
        }
    }
}
```
