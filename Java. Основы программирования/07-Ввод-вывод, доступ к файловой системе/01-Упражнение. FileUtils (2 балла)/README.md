# Упражнение. FileUtils

В классе `FileUtils` реализуйте метод `calculateFolderSize`, принимающий абсолютный путь к папке и возвращающий ее размер.

Если путь неверного формата или его не существует, то метод должен бросать `IllegalArgumentException`. Метод должен обойти не только файлы, но и папки с файлами.

**Все тесты пройдены, задача сдана:**
```java
public class FileUtils {
    public static long calculateFolderSize(String path) {
        File mypath = new File(path);
        long length = 0;
        if (!mypath.isDirectory() || !mypath.exists()) throw new IllegalArgumentException();
        for (File file : mypath.listFiles()) {
            length += file.length();
        }
        return length;
    }
}
```
