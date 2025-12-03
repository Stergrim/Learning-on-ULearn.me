# Практика. Utils

В классе `Utils` реализовать следующие методы:
1. `calculateFolderSize` – как в предыдущей задаче.
2. `copyFolder` – принимает абсолютный путь к папке, которую нужно скопировать и куда нужно скопировать. Если копируемой папки не существует, метод бросает `IllegalArgumentException`.

Создайте класс `MyFileVisitor`, наследующий `SimpleFileVisitor`, и переопределите в нем метод `visitFile` так, чтобы при обходе сохранялись пути к каждому файлу и папке. С помощью `Files.walkFileTree` обойдите оригинальную папку, после чего скопируйте из неё файлы в новую папку. В оригинальной папке могут находиться не только файлы, но и другие папки с другими файлами, вам нужно сохранить эту иерархию при копировании.

**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Utils {
    public static long calculateFolderSize(String path) {
        File mypath = new File(path);
        long length = 0;
        if (!mypath.isDirectory() || !mypath.exists()) throw new IllegalArgumentException();
        for (File file : mypath.listFiles()) {
            length += file.length();
        }
        return length;
    }
    
    public static void copyFolder(String sourceDirectory, String destinationDirectory) {
        File directory = new File(sourceDirectory);
        Path mainPath = Paths.get(sourceDirectory);
        Path copyDir = Paths.get(destinationDirectory);
    
        if (!directory.exists()) throw new IllegalArgumentException();
        else {
            try {
                if (!Files.exists(copyDir)) Files.createDirectory(copyDir);
                MyFileVisitor myFileVisitor = new MyFileVisitor();
                Files.walkFileTree(mainPath,myFileVisitor);
                for (Path path : myFileVisitor.getPaths()) {
                    Path test = new File(destinationDirectory+File.separator+path).toPath();
                    Files.copy(path,new File(copyDir+File.separator + 
                            path.getFileName()).toPath(),StandardCopyOption.REPLACE_EXISTING);
                }
            } catch (IOException e) { e.printStackTrace(); }
        }
    }
}

public class MyFileVisitor extends SimpleFileVisitor<Path> {
    private ArrayList<Path> paths = new ArrayList<>();
    
    public ArrayList<Path> getPaths() { return paths; }
    
    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        paths.add(file);
        return FileVisitResult.CONTINUE;
    }
}
```
