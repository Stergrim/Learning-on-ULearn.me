# Упражнение. ImageResizer

В классе `ImageResizer` создать статический метод `resize`, принимающий абсолютный путь к папке с изображениями и новую ширину для них.
- Если по переданному пути папки не существует, необходимо бросить `IllegalArgumentException`.
- Используя `Scalr.resize(image, width, height)` (*org.imgscalr.Scalr*), измените ширину каждого изображения в этой папке на новую, при этом сохранив пропорции изображения.

**Все тесты пройдены, задача сдана:**
```java
public class ImageResizer {
    public static void resize(String path, int width) {
        File fileImage = new File(path);
        File[] files = fileImage.listFiles(filter);
    
        if (!fileImage.exists() || files.length == 0) throw new IllegalArgumentException();
    
        for (File file : files) {
            try {
                BufferedImage image = ImageIO.read(file);
                image = Scalr.resize(image,Scalr.Mode.FIT_TO_WIDTH,width);
                if (file.getName().endsWith("jpg")) ImageIO.write(image,"jpg",file);
                else if (file.getName().endsWith("png")) ImageIO.write(image,"png",file);
            } catch (IOException e) { e.printStackTrace(); }
        }
    }
    
    private static FileFilter filter = new FileFilter() {
        public boolean accept(File f) {
            return f.getName().endsWith("png") || f.getName().endsWith("jpg");
        }
    };
}
```
