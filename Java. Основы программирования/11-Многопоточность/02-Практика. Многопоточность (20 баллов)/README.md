# Практика. Многопоточность

Создайте новый проект и напишите приложение, которое в многопоточном режиме сформирует карту заданного сайта (список ссылок), и запишите её в файл.

Ссылки на дочерние страницы должны располагаться в файле с отступами на одну табуляцию относительно родительских.

В файле должны быть ссылки на страницы, размещённые на том же домене (в примере — urfu.ru). В списке не должно быть:
- ссылок на другие сайты и поддомены,
- ссылок на внутренние элементы страниц (у таких ссылок есть символ # после адреса страницы).
- При запросе страниц нужно выдерживать паузы (с помощью метода `sleep()` у потока), чтобы сайт не заблокировал доступ вашего приложения. Используйте значения от 100 до 150 мс.
- Для отладки программы выберите сайт с сотнями или тысячами страниц (например, http://www.lenta.ru/), чтобы сервер вас не заблокировал.
- Учитывайте что сайт имеет структуру графа, то есть страницы могут содержать ссылки на главную, на страницы с которой вы пришли по ссылке. Исключите возможность циклического перебора ссылок.


**Все тесты пройдены, решение ожидает код-ревью:**
```java
public class Main {
    private static String ROOT_SITE = "https://urfu.ru/";
    
    public static void main(String[] args) throws IOException {
        WebsiteNode websiteNode = new WebsiteNode(ROOT_SITE);
        new ForkJoinPool().invoke(new WebsiteNodeRecursiveAction(websiteNode));
    
        java.io.FileOutputStream stream = new java.io.FileOutputStream("sitemap.txt");
        String result = createSitemapString(websiteNode, 0);
        stream.write(result.getBytes());
        stream.flush();
        stream.close();
    }
    
    public static String createSitemapString(WebsiteNode node, int depth) {
        String tabs = String.join("", Collections.nCopies(depth, "\t"));
        StringBuilder result = new StringBuilder(tabs + node.getUrl());
        node.getChildren().forEach(child -> {
            result.append("\n").append(createSitemapString(child, depth + 1));
        });
        return result.toString();
    }
}


public class WebsiteNode {
    private volatile WebsiteNode parent;
    private volatile int depth;
    private String url;
    private volatile java.util.concurrent.CopyOnWriteArrayList<WebsiteNode> children;
    
    public WebsiteNode(String url) {
        depth = 0;
        this.url = url;
        parent = null;
        children = new java.util.concurrent.CopyOnWriteArrayList<>();
    }
    
    private int calculateDepth() {
        int result = 0;
        if (parent == null) {
            return result;
        }
        result = 1 + parent.calculateDepth();
        return result;
    }
    
    public synchronized void addChild(WebsiteNode element) {
        WebsiteNode root = getRootElement();
        if(!root.contains(element.getUrl())) {
            element.setParent(this);
            children.add(element);
        }
    }
    
    private boolean contains(String url) {
        if (this.url.equals(url)) {
            return true;
        }
    
        for (WebsiteNode child : children) {
            if(child.contains(url))
                return true;
        }
        return false;
    }
    
    private void setParent(WebsiteNode sitemapNode) {
        synchronized (this) {
            this.parent = sitemapNode;
            this.depth = calculateDepth();
        }
    }
    
    public WebsiteNode getRootElement() {
        return parent == null ? this : parent.getRootElement();
    }
    
    public String getUrl() { return url; }
    public java.util.concurrent.CopyOnWriteArrayList<WebsiteNode> getChildren() { return children; }
}

public class WebsiteNodeRecursiveAction extends RecursiveAction {
    private WebsiteNode node;
    
    public WebsiteNodeRecursiveAction(WebsiteNode node) { this.node = node; }
    
    @Override
    protected void compute() {
        try {
            java.lang.Thread.sleep(150);
            org.jsoup.Connection connection = Jsoup.connect(node.getUrl())
                    .timeout(10000);
            Document page = connection.get();
            org.jsoup.select.Elements elements = page.select("body").select("a");
            for (org.jsoup.nodes.Element a : elements) {
                String childUrl = a.absUrl("href");
                if (isCorrectUrl(childUrl)) {
                    childUrl = stripParams(childUrl);
                    node.addChild(new WebsiteNode(childUrl));
                }
            }
        } catch (IOException | InterruptedException e) {
            System.out.println(e.toString());;
        }
    
        for (WebsiteNode child : node.getChildren()) {
            WebsiteNodeRecursiveAction task = new WebsiteNodeRecursiveAction(child);
            task.compute();
        }
    }
    
    private String stripParams(String url) { return url.replaceAll("\\?.+",""); }
    
    private boolean isCorrectUrl(String url) {
        java.util.regex.Pattern patternRoot = java.util.regex.Pattern.compile("^" + node.getUrl());
        java.util.regex.Pattern patternNotFile = java.util.regex.Pattern.compile("([^\\s]+(\\.(?i)(jpg|png|gif|bmp|pdf))$)");
        java.util.regex.Pattern patternNotAnchor = java.util.regex.Pattern.compile("#([\\w\\-]+)?$");
    
        return patternRoot.matcher(url).lookingAt()
                && !patternNotFile.matcher(url).find()
                && !patternNotAnchor.matcher(url).find();
    }
}
```
