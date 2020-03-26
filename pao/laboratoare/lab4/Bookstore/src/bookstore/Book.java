package bookstore;

public class Book {
    private String title, author, publisher;
    private int pageCount;

    public Book(String title, String author, String publisher, int pageCount) {
        if (pageCount <= 0) {
            throw new IllegalArgumentException("pageCount must be positive");
        }

        this.title = title;
        this.author = author;
        this.publisher = publisher;
        this.pageCount = pageCount;
    }

    public int getPageCount() {
        return pageCount;
    }

    @Override
    public String toString() {
        return "BOOK_TITLE: '" + title.toUpperCase() + "'\n" +
               "PUBLISHER: '" + publisher.toLowerCase() + "'\n" +
               "AUTHOR: '" + author + "'";
    }
}
