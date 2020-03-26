package bookstore;

public class BookStore {
    private int capacity;
    private Book[] books;
    private int index;

    public BookStore(int capacity) {
        this.capacity = capacity;
        this.books = new Book[capacity];
        this.index = 0;
    }

    public void add(Book book) {
        if (index == capacity) {
            throw new RuntimeException("Book store full");
        }
        books[index++] = book;
    }

    public int getLength() {
        return index;
    }

    public Book get(int i) {
        if (i > index) {
            throw new ArrayIndexOutOfBoundsException("Book not found");
        }
        return books[i];
    }

    public int find(Book book) {
        for (int i = 0; i < index; ++i) {
            if (books[i].equals(book)) {
                return i;
            }
        }
        return -1;
    }

    public void remove(Book book) {
        int bookIndex = find(book);
        for (int i = bookIndex; i < index - 1; ++i) {
            books[i] = books[i + 1];
        }
        --index;
    }
}
