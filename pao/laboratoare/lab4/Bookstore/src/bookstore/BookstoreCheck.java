package bookstore;

public class BookstoreCheck {
    static boolean duplicate(BookStore store, Book book) {
        int count = 0;
        for (int i = 0; i < store.getLength(); ++i) {
            if (book == store.get(i)) {
                ++count;
                if (count == 2) {
                    return true;
                }
            }
        }
        return false;
    }

    static Book thicker(Book a, Book b) {
        if (a.getPageCount() > b.getPageCount()) {
            return a;
        } else {
            return b;
        }
    }
}
