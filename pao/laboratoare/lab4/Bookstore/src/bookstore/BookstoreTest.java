package bookstore;

import java.util.Scanner;

public class BookstoreTest {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Number of books:");
        int numBooks = scanner.nextInt();

        BookStore store = new BookStore(20);

        for (int i = 0; i < numBooks; ++i) {
            String title = scanner.next();
            String author = scanner.next();
            String publisher = scanner.next();
            int pageCount = scanner.nextInt();
            store.add(new Book(title, author, publisher, pageCount));
        }

        scanner.close();

        Book first = store.get(0);
        store.add(first);
        System.out.println("Duplicate: " + BookstoreCheck.duplicate(store, first));

        Book a = store.get(0);
        Book b = store.get(1);
        System.out.println("Thicker: " + BookstoreCheck.thicker(a, b));

        System.out.println("Books:");
        for (int i = 0; i < store.getLength(); ++i) {
            System.out.println(store.get(i));
        }
    }
}
