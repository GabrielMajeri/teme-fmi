import java.util.HashSet;
import java.util.Set;

public class StringSet {
    private final Set<String> strings;

    public StringSet() {
        this(20);
    }

    public StringSet(int initialCapacity) {
        strings = new HashSet<>(initialCapacity);
    }

    public void add(String string) {
        if (strings.contains(string)) {
            throw new RuntimeException("Cannot add duplicate string");
        }

        strings.add(string);
    }

    public Iterable<String> get() {
        return strings;
    }

    public static void main(String[] args) {
        StringSet stringSet = new StringSet();

        stringSet.add("mere");
        stringSet.add("pere");
        stringSet.add("banana");

        for (String string : stringSet.get()) {
            System.out.println(string);
        }

        stringSet.add("mere");
    }
}
