package jobs.utils;

import java.util.Collection;
import java.util.function.Supplier;

public class AssertUtil {
    private AssertUtil() {
        throw new AssertionError();
    }

    public static void assertThat(boolean condition, String message) {
        assertThat(condition, () -> message);
    }

    public static void assertThat(boolean condition, Supplier<String> messageProducer) {
        if (!condition) {
            throw new AssertionError(messageProducer.get());
        }
    }

    public static <T> void assertEmpty(Collection<T> collection) {
        assertThat(collection.isEmpty(), () -> "Collection is not empty: " + collection);
    }

    public static <T> void assertContains(Collection<T> collection, T object) {
        assertThat(collection.contains(object), () -> object + " not found in " + collection);
    }

    public static <T> void assertNotContains(Collection<T> collection, T object) {
        assertThat(!collection.contains(object), () -> object + " present in " + collection);
    }
}
