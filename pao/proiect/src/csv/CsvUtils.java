package csv;

import java.util.Objects;

public class CsvUtils {
    private CsvUtils() {
        throw new AssertionError();
    }

    public static String requireNoComma(String s) {
        s = Objects.requireNonNull(s);
        if (s.contains(",")) {
            throw new AssertionError();
        }
        return s;
    }
}
