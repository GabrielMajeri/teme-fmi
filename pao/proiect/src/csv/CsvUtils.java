package csv;

import java.util.Objects;

/**
 * Helper class for CSV data.
 */
public class CsvUtils {
    private CsvUtils() {
        throw new AssertionError();
    }

    /**
     * Checks that the given string contains no inner commas,
     * to avoid breaking CSV serialization.
     * Throws an error if commas are found.
     * @param s string to check
     * @return the same input string
     */
    public static String requireNoComma(String s) {
        s = Objects.requireNonNull(s);
        if (s.contains(",")) {
            throw new AssertionError();
        }
        return s;
    }
}
