package csv;

import java.io.IOException;

/**
 * Custom exception class for CSV serialization errors.
 */
public class CsvError extends IOException {
    CsvError(String message) {
        super(message);
    }

    static CsvError wrongNumberOfColumns() {
        return new CsvError("invalid number of columns");
    }
}
