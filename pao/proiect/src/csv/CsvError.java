package csv;

import java.io.IOException;

public class CsvError extends IOException {
    CsvError(String message) {
        super(message);
    }

    static CsvError wrongNumberOfColumns() {
        return new CsvError("invalid number of columns");
    }
}
