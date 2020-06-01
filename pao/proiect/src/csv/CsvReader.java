package csv;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Arrays;

public class CsvReader<T> {
    private final BufferedReader reader;
    private final CsvTypeFactory<T> factory;

    public CsvReader(BufferedReader reader, CsvTypeFactory<T> factory) throws IOException {
        this.reader = reader;
        this.factory = factory;

        String header = reader.readLine();
        String[] columns = header.split(",");
        boolean headersMatch = Arrays.equals(columns, factory.getColumnNames());
        if (!headersMatch) {
            throw new RuntimeException("headers of CSV file do not match");
        }
    }

    public boolean hasMoreObjects() throws IOException {
        return reader.ready();
    }

    public T readObject() throws IOException {
        String line = reader.readLine();
        String[] values = line.split(",");
        return factory.fromStringArray(values);
    }
}
