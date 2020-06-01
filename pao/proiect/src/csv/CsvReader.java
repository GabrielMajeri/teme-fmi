package csv;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class CsvReader<T> {
    private final BufferedReader reader;
    private final CsvTypeFactory<T> factory;
    private final int numColumns;

    public CsvReader(BufferedReader reader, CsvTypeFactory<T> factory) throws IOException {
        this.reader = reader;
        this.factory = factory;

        String[] headerColumns = readLine();
        String[] columnNames = factory.getColumnNames();
        if (headerColumns.length != columnNames.length) {
            throw CsvError.wrongNumberOfColumns();
        }
        numColumns = headerColumns.length;
        boolean headersMatch = Arrays.equals(headerColumns, columnNames);
        if (!headersMatch) {
            throw new CsvError("header does not have same columns");
        }
    }

    public boolean hasMoreObjects() throws IOException {
        return reader.ready();
    }

    public T readObject() throws IOException {
        String[] values = readLine();
        if (values.length != numColumns) {
            throw CsvError.wrongNumberOfColumns();
        }
        return factory.fromStringArray(values);
    }

    public Collection<T> readAll() throws IOException {
        Collection<T> objects = new ArrayList<>();
        while (hasMoreObjects()) {
            objects.add(readObject());
        }
        return objects;
    }

    private String[] readLine() throws IOException {
        String line = reader.readLine();
        return line.split(",");
    }
}
