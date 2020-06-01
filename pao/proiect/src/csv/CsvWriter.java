package csv;

import java.io.BufferedWriter;
import java.io.IOException;

public class CsvWriter<T> {
    private final BufferedWriter writer;
    private final CsvTypeFactory<T> factory;
    private final int numColumns;

    /**
     * Constructs a new CSV file writer.
     *
     * @param writer destination of CSV file
     * @param factory factory describing type metadata
     * @throws IOException if there was any error with writing the object to file
     */
    public CsvWriter(BufferedWriter writer, CsvTypeFactory<T> factory) throws IOException {
        this.writer = writer;
        this.factory = factory;

        String[] columnNames = factory.getColumnNames();
        this.numColumns = columnNames.length;
        writeLine(columnNames);
    }

    public void writeObject(T object) throws IOException {
        String[] strings = factory.toStringArray(object);
        assert strings.length == numColumns : "Invalid number of columns";
        writeLine(strings);
    }

    private void writeLine(String[] strings) throws IOException {
        String line = String.join(",", strings) + '\n';
        writer.write(line);
    }
}
