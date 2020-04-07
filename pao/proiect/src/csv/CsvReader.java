package csv;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Arrays;

public class CsvReader<T extends CsvSerializable> {
    private BufferedReader reader;

    public CsvReader(BufferedReader reader, T object) throws IOException {
        this.reader = reader;
        if (object != null) {
            String header = reader.readLine();
            String[] columns = header.split(",");
            boolean headersMatch = Arrays.equals(columns, object.getColumnNames());
            if (!headersMatch) {
                throw new RuntimeException("headers of CSV file do not match");
            }
        }
    }

    public boolean hasMoreObjects() throws IOException {
        return reader.ready();
    }

    public void readObject(T object) throws IOException {
        String line = reader.readLine();
        String[] values = line.split(",");
        object.fromStringArray(values);
    }
}
