package csv;

public interface CsvTypeFactory<T> {
    /**
     * Get the names of the columns of the CSV files.
     *
     * @return array of strings describing each object in the object's string
     * array representation
     */
    String[] getColumnNames();

    /**
     * Serializes the object's state to an array of strings.
     * <p>
     * The strings will be written out to a file, quoted and separated by commas.
     *
     * @return array of strings containing the object's current state
     */
    String[] toStringArray(T object);

    /**
     * Updates the object's values from an array of strings.
     * <p>
     * The strings are read from a comma-separated file, and are
     * in the same order as they were given by toStringArray.
     *
     * @param data the state of the current object, as a string array
     */
    T fromStringArray(String[] data);
}
