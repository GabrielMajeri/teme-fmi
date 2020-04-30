package javaio;

import java.io.*;
import java.util.List;

import static javaio.LoadFileToArray.readFileToArray;

public class SaveJavaClass {
    public static void writeLinesToFile(List<String> lines, String path, boolean append) {
        try (FileWriter fileWriter = new FileWriter(path, append);
             BufferedWriter writer = new BufferedWriter(fileWriter)) {
            for (String line : lines) {
                writer.write(line + '\n');
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        List<String> lines = readFileToArray("src/javaio/SaveJavaClass.java");
        writeLinesToFile(lines, "words.txt", true);
    }
}
