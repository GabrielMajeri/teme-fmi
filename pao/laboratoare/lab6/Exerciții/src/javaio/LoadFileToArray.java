package javaio;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class LoadFileToArray {
    public static List<String> readFileToArray(String path) {
        try (FileReader fileReader = new FileReader(path);
             BufferedReader reader = new BufferedReader(fileReader)) {
            ArrayList<String> lines = new ArrayList<>();
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
            System.out.println("Read " + lines.size() + " lines.");
            return lines;
        } catch (FileNotFoundException e) {
            System.err.println("Cannot find `" + path + "`");
            System.exit(1);
            return null;
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        readFileToArray("words.txt");
    }
}
