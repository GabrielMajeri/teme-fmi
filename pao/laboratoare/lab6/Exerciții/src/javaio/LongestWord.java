package javaio;

import java.io.*;
import java.util.List;

import static javaio.LoadFileToArray.readFileToArray;

public class LongestWord {
    public static void main(String[] args) {
        List<String> lines = readFileToArray("words.txt");

        String longestWord = "";
        for (String line : lines) {
            String[] words = line.split("([ ,!\\-])");
            for (String word : words) {
                if (word.length() > longestWord.length()) {
                    longestWord = word;
                }
            }
        }
    }
}
