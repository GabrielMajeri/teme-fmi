package javaio;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static javaio.LoadFileToArray.readFileToArray;
import static javaio.SaveJavaClass.writeLinesToFile;

public class CaesarCipher {
    private final static String LOWERCASE_ALPHABET =
            "aăâbcdefghiîjklmnopqrsștțuvwxyz";
    private final static String UPPERCASE_ALPHABET =
            LOWERCASE_ALPHABET.toUpperCase();

    private final static int ALPHABET_SIZE = LOWERCASE_ALPHABET.length();

    private final static int OFFSET = 5;

    private static char encode(char ch) {
        String alphabet = LOWERCASE_ALPHABET;
        if (Character.isUpperCase(ch)) {
            alphabet = UPPERCASE_ALPHABET;
        }
        int index = alphabet.indexOf(ch);
        if (index != -1) {
            int newIndex = (index + OFFSET) % ALPHABET_SIZE;
            return alphabet.charAt(newIndex);
        } else {
            return ch;
        }
    }

    private static char decode(char ch) {
        String alphabet = LOWERCASE_ALPHABET;
        if (Character.isUpperCase(ch)) {
            alphabet = UPPERCASE_ALPHABET;
        }
        int index = alphabet.indexOf(ch);
        if (index != -1) {
            int newIndex = (index - OFFSET + ALPHABET_SIZE) % ALPHABET_SIZE;
            return alphabet.charAt(newIndex);
        } else {
            return ch;
        }
    }

    private static String encypher(String plainText) {
        char[] encrypted = new char[plainText.length()];
        for (int i = 0; i < plainText.length(); ++i) {
            char srcChar = plainText.charAt(i);
            encrypted[i] = encode(srcChar);
        }
        return new String(encrypted);
    }

    private static String decypher(String encrypted) {
        char[] decrypted = new char[encrypted.length()];
        for (int i = 0; i < encrypted.length(); ++i) {
            char srcChar = encrypted.charAt(i);
            decrypted[i] = decode(srcChar);
        }
        return new String(decrypted);
    }

    public static void encryptFile(String inputPath, String outputPath) {
        List<String> lines = readFileToArray(inputPath);

        List<String> encrypted = lines.stream()
                .map(CaesarCipher::encypher)
                .collect(Collectors.toList());

        writeLinesToFile(encrypted, outputPath, false);
    }

    public static void decryptFile(String inputPath, String outputPath) {
        List<String> lines = readFileToArray(inputPath);

        List<String> decrypted = lines.stream()
                .map(CaesarCipher::decypher)
                .collect(Collectors.toList());

        writeLinesToFile(decrypted, outputPath, false);
    }

    public static void main(String[] args) {
        encryptFile("cuvinte.txt", "criptat.txt");
        decryptFile("criptat.txt", "decriptat.txt");

        List<String> original = readFileToArray("cuvinte.txt");
        List<String> decrypted = readFileToArray("decriptat.txt");
        System.out.println("Contents match: " + (original.equals(decrypted)));
    }
}
