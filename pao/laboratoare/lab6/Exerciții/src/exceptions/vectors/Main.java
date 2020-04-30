package exceptions.vectors;

import java.io.*;
import java.util.*;

public class Main {
    public static void main(String[] args) {
        String testFilePath = "exceptions.vectors.txt";

        try {
            createTestFile(testFilePath);

            Vector<Integer> intVector = new Vector<>();
            Vector<Double> doubleVector = new Vector<>();

            try (BufferedReader reader = new BufferedReader(new FileReader(testFilePath))) {
                Scanner scanner = new Scanner(reader);

                while (scanner.hasNext()) {
                    if (scanner.hasNextInt()) {
                        int number = scanner.nextInt();
                        System.out.println("Found integer: " + number);
                        intVector.add(number);
                    } else if (scanner.hasNextDouble()) {
                        double number = scanner.nextDouble();
                        System.out.println("Found double: " + number);
                        doubleVector.add(number);
                    } else {
                        System.err.println("Unknown element in file: " + scanner.next());
                    }
                }
            }

            Collections.sort(intVector);
            Collections.sort(doubleVector);

            System.out.println("Integers: " + intVector);
            System.out.println("Doubles: " + doubleVector);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void createTestFile(String path) throws IOException {
        File file = new File(path);

        if (!file.createNewFile()) {
            System.out.println("Test file already exists");
            return;
        }

        try (FileWriter fileWriter = new FileWriter(file);
             BufferedWriter writer = new BufferedWriter(fileWriter)) {
            Random random = new Random();

            int size = 5 + random.nextInt(6);
            System.out.println("Generating " + size + " random numbers");

            for (int i = 0; i < size; ++i) {
                if (random.nextBoolean()) {
                    // write an int
                    writer.write(Integer.toString(random.nextInt()));
                } else {
                    // write a double
                    writer.write(Double.toString(random.nextDouble()));
                }
                writer.write(" ");
            }
        }
    }
}
