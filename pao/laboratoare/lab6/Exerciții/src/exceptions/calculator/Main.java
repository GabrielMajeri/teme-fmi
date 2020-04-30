package exceptions.calculator;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;

public class Main {
    public static void main(String[] main) {
        Calculator calc = new SimpleCalculator();

        testFunctionality(calc);
        testExceptions(calc);
    }

    private static void testFunctionality(Calculator calc) {
        try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
            Scanner scanner = new Scanner(reader);

            double a, b;

            a = scanner.nextDouble();
            b = scanner.nextDouble();

            System.out.println(calc.add(a, b));

            a = scanner.nextDouble();
            b = scanner.nextDouble();

            System.out.println(calc.divide(a, b));

            int n = scanner.nextInt();
            Double[] array = new Double[n];
            for (int i = 0; i < n; ++i) {
                array[i] = scanner.nextDouble();
            }

            System.out.println(calc.average(array));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void testExceptions(Calculator calc) {
        Double a, b;

        a = null;
        b = 3.5;
        try {
            calc.add(a, b);
        } catch (Exception e) {
            e.printStackTrace();
        }

        a = Double.MAX_VALUE;
        b = Double.MAX_VALUE;
        try {
            calc.add(a, b);
        } catch (Exception e) {
            e.printStackTrace();
        }

        a = -Double.MAX_VALUE;
        b = -Double.MAX_VALUE;
        try {
            calc.add(a, b);
        } catch (Exception e) {
            e.printStackTrace();
        }

        a = 5.2;
        b = 0.0;
        try {
            calc.divide(a, b);
        } catch (Exception e) {
            e.printStackTrace();
        }

        Double[] doubles = { 3.5, null, 2.3 };
        try {
            calc.average(doubles);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
