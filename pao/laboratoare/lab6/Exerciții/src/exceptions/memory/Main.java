package exceptions.memory;

import java.util.ArrayList;

public class Main {
    private static void memoryLeak() {
        ArrayList<int[]> arrays = new ArrayList<>();
        while (true) {
            arrays.add(new int[8192]);
        }
    }

    private static void infiniteRecursion() {
        infiniteRecursion();
    }

    public static void main(String[] args) {
        try {
            memoryLeak();
        } catch (OutOfMemoryError e) {
            System.err.println("Error: " + e.toString());
        }

        try {
            infiniteRecursion();
        } catch (StackOverflowError e) {
            System.err.println("Error: " + e.toString());
        }

        System.out.println("All memory exceptions were caught");
    }
}
