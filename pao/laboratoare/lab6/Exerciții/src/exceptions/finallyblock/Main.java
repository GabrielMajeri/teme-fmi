package exceptions.finallyblock;

public class Main {
    private static int finallyReturn(boolean doException) {
        try {
            if (doException) {
                throw new RuntimeException("Something went wrong");
            }
            return 42;
        } finally {
            System.out.println("Finally hello!");
        }
    }

    public static void main(String[] args) {
        try {
            int value = finallyReturn(false);
            System.out.println("Returned " + value);
        } catch (Exception e) {
            e.printStackTrace();
        }

        try {
            int value = finallyReturn(true);
            System.out.println("Returned " + value);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
