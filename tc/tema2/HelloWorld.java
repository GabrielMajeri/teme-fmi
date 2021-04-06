// This is a class.
class HelloWorld {
    private final static int MY_CONSTANT = 1234;

    public static void main(String[] args) {
        System.out.println("Hello, world!");

        String myString = "Test";
        int x = 5;
        if (x > 3) {
            System.out.println(myString);
        }

        System.out.println(HelperClass.compute(convert(x)));

        while (x < 10) {
            x = x + 1;
            System.out.println("Hello!");
        }
    }

    // This is a private static method
    private static String convert(int input) {
        return input.toString();
    }

    /** This is an instance method. */
    public int getValue() {
        return 15;
    }
}
