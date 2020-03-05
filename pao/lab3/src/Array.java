import java.util.Arrays;

public class Array {
    public static void main(String[] args) {
        // Declare and display an array
        byte[] bytes;

        bytes = new byte[5];
        bytes[0] = -128;
        bytes[4] = 127;

        for (byte i = 0; i < bytes.length; ++i) {
            System.out.println(bytes[i]);
        }

        // Print an array (displays the address)
        System.out.println(bytes);

        // Print an array in a human-readable format
        System.out.println(Arrays.toString(bytes));
    }
}
