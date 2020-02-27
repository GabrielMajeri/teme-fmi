package hello;

import java.util.Scanner;

public class ReadName {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        String name = scanner.nextLine();

        System.out.println("Hello " + name + " I am Java");
    }
}
