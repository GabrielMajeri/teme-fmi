import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class IntStreamTest {
    static boolean isEven(int n) {
        return n % 2 == 0;
    }

    static String integersToString(IntStream integers) {
        return integers.mapToObj(n -> isEven(n) ? "p" + n : "i" + n)
                .collect(Collectors.joining(","));
    }

    public static void main(String[] args) {
        IntStream stream = IntStream.range(1, 10);

        String result = integersToString(stream);

        System.out.println(result);
    }
}
