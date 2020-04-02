package task;

public class CounterOutTask implements Task {
    private static int counter;

    @Override
    public void run() {
        ++counter;
        System.out.println("Counter: " + counter);
    }
}
