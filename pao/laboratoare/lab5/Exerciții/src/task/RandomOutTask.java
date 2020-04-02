package task;

import java.util.Random;

public class RandomOutTask implements Task {
    private int randomNum;

    public RandomOutTask() {
        randomNum = new Random().nextInt();
    }

    @Override
    public void run() {
        System.out.println("Random number: " + randomNum);
    }
}
