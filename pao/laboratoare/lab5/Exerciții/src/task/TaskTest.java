package task;

import container.Container;
import container.Queue;
import container.Stack;

public class TaskTest {
    public static void main(String[] args) {
        Container container1 = new Stack();
        Container container2 = new Queue();

        System.out.println("Testing LIFO container");
        addTasksAndRun(container1);
        System.out.println("Testing FIFO container");
        addTasksAndRun(container2);
    }

    private static void addTasksAndRun(Container container) {
        container.addTask(new OutTask());
        container.addTask(new CounterOutTask());
        container.addTask(new RandomOutTask());
        container.addTask(new CounterOutTask());
        container.addTask(new OutTask());
        container.addTask(new CounterOutTask());
        container.addTask(new OutTask());

        while (container.hasTask()) {
            container.runTask();
        }
    }
}
