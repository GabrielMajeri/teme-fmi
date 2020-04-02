package container;

import task.Task;

import java.util.List;
import java.util.LinkedList;

public class Queue implements Container {
    private List<Task> list = new LinkedList<>();

    @Override
    public void addTask(Task task) {
        list.add(task);
    }

    @Override
    public boolean hasTask() {
        return !list.isEmpty();
    }

    @Override
    public void runTask() {
        Task task = list.get(0);
        list.remove(0);
        task.run();
    }
}
