package container;

import task.Task;

import java.util.List;
import java.util.Vector;

public class Stack implements Container {
    List<Task> list = new Vector<>();

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
        int lastPosition = list.size() - 1;
        Task task = list.get(lastPosition);
        list.remove(lastPosition);
        task.run();
    }
}
