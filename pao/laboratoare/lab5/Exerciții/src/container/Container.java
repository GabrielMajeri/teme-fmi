package container;

import task.Task;

public interface Container {
    /**
     * Adds a task to the collection.
     */
    void addTask(Task task);

    /**
     * @return true if there still are tasks available to be run.
     */
    boolean hasTask();

    /**
     * Runs a task based on the collection's intrinsic order.
     */
    void runTask();
}
