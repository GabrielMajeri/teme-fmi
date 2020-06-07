package jobs.utils;

public class IdAllocator {
    private final int maxId;
    private int counter;

    public IdAllocator(int start, int maxNumIds) {
        this.counter = start;
        this.maxId = start + maxNumIds;
    }

    public synchronized int next() {
        if (counter == maxId) {
            throw new RuntimeException("Cannot allocate new ID, limit reached");
        }

        return counter++;
    }
}
