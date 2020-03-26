package arrays;

import java.util.Arrays;

public class MyArrayList {
    private int capacity;
    private float[] values;
    private int size;

    public MyArrayList() {
        this(10);
    }

    public MyArrayList(int capacity) {
        setCapacity(capacity);
    }

    public void setCapacity(int newCapacity) {
        if (newCapacity > capacity) {
            if (capacity == 0) {
                values = new float[newCapacity];
            } else {
                values = Arrays.copyOf(values, newCapacity);
            }

            capacity = newCapacity;
        }
    }

    public void add(float value) {
        if (size == capacity) {
            setCapacity(2 * capacity);
        }
        values[size++] = value;
    }

    public boolean contains(float value) {
        for (float x : values) {
            if (x == value) {
                return true;
            }
        }
        return false;
    }

    public void remove(int index) {
        if (index >= size) {
            throw new IndexOutOfBoundsException(index);
        }

        for (int i = index; i < size - 1; ++i) {
            values[i] = values[i + 1];
        }

        --size;
    }

    public float get(int index) {
        if (index >= size) {
            throw new IndexOutOfBoundsException(index);
        }

        return values[index];
    }

    public int size() {
        return size;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("MyArrayList{");

        for (int i = 0; i < size - 1; ++i) {
            builder.append(values[i]).append(", ");
        }

        builder.append(values[size - 1]).append("}");

        return builder.toString();
    }
}
