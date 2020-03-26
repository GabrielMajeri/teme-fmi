package arrays;

import java.util.Random;

public class MyArrayListTest {
    public static void main(String[] args) {
        testCreateAndGet();
        useArrayList(10);
    }

    public static void testCreateAndGet() {
        MyArrayList list = new MyArrayList(5);

        list.add(3.5f);
        list.add(2.4f);
        list.add(-1);

        System.out.println("List: " + list);
        System.out.println("Test get: " +
                (list.get(0) == 3.5f) + " "
                        + (list.get(1) == 2.4f) + " "
                        + (list.get(2) == -1)
        );
    }

    public static void useArrayList(int testSize) {
        MyArrayList list = new MyArrayList(testSize / 2);

        Random random = new Random();
        float[] values = new float[testSize];
        for (int i = 0; i < testSize; ++i) {
            values[i] = random.nextFloat();
        }

        for (int i = 0; i < testSize; ++i) {
            list.add(values[i]);
        }

        System.out.println("Initial list: " + list);

        for (int i = 0; i < testSize / 2; ++i) {
            int index = random.nextInt(testSize);
            float value = values[index];
            assert list.contains(value);
        }

        for (int i = 0; i < testSize / 2; ++i) {
            int index = random.nextInt(testSize - i);
            list.remove(index);
            assert list.size() == testSize - i - 1;
        }

        System.out.println("Final list: " + list);
    }
}

