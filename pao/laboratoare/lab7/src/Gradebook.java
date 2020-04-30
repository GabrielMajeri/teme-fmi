import java.util.*;

public class Gradebook extends TreeMap<Integer, List<Student>> {
    private static class AlphabeticStudentComparator implements Comparator<Student> {

        @Override
        public int compare(Student a, Student b) {
            return a.getNume().compareTo(b.getNume());
        }
    }

    public Gradebook() {
        this(Comparator.reverseOrder());
    }

    public Gradebook(Comparator<Integer> comparator) {
        super(comparator);
    }

    private List<Student> getList(int key) {
        if (containsKey(key)) {
            return get(key);
        }
        List<Student> list = new ArrayList<>();
        put(key, list);
        return list;
    }

    public void addStudent(Student student) {
        int key = discretize(student.getMedie());
        getList(key).add(student);
    }

    private static int discretize(float number) {
        return Math.round(number);
    }

    public static void main(String[] args) {
        Gradebook gb = new Gradebook();
        gb.addStudent(new Student("Ion", 7.25F));
        gb.addStudent(new Student("Marius", 6.63F));
        gb.addStudent(new Student("Maria", 10));
        gb.addStudent(new Student("Elena", 4.45F));
        gb.addStudent(new Student("Florin", 8.40F));
        gb.addStudent(new Student("Stefan", 3.55F));

        for (List<Student> studentList : gb.values()) {
            studentList.sort(new AlphabeticStudentComparator());
            System.out.println(studentList);
        }
    }
}
