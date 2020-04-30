import java.util.HashSet;
import java.util.TreeSet;

public class Student implements Comparable<Student> {
    private String nume;
    private float medie;

    public Student(String nume, float medie) {
        this.nume = nume;
        if (medie < 0 || medie > 10) {
            throw new IllegalArgumentException("Medie incorectă");
        }
        this.medie = medie;
    }

    public String getNume() {
        return nume;
    }

    public float getMedie() {
        return medie;
    }

    @Override
    public String toString() {
        return "Student{" +
                "nume='" + nume + '\'' +
                ", medie=" + medie +
                '}';
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof Student)) {
            return false;
        }
        Student student = (Student)obj;
        return nume.equals(student.nume) && medie == student.medie;
    }

    /*
    @Override
    public boolean equals(Object obj) {
        return false;
    }
    */

    @Override
    public int compareTo(Student student) {
        int result = nume.compareTo(student.nume);
        if (result != 0) {
            return result;
        } else {
            return Float.compare(medie, student.medie);
        }
    }

    @Override
    public int hashCode() {
        return nume.hashCode() ^ Float.hashCode(medie);
    }

    public static void main(String[] args) {
        TreeSet<Student> treeSet = new TreeSet<>();
        HashSet<Student> hashSet = new HashSet<>();

        for (int i = 0; i < 5; ++i) {
            Student student = new Student("Ion", 8.75F);
            treeSet.add(student);
            hashSet.add(student);
        }

        System.out.println(treeSet);
        System.out.println(hashSet);
    }
}

/*
5. Creaţi o clasǎ care moşteneşte HashSet<Integer>.
a. Definiţi în aceastǎ clasǎ o variabilǎ membru care reţine numǎrul total de elemente adǎugate. Pentru a contoriza acest lucru,
suprascrieți metodele add şi addAll. Pentru adǎugarea efectivǎ a elementelor, folosiţi implementǎrile din clasa pǎrinte (HashSet).
b. Testaţi folosind atât add cât şi addAll. Ce observaţi? Corectaţi dacǎ este cazul.
c. Modificaţi implementarea astfel încât clasa voastrǎ sǎ moşteneascǎ LinkedList<Integer>. Ce observaţi? Ce concluzii trageţi?

 */
