import java.util.Comparator;
import java.util.stream.Stream;

public class Persoana {
    private String nume, prenume;
    private int varsta;

    public Persoana(String nume, String prenume, int varsta) {
        this.nume = nume;
        this.prenume = prenume;
        this.varsta = varsta;
    }

    @Override
    public String toString() {
        return nume + " " + prenume + ", " + varsta + " ani";
    }

    private static Stream<Persoana> persoane() {
        Stream.Builder<Persoana> builder = Stream.builder();

        builder.add(new Persoana("Popescu", "Ion", 23));
        builder.add(new Persoana("Cutarescu", "Maria", 18));
        builder.add(new Persoana("Marian", "Vasile", 51));
        builder.add(new Persoana("Smith", "Stefan", 13));

        return builder.build();
    }

    private static void afisare(Stream<Persoana> persoane) {
        System.out.println("Toate persoanele:");
        persoane.forEach(System.out::println);
    }

    private static void sortareDupaPrenume(Stream<Persoana> persoane) {
        System.out.println("Sortate dupa prenume:");
        persoane.sorted(Comparator.comparing(a -> a.prenume))
                .forEach(System.out::println);
    }

    private static void filtrareDupaInitiala(Stream<Persoana> persoane, char initiala) {
        System.out.println("Filtrate sa aiba initiala numelui '" + initiala + "':");
        persoane.filter(persoana -> persoana.nume.charAt(0) == initiala)
                .forEach(System.out::println);
    }

    private static void sortareDupaVarsta(Stream<Persoana> persoane) {
        System.out.println("Sortate dupa varsta:");
        persoane.sorted(Comparator.comparing(a -> a.varsta))
                .forEach(System.out::println);
    }

    private static void ceaMaiTanara(Stream<Persoana> persoane) {
        System.out.println("Cea mai tanara persoana:");
        Persoana ceaMaiTanara = persoane.min(Comparator.comparing(a -> a.varsta))
                .orElseThrow();
        System.out.println(ceaMaiTanara);
    }

    public static void main(String[] args) {
        afisare(persoane());

        sortareDupaPrenume(persoane());

        filtrareDupaInitiala(persoane(), 'P');

        sortareDupaVarsta(persoane());

        ceaMaiTanara(persoane());
    }
}
