package imutabilitate;

public class Main {
    public static void main(String[] args) {
        Adresa adresa = new Adresa("Timisoara", "4A");
        Persoana persoana = new Persoana(1, "Ion", adresa);

        System.out.println(persoana);

        adresa.setStrada("Orhideea");
        persoana.getAdresa().setStrada("Orhideea");

        System.out.println(persoana);
    }
}
