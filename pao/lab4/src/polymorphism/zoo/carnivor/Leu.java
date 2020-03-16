package polymorphism.zoo.carnivor;

public class Leu extends Carnivor {
    public Leu(String nume, int vârstă) {
        super(nume, vârstă);
        this.sunetSpecific = "rage";
    }

    @Override
    public void scoateSunet() {
        System.out.println("Leul " + sunetSpecific);
    }
}
