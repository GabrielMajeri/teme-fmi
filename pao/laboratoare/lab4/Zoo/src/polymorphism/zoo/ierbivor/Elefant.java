package polymorphism.zoo.ierbivor;

public class Elefant extends Ierbivor {
    public Elefant(String nume, int vârstă) {
        super(nume, vârstă);
        this.sunetSpecific = "trâmbițează";
    }

    @Override
    public void scoateSunet() {
        System.out.println("Elefantul " + sunetSpecific);
    }
}
