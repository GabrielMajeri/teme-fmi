package polymorphism.zoo.omnivor;

public class Câine extends Omnivor {
    public Câine(String nume, int vârstă) {
        super(nume, vârstă);
        this.sunetSpecific = "latră";
    }

    @Override
    public void scoateSunet() {
        System.out.println("Câine " + sunetSpecific);
    }
}
