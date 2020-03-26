package polymorphism.zoo.omnivor;

public class Urs extends Omnivor {
    public Urs(String nume, int vârstă) {
        super(nume, vârstă);
        this.sunetSpecific = "mormăie";
    }

    @Override
    public void scoateSunet() {
        System.out.println("Urs " + sunetSpecific);
    }
}
