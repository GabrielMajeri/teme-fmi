package polymorphism.zoo.ierbivor;

public class Cal extends Ierbivor {
    public Cal(String nume, int vârstă) {
        super(nume, vârstă);
        this.sunetSpecific = "nechează";
    }

    @Override
    public void scoateSunet() {
        System.out.println("Cal " + sunetSpecific);
    }
}
