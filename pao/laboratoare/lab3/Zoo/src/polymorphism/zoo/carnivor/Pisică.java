package polymorphism.zoo.carnivor;

public class Pisică extends Carnivor {
    public Pisică(String nume, int vârstă) {
        super(nume, vârstă);
        this.sunetSpecific = "miaună";
    }

    @Override
    public void scoateSunet() {
        System.out.println("Pisica " + sunetSpecific);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Pisică) {
            Pisică pisică = (Pisică)obj;
            return this.getNume().equals(pisică.getNume());
        }
        return super.equals(obj);
    }
}
