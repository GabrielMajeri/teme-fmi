package polymorphism.zoo;

public abstract class Animal {
    private int nrIdentificare;
    private String nume;
    private int vârstă;

    protected String tipHrană;
    protected String sunetSpecific;

    private static int id = 0;

    public Animal(String nume, int vârstă) {
        this.nume = nume;
        this.vârstă = vârstă;
        this.nrIdentificare = hashCode();
    }

    public abstract void seHrănește();
    public abstract void scoateSunet();
    public void afișeazăDetalii() {
        System.out.println("Acesta este " + this);
    }

    public String getNume() {
        return nume;
    }

    @Override
    public String toString() {
        return "Animal din categoria " + this.getClass().getSuperclass().getSimpleName() +
                ", din specia " + this.getClass().getSimpleName() + "{" +
                "nume='" + nume + "'" +
                ", vârstă=" + vârstă +
                ", nrIdentificare=" + nrIdentificare +
                '}';
    }
}
