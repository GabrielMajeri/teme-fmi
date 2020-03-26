package asociere;

public class Profesor {
    private int id;
    private String nume;

    public Profesor(String nume) {
        this.id = hashCode();
        this.nume = nume;
    }

    @Override
    public String toString() {
        return "Profesor{'" + nume + "'}";
    }
}
