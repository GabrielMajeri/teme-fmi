package asociere;

public class Departament {
    private String nume;
    private Profesor[] profesori;

    public Departament(String nume) {
        this.nume = nume;
    }

    public void setProfesori(Profesor[] profesori) {
        this.profesori = profesori;
    }
}
