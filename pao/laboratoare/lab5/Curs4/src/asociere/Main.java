package asociere;

public class Main {
    public static void main(String[] args) {
        Profesor profesorPrincipalMate = new Profesor("Popescu");
        Profesor profesorSecundarMate = new Profesor("Ionescu");

        Profesor profesorInfo = new Profesor("Angel");
        Profesor profesorMateSiInfo = new Profesor("Petrescu");
        Profesor profesorInfoSiMate = new Profesor("Andrei");

        Departament departamentInfo = new Departament("Informatică");
        departamentInfo.setProfesori(new Profesor[]{ profesorInfo, profesorInfoSiMate, profesorMateSiInfo });

        Departament departamentMate = new Departament("Matematică");
        departamentMate.setProfesori(new Profesor[]{ profesorPrincipalMate, profesorSecundarMate, profesorMateSiInfo, profesorInfoSiMate });

        Departament[] departamente = new Departament[]{ departamentInfo, departamentMate };
        Universitate universitate = new Universitate("Universitatea din București", departamente);

        System.out.println(universitate);
    }
}
