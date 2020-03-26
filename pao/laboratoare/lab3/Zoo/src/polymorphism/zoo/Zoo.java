package polymorphism.zoo;

import polymorphism.zoo.carnivor.Pisică;

public class Zoo {
    private int nrMaximAnimale;
    private Animal[] animale;
    private int indexCurent;

    public Zoo(int nrMaximAnimale) {
        if (nrMaximAnimale > 0) {
            this.nrMaximAnimale = nrMaximAnimale;
            this.animale = new Animal[nrMaximAnimale];
        } else {
            throw new RuntimeException("Nu ați introdus un număr pozitiv de animale");
        }
    }

    public void adaugăAnimal(Animal animal) {
        if (indexCurent < nrMaximAnimale) {
            animale[indexCurent] = animal;
            System.out.println("Animal adăugat: " + animal.getClass().getSimpleName() + " la poziția " + indexCurent++);
        } else {
            throw new RuntimeException("Nu se poate adăuga un nou animal");
        }
    }

    public void afișeazăAnimale() {
        for (int i = 0; i < indexCurent; ++i) {
            Animal animal = animale[i];
            animal.afișeazăDetalii();
            animal.seHrănește();
            animal.scoateSunet();
        }
    }
}
