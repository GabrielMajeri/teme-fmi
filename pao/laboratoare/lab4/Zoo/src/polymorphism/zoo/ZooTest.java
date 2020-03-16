package polymorphism.zoo;

import polymorphism.zoo.carnivor.*;
import polymorphism.zoo.ierbivor.*;
import polymorphism.zoo.omnivor.*;

import java.util.Scanner;

public class ZooTest {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Introduceți numărul maxim de animale din zoo:");
        int nrAnimale = scanner.nextInt();

        scanner.close();

        Zoo zooBucurești = new Zoo(nrAnimale);

        adaugăAnimaleLaZoo(zooBucurești);

        zooBucurești.afișeazăAnimale();
    }

    public static void adaugăAnimaleLaZoo(Zoo zoo) {
        Leu leu = new Leu("Simba", 7);
        zoo.adaugăAnimal(leu);

        Elefant elefant = new Elefant("Eli", 10);
        zoo.adaugăAnimal(elefant);

        Urs urs = new Urs("Fram", 4);
        zoo.adaugăAnimal(urs);

        Pisică pisică = new Pisică("Tom", 2);
        zoo.adaugăAnimal(pisică);

        Câine câine = new Câine("Toto", 3);
        zoo.adaugăAnimal(câine);

        Cal cal = new Cal("Thunder", 3);
        zoo.adaugăAnimal(cal);
    }

}
