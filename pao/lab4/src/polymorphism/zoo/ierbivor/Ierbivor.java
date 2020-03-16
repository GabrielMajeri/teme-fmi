package polymorphism.zoo.ierbivor;

import polymorphism.zoo.Animal;

public abstract class Ierbivor extends Animal {
    public Ierbivor(String nume, int vârstă) {
        super(nume, vârstă);
        this.tipHrană = "vegetație";
    }

    @Override
    public void seHrănește() {
        System.out.println(this + " se hrănește cu " + tipHrană);
    }
}
