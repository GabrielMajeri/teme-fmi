package polymorphism.zoo.omnivor;

import polymorphism.zoo.Animal;

public abstract class Omnivor extends Animal {
    public Omnivor(String nume, int vârstă) {
        super(nume, vârstă);
        this.tipHrană = "carne și vegetație";
    }

    @Override
    public void seHrănește() {
        System.out.println(this + " se hrănește cu " + tipHrană);
    }
}
