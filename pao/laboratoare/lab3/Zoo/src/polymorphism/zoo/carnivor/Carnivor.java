package polymorphism.zoo.carnivor;

import polymorphism.zoo.Animal;

public abstract class Carnivor extends Animal {
    public Carnivor(String nume, int vârstă) {
        super(nume, vârstă);
        this.tipHrană = "carne";
    }

    @Override
    public void seHrănește() {
        System.out.println(this + " se hrănește cu " + tipHrană);
    }
}
