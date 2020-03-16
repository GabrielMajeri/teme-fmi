package candy;

public class Milka extends CandyBox {
    private float height, radius;

    public Milka() {
        this.height = 1;
        this.radius = 1;
    }

    public Milka(float height, float radius) {
        super("Milka", "Germany");
        this.height = height;
        this.radius = radius;
    }

    @Override
    float getVolume() {
        return height * (float)Math.PI * (radius * radius);
    }

    public void printMilkaDim() {
        System.out.println(height + " " + radius);
    }
}
