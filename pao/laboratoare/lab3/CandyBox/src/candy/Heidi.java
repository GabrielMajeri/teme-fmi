package candy;

public class Heidi extends CandyBox {
    private float length;

    public Heidi() {
        super("Heidi", "Switzerland");
        this.length = 1;
    }

    public Heidi(float length) {
        super("Heidi", "Switzerland");
        this.length = length;
    }

    @Override
    float getVolume() {
        return length * length * length;
    }

    public void printHeidiDim() {
        System.out.println(length);
    }
}
