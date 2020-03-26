package candy;

public class Lindt extends CandyBox {
    private float width, height, length;

    public Lindt() {
        super("Lindt", "Switzerland");
        this.width = 1;
        this.height = 1;
        this.length = 1;
    }

    public Lindt(float width, float height, float length) {
        super("Lindt", "Switzerland");
        this.width = width;
        this.height = height;
        this.length = length;
    }

    @Override
    float getVolume() {
        return width * height * length;
    }

    public void printLindtDim() {
        System.out.println(width + " " + height + " " + length);
    }
}
