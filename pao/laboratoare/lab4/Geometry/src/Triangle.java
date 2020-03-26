import java.util.Objects;

public class Triangle extends Form {
    private float height, base;

    public Triangle() {
        this.height = 0;
        this.base = 0;
    }

    public Triangle(String color, float height, float base) {
        super(color);
        this.height = height;
        this.base = base;
    }

    public void printTriangleDimensions() {
        System.out.println("height=" + height + " base=" + base);
    }

    @Override
    public void printDimensions() {
        printTriangleDimensions();
    }

    @Override
    public float getArea() {
        return height * base / 2;
    }

    @Override
    public String toString() {
        return "Triunghi: " + super.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Triangle triangle = (Triangle) o;
        // trebuie să aibă baza și înălțimea aproximativ egală
        return Float.compare(triangle.height, height) == 0 &&
                Float.compare(triangle.base, base) == 0;
    }
}
