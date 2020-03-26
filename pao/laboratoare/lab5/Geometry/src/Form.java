public abstract class Form {
    private String color;

    public Form() {
    }

    public Form(String color) {
        this.color = color;
    }

    public float getArea() {
        return 0;
    }

    public abstract void printDimensions();

    @Override
    public String toString() {
        return "color='" + color + "' area=" + getArea();
    }
}
