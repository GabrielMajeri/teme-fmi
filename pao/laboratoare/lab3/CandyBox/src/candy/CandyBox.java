package candy;

import java.util.Objects;

public class CandyBox {
    private String flavor;
    private String origin;

    public CandyBox() {
        flavor = "plain";
        origin = "unknown";
    }

    public CandyBox(String flavor, String origin) {
        this.flavor = flavor;
        this.origin = origin;
    }

    float getVolume() {
        return 0;
    }

    @java.lang.Override
    public java.lang.String toString() {
        return "The " + origin + " " + flavor + " has volume " + getVolume();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CandyBox candyBox = (CandyBox) o;
        // Fiecare tip de ciocolată are o singură origine,
        // nu este nevoie să comparăm și originea.
        return flavor.equals(candyBox.flavor);
    }

    @Override
    public int hashCode() {
        return Objects.hash(flavor, origin);
    }
}
