public class FormTest {
    public static void main(String[] args) {
        int numForms = 3;

        Form[] forms = new Form[10];
        forms[0] = new Triangle("rosu", 3, 5);
        forms[1] = new Circle("verde", 2);
        forms[2] = new Triangle("albastru", 1, 2);

        for (int i = 0; i < numForms; ++i) {
            Form form = forms[i];
            System.out.print(form + " ");
            if (form instanceof Triangle) {
                Triangle triangle = (Triangle)form;
                triangle.printTriangleDimensions();
            } else if (form instanceof Circle) {
                Circle circle = (Circle)form;
                circle.printCircleDimensions();
            }
        }

        for (int i = 0; i < numForms; ++i) {
            forms[i].printDimensions();
        }
    }
}
