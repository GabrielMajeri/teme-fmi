package exceptions.calculator;

public class DivideByZeroException extends IllegalArgumentException {
    public DivideByZeroException(String message) {
        super(message);
    }
}
