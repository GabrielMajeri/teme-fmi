package strings;

public class Main {
    public static void main(String[] args) {
        String s1 = "";
        String s2 = "abc";
        System.out.println(s2);

        s2.toUpperCase();
        System.out.println(s2);

        s2 = s2.toUpperCase();
        System.out.println(s2);

        s2 = "abc";

        String s3 = new String("abc");
        System.out.println(s3);

        System.out.println(s2 == s3);
        System.out.println(s2.equals(s3));

        s3 = s3.intern();

        System.out.println(s2 == s3);

        String s4 = "abc \n de\tf";
        System.out.println(s4);

        String adresa = s1 + s2 + s3.toUpperCase() + s4.length();
        System.out.println(adresa);

        StringBuilder sb = new StringBuilder();
        sb.append(s1).append(s2)
                .append(s3.toUpperCase())
                .append(s4.length());
        System.out.println(sb);

        StringBuffer buffer = new StringBuffer();
        buffer.append(12345);
        System.out.println(buffer);
    }
}
