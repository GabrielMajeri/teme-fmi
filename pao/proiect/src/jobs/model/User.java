package jobs.model;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

public abstract class User {
    private int id;
    private String name;
    private String saltedPassword;
    private String salt;

    private final static int SALT_LENGTH_BYTES = 32;
    private static int userCounter;

    protected User(String name) {
        this.id = ++userCounter;
        this.name = name;
    }

    public void setPassword(String plainText) {
        salt = generateRandomString(SALT_LENGTH_BYTES);
        saltedPassword = hashPassword(plainText + salt);
    }

    private static String generateRandomString(int length) {
        byte[] buffer = new byte[length];
        new Random().nextBytes(buffer);

        return new String(buffer, StandardCharsets.UTF_8);
    }

    private static String hashPassword(String password) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-512");
            byte[] passwordDigest = digest.digest(password.getBytes());
            BigInteger passwordInteger = new BigInteger(1, passwordDigest);
            return passwordInteger.toString(16);
        } catch (NoSuchAlgorithmException e) {
            System.err.println("Your system does not support" +
                    "the required cryptographic hashing function.");
            e.printStackTrace();
            System.exit(1);
            return null;
        }
    }

    @Override
    public String toString() {
        return name;
    }
}
