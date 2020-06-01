package jobs.model;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Objects;
import java.util.Random;

public abstract class User {
    private final int id;
    private final Name name;
    private String saltedPassword;
    private String salt;

    private final static int SALT_LENGTH_BYTES = 32;

    protected User(int id, Name name) {
        if (id == 0) {
            throw new IllegalArgumentException("user ID cannot be 0");
        }
        Objects.requireNonNull(name);

        this.id = id;
        this.name = name;
        // Initially the user has an invalid password
        this.saltedPassword = null;
        this.salt = null;
    }

    public int getId() {
        return id;
    }

    /**
     * Sets a new password for this user.
     * @param plainText the input password, in plain text
     */
    public void setPassword(String plainText) {
        salt = generateRandomString(SALT_LENGTH_BYTES);
        saltedPassword = hashPassword(plainText + salt);
    }

    /**
     * Checks if a given password is correct for this user.
     * @param plainText the input password, in plain text
     * @return whether the given password is correct
     */
    public boolean checkPassword(String plainText) {
        if (saltedPassword == null) {
            // Password hasn't been set yet, user is not allowed to login.
            return false;
        }
        return hashPassword(plainText + salt).equals(saltedPassword);
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
            throw new RuntimeException("Your system does not support " +
                    "the required cryptographic hashing function.");
        }
    }

    @Override
    public String toString() {
        return "User{" +
                "id=" + id +
                ", name='" + name + '\'' +
                '}';
    }
}
