package jobs.model;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

public class Password {
    private final static int SALT_LENGTH_BYTES = 32;
    public final String saltedPassword;
    public final String salt;

    public Password(String plainText, Random rng) {
        this.salt = generateRandomSalt(rng);
        this.saltedPassword = hashPassword(plainText + salt);
    }

    private static String generateRandomSalt(Random rng) {
        byte[] buffer = new byte[SALT_LENGTH_BYTES];
        rng.nextBytes(buffer);
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

    /**
     * Checks if a given password is correct for this user.
     *
     * @param plainText the input password, in plain text
     * @return whether the given password is correct
     */
    public boolean checkPassword(String plainText) {
        return hashPassword(plainText + salt).equals(saltedPassword);
    }
}
