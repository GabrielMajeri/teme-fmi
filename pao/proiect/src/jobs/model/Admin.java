package jobs.model;

public final class Admin extends User {
    private final Admin admin = new Admin();

    private Admin() {
        super("admin");
    }

    public Admin getAdminAccount() {
        return admin;
    }
}
