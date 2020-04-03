package jobs.model;

public final class Candidate extends User {
    public Candidate(String firstName, String lastName) {
        super(firstName.toLowerCase() + "_" + lastName.toLowerCase());
    }
}
