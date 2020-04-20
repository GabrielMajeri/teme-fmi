package jobs.model;

public final class Candidate extends User {
    private String firstName, lastName;

    public Candidate(String userName, String firstName, String lastName) {
        super(userName);
        this.firstName = firstName;
        this.lastName = lastName;
    }
}
