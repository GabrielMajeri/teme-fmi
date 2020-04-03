package jobs.model;

public final class Recruiter extends User {
    public Recruiter(String firstName, String lastName, Company company) {
        super(company.getName() + "_" + firstName + "_" + lastName);
    }
}
