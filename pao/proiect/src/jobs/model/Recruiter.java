package jobs.model;

public final class Recruiter extends User {
    private Company company;

    public Recruiter(String firstName, String lastName, Company company) {
        super(firstName + "_" + lastName);
        this.company = company;
    }

    @Override
    public String toString() {
        return super.toString() + "@" + this.company.getName().toLowerCase() + ".com";
    }
}
