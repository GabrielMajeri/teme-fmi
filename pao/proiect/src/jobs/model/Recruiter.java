package jobs.model;

import jobs.utils.IdAllocator;

public final class Recruiter extends User {
    private static final IdAllocator recruiterIds = new IdAllocator(-5000, 5000);
    private final Company company;

    public Recruiter(int id, Name name, Company company) {
        super(id, name);
        this.company = company;
    }

    public Recruiter(Name name, Company company) {
        this(recruiterIds.next(), name, company);
    }

    @Override
    public String toString() {
        return super.toString() + " @ " + company;
    }
}
