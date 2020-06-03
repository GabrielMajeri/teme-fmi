package jobs.model;

import jobs.utils.IdAllocator;

public final class Recruiter extends User {
    private static final IdAllocator recruiterIds = new IdAllocator(-5000, 5000);

    public final int companyId;

    public Recruiter(int id, Name name, int companyId) {
        super(id, name);
        this.companyId = companyId;
    }

    public Recruiter(Name name, Company company) {
        this(recruiterIds.next(), name, company.id);
    }

    @Override
    public String toString() {
        return super.toString() + " @ " + companyId;
    }
}
