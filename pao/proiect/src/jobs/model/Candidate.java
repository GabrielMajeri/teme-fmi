package jobs.model;

import jobs.utils.IdAllocator;

public final class Candidate extends User {
    private final static IdAllocator candidateIds = new IdAllocator(1000, 20_000);

    public Candidate(int id, Name name) {
        super(id, name);
    }

    public Candidate(Name name) {
        this(candidateIds.next(), name);
    }
}
