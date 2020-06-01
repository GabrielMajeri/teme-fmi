package jobs.model;

import jobs.utils.IdAllocator;

public final class Admin extends User {
    private static final IdAllocator adminIds = new IdAllocator(1, 8);

    public Admin(Name name) {
        super(adminIds.next(), name);
    }
}
