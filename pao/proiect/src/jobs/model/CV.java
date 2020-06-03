package jobs.model;

import jobs.utils.IdAllocator;

import java.util.Objects;

public class CV {
    public final int id;
    public final int candidateId;
    public final String description;

    private final static IdAllocator cvIds = new IdAllocator(1, 30000);

    public CV(int id, int candidateId, String description) {
        this.id = id;
        this.candidateId = candidateId;
        this.description = Objects.requireNonNull(description);
    }

    public CV(Candidate candidate, String description) {
        this(cvIds.next(), Objects.requireNonNull(candidate).id, description);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CV cv = (CV) o;
        return candidateId == cv.candidateId &&
                description.equals(cv.description);
    }

    @Override
    public int hashCode() {
        return Objects.hash(candidateId, description);
    }

    @Override
    public String toString() {
        return "CV{" +
                "candidateId=" + candidateId + ", " +
                "description='" + description + '\'' +
                '}';
    }
}
