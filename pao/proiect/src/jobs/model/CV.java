package jobs.model;

import java.util.Objects;

public class CV {
    private final Candidate candidate;
    private final String description;

    public CV(Candidate candidate, String description) {
        Objects.requireNonNull(candidate);
        Objects.requireNonNull(description);
        this.candidate = candidate;
        this.description = description;
    }

    public int getCandidateUserId() {
        return candidate.getId();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CV cv = (CV) o;
        return candidate.equals(cv.candidate) &&
                description.equals(cv.description);
    }

    @Override
    public int hashCode() {
        return Objects.hash(candidate, description);
    }

    @Override
    public String toString() {
        return "CV{" +
                "candidate=" + candidate + ", " +
                "description='" + description + '\'' +
                '}';
    }
}
