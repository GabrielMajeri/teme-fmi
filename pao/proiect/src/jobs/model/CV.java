package jobs.model;

public class CV {
    private Candidate candidate;
    private String description;

    public CV(Candidate candidate, String description) {
        this.candidate = candidate;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    @Override
    public String toString() {
        return "CV{" +
                "candidate=" + candidate + ", " +
                "description='" + description + '\'' +
                '}';
    }
}
