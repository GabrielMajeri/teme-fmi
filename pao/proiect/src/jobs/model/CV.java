package jobs.model;

import csv.CsvTypeFactory;
import csv.CsvUtils;
import jobs.utils.IdAllocator;

public class CV {
    public final int id;
    public final int candidateId;
    public final String description;

    public final static CsvTypeFactory<CV> FACTORY = new CsvTypeFactory<CV>() {
        @Override
        public String[] getColumnNames() {
            return new String[]{ "id", "candidateId", "description" };
        }

        @Override
        public String[] toStringArray(CV cv) {
            String id = Integer.toString(cv.id);
            String candidateId = Integer.toString(cv.candidateId);
            String description = cv.description;
            return new String[]{ id, candidateId, description };
        }

        @Override
        public CV fromStringArray(String[] data) {
            int id = Integer.parseInt(data[0]);
            int candidateId = Integer.parseInt(data[1]);
            String description = data[2];
            return new CV(id, candidateId, description);
        }
    };

    private final static IdAllocator cvIds = new IdAllocator(1, 30000);

    public CV(int id, int candidateId, String description) {
        this.id = id;
        this.candidateId = candidateId;
        this.description = CsvUtils.requireNoComma(description);
    }

    public CV(Candidate candidate, String description) {
        this(cvIds.next(), candidate.id, description);
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
        return candidateId;
    }

    @Override
    public String toString() {
        return "CV{" +
                "candidateId=" + candidateId + ", " +
                "description='" + description + '\'' +
                '}';
    }
}
