package jobs.model;

import csv.CsvTypeFactory;
import jobs.utils.IdAllocator;

import java.util.Arrays;
import java.util.stream.Stream;

public final class Candidate extends User {
    public final static CsvTypeFactory<Candidate> FACTORY = new CsvTypeFactory<Candidate>() {
        @Override
        public String[] getColumnNames() {
            Stream<String> columns = Arrays.stream(new String[]{"id"});
            Stream<String> nameColumns = Arrays.stream(Name.FACTORY.getColumnNames());
            return Stream.concat(columns, nameColumns).toArray(String[]::new);
        }

        @Override
        public String[] toStringArray(Candidate candidate) {
            Stream<String> id = Arrays.stream(new String[]{Integer.toString(candidate.id)});
            Stream<String> name = Arrays.stream(Name.FACTORY.toStringArray(candidate.name));
            return Stream.concat(id, name).toArray(String[]::new);
        }

        @Override
        public Candidate fromStringArray(String[] data) {
            int id = Integer.parseInt(data[0]);
            Name name = Name.FACTORY.fromStringArray(Arrays.copyOfRange(data, 1, 4));
            return new Candidate(id, name);
        }
    };

    private final static IdAllocator candidateIds = new IdAllocator(1000, 20_000);

    public Candidate(int id, Name name) {
        super(id, name);
    }

    public Candidate(Name name) {
        this(candidateIds.next(), name);
    }
}
