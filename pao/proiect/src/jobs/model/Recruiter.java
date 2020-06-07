package jobs.model;

import csv.CsvTypeFactory;
import jobs.utils.IdAllocator;

import java.util.Arrays;
import java.util.stream.Stream;

public final class Recruiter extends User {
    public final static CsvTypeFactory<Recruiter> FACTORY = new CsvTypeFactory<Recruiter>() {
        @Override
        public String[] getColumnNames() {
            Stream<String> columns = Arrays.stream(new String[]{"id", "companyId"});
            Stream<String> nameColumns = Arrays.stream(Name.FACTORY.getColumnNames());
            return Stream.concat(columns, nameColumns).toArray(String[]::new);
        }

        @Override
        public String[] toStringArray(Recruiter recruiter) {
            String id = Integer.toString(recruiter.id);
            String companyId = Integer.toString(recruiter.companyId);
            Stream<String> values = Arrays.stream(new String[]{id, companyId});
            Stream<String> name = Arrays.stream(Name.FACTORY.toStringArray(recruiter.name));
            return Stream.concat(values, name).toArray(String[]::new);
        }

        @Override
        public Recruiter fromStringArray(String[] data) {
            int id = Integer.parseInt(data[0]);
            int companyId = Integer.parseInt(data[1]);
            Name name = Name.FACTORY.fromStringArray(Arrays.copyOfRange(data, 2, 5));
            return new Recruiter(id, name, companyId);
        }
    };

    private final static IdAllocator recruiterIds = new IdAllocator(-5000, 5000);

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
