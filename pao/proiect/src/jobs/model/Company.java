package jobs.model;

import csv.CsvTypeFactory;
import csv.CsvUtils;
import jobs.utils.IdAllocator;

/**
 * An employer which posts jobs, and hires candidates through recruiters.
 */
public class Company implements Comparable<Company> {
    public final static CsvTypeFactory<Company> FACTORY = new CsvTypeFactory<Company>() {
        @Override
        public String[] getColumnNames() {
            return new String[]{"id", "name"};
        }

        @Override
        public String[] toStringArray(Company company) {
            return new String[]{Integer.toString(company.id), company.name};
        }

        @Override
        public Company fromStringArray(String[] data) {
            int id = Integer.parseInt(data[0]);
            String name = data[1];
            return new Company(id, name);
        }
    };
    private final static IdAllocator companyIds = new IdAllocator(100000, 500000);
    public final int id;
    public final String name;

    public Company(int id, String name) {
        this.id = id;
        this.name = CsvUtils.requireNoComma(name);
    }

    public Company(String name) {
        this(companyIds.next(), name);
    }

    @Override
    public String toString() {
        return "Company{" +
                "name='" + name + "', " +
                "cui=" + id +
                "}";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Company company = (Company) o;
        return id == company.id;
    }

    @Override
    public int hashCode() {
        return id;
    }

    @Override
    public int compareTo(Company company) {
        return this.name.compareTo(company.name);
    }
}
