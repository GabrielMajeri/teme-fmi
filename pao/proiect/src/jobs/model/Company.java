package jobs.model;

import csv.CsvTypeFactory;

public class Company implements Comparable<Company> {
    private final String name;
    private final int id;

    public final static CsvTypeFactory<Company> FACTORY = new CsvTypeFactory<Company>() {
        @Override
        public String[] getColumnNames() {
            return new String[]{"name", "id"};
        }

        @Override
        public String[] toStringArray(Company company) {
            return new String[]{company.name, Integer.toString(company.id)};
        }

        @Override
        public Company fromStringArray(String[] data) {
            return new Company(data[0], Integer.parseInt(data[1]));
        }
    };

    public Company(String name, int id) {
        this.name = name;
        this.id = id;
    }

    public String getName() {
        return name;
    }
    public int getId() { return id; }

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
