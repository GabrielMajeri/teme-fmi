package jobs.model;

import csv.CsvSerializable;

public class Company implements CsvSerializable {
    private String name;
    private int id;

    public Company() {
    }

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
    public String[] getColumnNames() {
        return new String[]{ "id", "name" };
    }

    @Override
    public String[] toStringArray() {
        return new String[]{ Integer.toString(id) , name };
    }

    @Override
    public void fromStringArray(String[] data) {
        id = Integer.parseInt(data[0]);
        name = data[1];
    }
}
