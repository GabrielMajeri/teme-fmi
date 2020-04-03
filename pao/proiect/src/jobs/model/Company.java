package jobs.model;

public class Company {
    private String name;
    private int id;

    public Company(String name, int id) {
        this.name = name;
        this.id = id;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return "Company{" +
                "name='" + name + "', " +
                "cui=" + id +
                "}";
    }
}
