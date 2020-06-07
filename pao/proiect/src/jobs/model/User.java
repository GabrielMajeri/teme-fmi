package jobs.model;

import java.util.Objects;

public abstract class User {
    public final int id;
    public final Name name;

    protected User(int id, Name name) {
        if (id == 0) {
            throw new IllegalArgumentException("user ID cannot be 0");
        }

        this.id = id;
        this.name = Objects.requireNonNull(name);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return id == user.id &&
                name.equals(user.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "User{" +
                "id=" + id + ", " +
                "name='" + name + "'" +
                '}';
    }
}
