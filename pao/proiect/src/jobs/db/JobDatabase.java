package jobs.db;

import jobs.model.*;

import java.util.Collection;
import java.util.stream.Collectors;

public interface JobDatabase {
    void addCompany(Company company);

    Collection<Company> getCompanies();

    default Company getCompanyById(int id) {
        return getCompanies().stream()
                .filter(company -> company.id == id)
                .findAny().orElse(null);
    }

    default void updateCompany(int id, String name) {
        removeCompany(id);
        addCompany(new Company(id, name));
    }

    void removeCompany(int companyId);


    void addJob(Job job);

    Collection<Job> getJobs();

    default Job getJobById(int id) {
        return getJobs().stream()
                .filter(job -> job.id == id)
                .findAny().orElse(null);
    }

    void addUser(User user);

    Collection<User> getUsers();

    default User getUserById(int id) {
        return getUsers().stream()
                .filter(user -> user.id == id)
                .findAny().orElse(null);
    }

    void addCV(CV cv);

    Collection<CV> getCVs();

    default Collection<CV> getCVsByCandidate(Candidate candidate) {
        return getCVs().stream()
                .filter(cv -> cv.candidateId == candidate.id)
                .collect(Collectors.toList());
    }

    default CV getCVById(int id) {
        return getCVs().stream()
                .filter(cv -> cv.id == id)
                .findAny().orElse(null);
    }

    void addApplication(Application application);

    void removeApplication(Application application);

    Collection<Application> getApplications();
}
