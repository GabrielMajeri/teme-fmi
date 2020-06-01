package jobs.db;

import jobs.model.*;

import java.util.Collection;

public interface JobDatabase {
    void addCompany(Company company);
    Collection<Company> getCompanies();
    Company findCompanyByName(String name);

    void addJob(Job job);
    Collection<Job> getJobs();

    void addUser(User user);
    void addCV(CV cv);

    void addApplication(Application application);
}
