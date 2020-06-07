package jobs.db.impl;

import jobs.model.*;
import jobs.db.JobDatabase;

import java.util.*;
import java.util.stream.Collectors;

public final class InMemoryDatabase implements JobDatabase {
    private final SortedMap<String, Company> companies = new TreeMap<>();
    private final AbstractMap<Integer, Job> jobs = new HashMap<>();
    private final AbstractMap<Integer, User> users = new HashMap<>();
    private final AbstractMap<Integer, List<CV>> cvsByCandidate = new HashMap<>();
    private final AbstractMap<Integer, CV> cvsById = new HashMap<>();
    private final List<Application> applications = new ArrayList<>();

    @Override
    public void addCompany(Company company) {
        if (companies.containsKey(company.name)) {
            throw new IllegalArgumentException("cannot add company to DB twice");
        }
        companies.put(company.name, company);
    }

    @Override
    public void removeCompany(Company company) {
        if (!companies.containsKey(company.name)) {
            throw new IllegalArgumentException("cannot remove non-existing company");
        }
        companies.remove(company.name);
    }

    @Override
    public Collection<Company> getCompanies() {
        return companies.values();
    }

    @Override
    public void addJob(Job job) {
        if (jobs.containsKey(job.id)) {
            throw new IllegalArgumentException("job with duplicate IDs are not allowed");
        }
        jobs.put(job.id, job);
    }

    @Override
    public Collection<Job> getJobs() {
        return jobs.values();
    }

    @Override
    public void addUser(User user) {
        if (users.containsKey(user.id)) {
            throw new IllegalArgumentException("users with duplicate IDs are not allowed");
        }
        users.put(user.id, user);
    }

    @Override
    public Collection<User> getUsers() {
        return users.values();
    }

    @Override
    public void addCV(CV cv) {
        int candidateId = cv.candidateId;
        if (!users.containsKey(candidateId)) {
            throw new IllegalArgumentException("CV's candidate not found");
        }
        if (cvsById.containsKey(cv.id)) {
            throw new IllegalArgumentException("CV with same ID already in database");
        }
        cvsById.put(cv.id, cv);
        if (!cvsByCandidate.containsKey(candidateId)) {
            cvsByCandidate.put(candidateId, new ArrayList<>());
        }
        cvsByCandidate.get(candidateId).add(cv);
    }

    @Override
    public Collection<CV> getCVs(Candidate candidate) {
        if (!cvsByCandidate.containsKey(candidate.id)) {
            return new ArrayList<>();
        }
        return cvsByCandidate.get(candidate.id);
    }

    @Override
    public void addApplication(Application application) {
        if (!jobs.containsKey(application.jobId)) {
            throw new IllegalArgumentException("application's job not found");
        }
        if (!cvsById.containsKey(application.cvId)) {
            throw new IllegalArgumentException("application's CV not found");
        }
        applications.add(application);
    }

    @Override
    public void removeApplication(Application application) {
        applications.remove(application);
    }

    @Override
    public Collection<Application> getApplications() {
        return applications;
    }

    @Override
    public String toString() {
        return "InMemoryJobDatabase{" +
                "companies=" + companies.values() + "," +
                "jobs=" + jobs + "," +
                "users=" + users +
                '}';
    }
}
