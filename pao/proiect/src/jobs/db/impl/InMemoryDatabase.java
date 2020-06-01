package jobs.db.impl;

import jobs.model.*;
import jobs.db.JobDatabase;

import java.util.*;
import java.util.stream.Collectors;

public final class InMemoryDatabase implements JobDatabase {
    private final SortedMap<String, Company> companies = new TreeMap<>();
    private final List<Job> jobs = new ArrayList<>();
    private final AbstractMap<Integer, User> users = new HashMap<>();
    private final AbstractMap<Integer, List<CV>> cvs = new HashMap<>();
    private final List<Application> applications = new ArrayList<>();

    @Override
    public void addCompany(Company company) {
        String companyName = company.getName();
        if (companies.containsKey(companyName)) {
            throw new IllegalArgumentException("cannot add company to DB twice");
        }
        companies.put(companyName, company);
    }

    @Override
    public void removeCompany(Company company) {
        String name = company.getName();
        if (!companies.containsKey(name)) {
            throw new IllegalArgumentException("cannot remove non-existing company");
        }
        companies.remove(name);
    }

    @Override
    public Collection<Company> getCompanies() {
        return companies.values();
    }

    @Override
    public Company findCompanyByName(String name) {
        return companies.get(name);
    }

    @Override
    public void addJob(Job job) {
        jobs.add(job);
    }

    @Override
    public Collection<Job> getJobs() {
        return jobs;
    }

    @Override
    public void addUser(User user) {
        int userId = user.getId();
        if (users.containsKey(userId)) {
            throw new IllegalArgumentException("users with duplicate IDs are not allowed");
        }
        users.put(userId, user);
    }

    @Override
    public Collection<User> getUsers() {
        return users.values();
    }

    @Override
    public void addCV(CV cv) {
        int candidateId = cv.getCandidateUserId();
        if (!users.containsKey(candidateId)) {
            throw new IllegalArgumentException("CV's candidate not found");
        }
        if (!cvs.containsKey(candidateId)) {
            cvs.put(candidateId, new ArrayList<>());
        }
        cvs.get(candidateId).add(cv);
    }

    @Override
    public Collection<CV> getCVs(Candidate candidate) {
        return cvs.get(candidate.getId());
    }

    @Override
    public void addApplication(Application application) {
        if (!jobs.contains(application.job)) {
            throw new IllegalArgumentException("application's job not found");
        }
        int candidateId = application.cv.getCandidateUserId();
        if (!cvs.get(candidateId).contains(application.cv)) {
            throw new IllegalArgumentException("applicant's CV not found");
        }
        applications.add(application);
    }

    @Override
    public void removeApplication(Application application) {
        applications.remove(application);
    }

    @Override
    public Collection<Application> getApplications(Job job) {
        return applications.stream()
                .filter(application -> application.job.equals(job))
                .collect(Collectors.toUnmodifiableList());
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
