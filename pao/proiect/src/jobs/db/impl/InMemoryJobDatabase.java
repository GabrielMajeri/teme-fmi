package jobs.db.impl;

import jobs.model.*;
import jobs.db.JobDatabase;

import java.util.*;

public final class InMemoryJobDatabase implements JobDatabase {
    private final SortedMap<String, Company> companies = new TreeMap<>();
    private final List<Job> jobs = new ArrayList<>();
    private final AbstractMap<Integer, User> users = new HashMap<>();
    private final AbstractMap<Integer, CV> cvs = new HashMap<>();
    private final List<Application> applications = new ArrayList<>();

    @Override
    public void addCompany(Company company) {
        String companyName = company.getName();
        if (companies.containsKey(companyName)) {
            throw new IllegalArgumentException("Cannot add company to DB twice");
        }
        companies.put(companyName, company);
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
    public void addCV(CV cv) {
        int candidateId = cv.getCandidateUserId();
        if (!users.containsKey(candidateId)) {
            throw new IllegalArgumentException("CV's candidate not found");
        }
        cvs.put(candidateId, cv);
    }

    @Override
    public void addApplication(Application application) {
        if (!jobs.contains(application.job)) {
            throw new IllegalArgumentException("application's job not found");
        }
        if (!cvs.containsValue(application.cv)) {
            throw new IllegalArgumentException("application's CV not found");
        }
        applications.add(application);
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
