package jobs.db.impl;

import jobs.model.Company;
import jobs.db.JobDatabase;
import jobs.model.Job;

import java.util.*;

public final class InMemoryJobDatabase implements JobDatabase {
    private final SortedMap<String, Company> companies = new TreeMap<>();
    private final List<Job> jobs = new ArrayList<>();

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
    public String toString() {
        return "InMemoryJobDatabase{" +
                "companies=" + companies.values() +
                '}';
    }
}
