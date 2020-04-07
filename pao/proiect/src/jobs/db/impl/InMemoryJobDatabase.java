package jobs.db.impl;

import jobs.model.Company;
import jobs.db.JobDatabase;
import jobs.model.Job;

import java.util.ArrayList;
import java.util.List;

public final class InMemoryJobDatabase implements JobDatabase {
    private final List<Company> companyList = new ArrayList<>();
    private final List<Job> jobList = new ArrayList<>();

    @Override
    public void addCompany(Company company) {
        companyList.add(company);
    }

    @Override
    public List<Company> getCompanies() {
        return companyList;
    }

    @Override
    public Company findCompanyByName(String name) {
        for (Company company : companyList) {
            if (company.getName().equals(name)) {
                return company;
            }
        }
        return null;
    }

    @Override
    public void addJob(Job job) {
        jobList.add(job);
    }

    @Override
    public List<Job> getJobPostings() {
        return jobList;
    }

    @Override
    public String toString() {
        return "InMemoryJobDatabase{" +
                "companyList=" + companyList +
                '}';
    }
}
