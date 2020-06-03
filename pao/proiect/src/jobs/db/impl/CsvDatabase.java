package jobs.db.impl;

import csv.CsvReader;
import csv.CsvTypeFactory;
import csv.CsvWriter;
import jobs.db.JobDatabase;
import jobs.model.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;

public final class CsvDatabase implements JobDatabase {
    private static <T> Collection<T> read(String path, CsvTypeFactory<T> factory) {
        try (FileReader fileReader = new FileReader(path);
             BufferedReader bufferedReader = new BufferedReader(fileReader)) {
            CsvReader<T> csvReader = new CsvReader<>(bufferedReader, factory);
            return csvReader.readAll();
        } catch (FileNotFoundException exception) {
            // Assume no items were added yet
            return new ArrayList<>();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static <T> void write(String path, CsvTypeFactory<T> factory, Collection<T> objects) {
        try (FileWriter fileWriter = new FileWriter(path);
             BufferedWriter bufferedWriter = new BufferedWriter(fileWriter)) {
            CsvWriter<T> csvWriter = new CsvWriter<>(bufferedWriter, factory);
            csvWriter.writeAll(objects);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public CsvDatabase() {}

    private final static String COMPANIES_FILE = "companies.csv";

    @Override
    public void addCompany(Company company) {
        Collection<Company> companies = read(COMPANIES_FILE, Company.FACTORY);
        companies.add(company);
        write(COMPANIES_FILE, Company.FACTORY, companies);
    }

    @Override
    public void removeCompany(Company company) {
        Collection<Company> companies = read(COMPANIES_FILE, Company.FACTORY);
        companies.remove(company);
        write(COMPANIES_FILE, Company.FACTORY, companies);
    }

    @Override
    public Collection<Company> getCompanies() {
        return read(COMPANIES_FILE, Company.FACTORY);
    }

    @Override
    public void addJob(Job job) {
    }

    @Override
    public Collection<Job> getJobs() {
        return null;
    }

    @Override
    public void addUser(User user) {
    }

    @Override
    public Collection<User> getUsers() {
        return null;
    }

    @Override
    public void addCV(CV cv) {}

    @Override
    public Collection<CV> getCVs(Candidate user) {
        return null;
    }

    @Override
    public void addApplication(Application application) {}

    @Override
    public void removeApplication(Application application) {}

    @Override
    public Collection<Application> getApplications(Job job) {
        return null;
    }
}
