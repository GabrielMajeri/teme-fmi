package jobs.db.impl;

import csv.CsvReader;
import csv.CsvTypeFactory;
import csv.CsvWriter;
import jobs.db.JobDatabase;
import jobs.model.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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

    private static <T> void update(String path, CsvTypeFactory<T> factory, Consumer<Collection<T>> callback) {
        Collection<T> objects = read(path, factory);
        callback.accept(objects);
        write(path, factory, objects);
    }

    public CsvDatabase() {}

    private final static String COMPANIES_FILE = "companies.csv";
    private final static String JOBS_FILE = "jobs.csv";
    private final static String CANDIDATES_FILE = "candidates.csv";
    private final static String RECRUITERS_FILE = "recruiters.csv";
    private final static String CVS_FILE = "cvs.csv";
    private final static String APPLICATIONS_FILE = "applications.csv";

    @Override
    public void addCompany(Company company) {
        update(COMPANIES_FILE, Company.FACTORY, companies -> companies.add(company));
    }

    @Override
    public void removeCompany(Company company) {
        update(COMPANIES_FILE, Company.FACTORY, companies -> companies.remove(company));
    }

    @Override
    public Collection<Company> getCompanies() {
        return read(COMPANIES_FILE, Company.FACTORY);
    }

    @Override
    public void addJob(Job job) {
        update(JOBS_FILE, Job.FACTORY, jobs -> jobs.add(job));
    }

    @Override
    public Collection<Job> getJobs() {
        return read(JOBS_FILE, Job.FACTORY);
    }

    @Override
    public void addUser(User user) {
        if (user instanceof Candidate) {
            Candidate candidate = (Candidate)user;
            update(CANDIDATES_FILE, Candidate.FACTORY, candidates -> candidates.add(candidate));
        } else if (user instanceof Recruiter) {
            Recruiter recruiter = (Recruiter)user;
            update(RECRUITERS_FILE, Recruiter.FACTORY, recruiters -> recruiters.add(recruiter));
        } else {
            throw new UnsupportedOperationException("unknown user type");
        }
    }

    @Override
    public Collection<User> getUsers() {
        Collection<Candidate> candidates = read(CANDIDATES_FILE, Candidate.FACTORY);
        Collection<Recruiter> recruiters = read(RECRUITERS_FILE, Recruiter.FACTORY);
        return Stream.concat(candidates.stream(), recruiters.stream()).collect(Collectors.toList());
    }

    @Override
    public void addCV(CV cv) {
        update(CVS_FILE, CV.FACTORY, cvs -> cvs.add(cv));
    }

    @Override
    public Collection<CV> getCVs(Candidate candidate) {
        return read(CVS_FILE, CV.FACTORY).stream()
                .filter(cv -> cv.candidateId == candidate.id)
                .collect(Collectors.toList());
    }

    @Override
    public void addApplication(Application application) {
        update(APPLICATIONS_FILE, Application.FACTORY, applications -> applications.add(application));
    }

    @Override
    public void removeApplication(Application application) {
        update(APPLICATIONS_FILE, Application.FACTORY, applications -> applications.remove(application));
    }

    @Override
    public Collection<Application> getApplications() {
        return read(APPLICATIONS_FILE, Application.FACTORY);
    }
}
