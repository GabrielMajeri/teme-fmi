package jobs;

import csv.CsvReader;
import csv.CsvWriter;
import jobs.db.JobDatabase;
import jobs.db.impl.InMemoryJobDatabase;
import jobs.model.Company;

import java.io.*;

public class CsvTest {
    private final static String COMPANY_CSV_PATH = "companies.csv";

    private CsvTest() {}

    public static void writeToFile(JobDatabase db) throws IOException {
        BufferedWriter companyWriter = new BufferedWriter(new FileWriter(COMPANY_CSV_PATH));
        CsvWriter<Company> companyCsv = new CsvWriter<>(companyWriter, Company.FACTORY);

        for (Company company : db.getCompanies()) {
            companyCsv.writeObject(company);
        }

        companyWriter.flush();
    }

    public static InMemoryJobDatabase readFromFile() throws IOException {
        BufferedReader companyReader = new BufferedReader(new FileReader(COMPANY_CSV_PATH));
        CsvReader<Company> companyCsv = new CsvReader<>(companyReader, Company.FACTORY);
        InMemoryJobDatabase db = new InMemoryJobDatabase();

        while (companyCsv.hasMoreObjects()) {
            db.addCompany(companyCsv.readObject());
        }

        return db;
    }
}
