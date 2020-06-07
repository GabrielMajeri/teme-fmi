package jobs.gui;

import jobs.db.JobDatabase;
import jobs.model.Application;
import jobs.model.Company;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class ApplicationsPanel extends JPanel {
    private final JobDatabase db;
    private final List<Company> companies;
    private final List<Application> applications;
    private final JButton previousButton, nextButton, deleteButton;
    private final ApplicationDisplay applicationDisplay;
    private int currentCompany;
    private int currentApplication;

    public ApplicationsPanel(JobDatabase db) {
        this.db = db;

        companies = new ArrayList<>(db.getCompanies());
        applications = new ArrayList<>();

        String[] companyNames = companies.stream()
                .map(company -> company.name)
                .toArray(String[]::new);

        JPanel filterPanel = new JPanel(false);
        JLabel filterLabel = new JLabel("Company:");
        JComboBox<String> filterComboBox = new JComboBox<>(companyNames);
        filterComboBox.addActionListener(actionEvent -> {
            currentCompany = filterComboBox.getSelectedIndex();
            refreshApplicationsList();
        });
        filterPanel.add(filterLabel);
        filterPanel.add(filterComboBox);

        JPanel controlPanel = new JPanel(false);
        previousButton = new JButton("Previous");
        previousButton.addActionListener(actionEvent -> previousApplication());
        nextButton = new JButton("Next");
        nextButton.addActionListener(actionEvent -> nextApplication());
        deleteButton = new JButton("Delete");
        deleteButton.addActionListener(actionEvent -> deleteApplication());
        controlPanel.add(previousButton);
        controlPanel.add(nextButton);
        controlPanel.add(deleteButton);

        applicationDisplay = new ApplicationDisplay(db);

        currentCompany = 0;
        refreshApplicationsList();

        setLayout(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        c.gridy = 0;
        add(filterPanel, c);
        c.gridy = 1;
        add(controlPanel, c);
        c.gridy = 2;
        add(applicationDisplay, c);
    }

    private void refreshApplicationsList() {
        Company company = companies.get(currentCompany);
        applications.clear();
        db.getApplications().stream()
                .filter(application -> db.getJobById(application.jobId).companyId == company.id)
                .forEachOrdered(applications::add);

        boolean hasApplications = !applications.isEmpty();
        currentApplication = hasApplications ? 0 : -1;
        previousButton.setEnabled(hasApplications);
        nextButton.setEnabled(hasApplications);
        deleteButton.setEnabled(hasApplications);
        updateApplicationDisplay();
    }

    private void previousApplication() {
        currentApplication = (currentApplication + applications.size() - 1) % applications.size();
        updateApplicationDisplay();
    }

    private void nextApplication() {
        currentApplication = (currentApplication + 1) % applications.size();
        updateApplicationDisplay();
    }

    private void deleteApplication() {
        db.removeApplication(applications.get(currentApplication));
        updateApplicationDisplay();
        refreshApplicationsList();
    }

    private void updateApplicationDisplay() {
        if (currentApplication >= 0) {
            applicationDisplay.setApplication(applications.get(currentApplication));
        } else {
            applicationDisplay.setApplication(null);
        }
    }
}
