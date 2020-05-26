package jobs.gui;

import jobs.MockUtil;
import jobs.db.DatabaseAudit;
import jobs.db.JobDatabase;
import jobs.db.impl.InMemoryJobDatabase;

import java.awt.*;
import java.io.IOException;
import javax.swing.*;

public class MainWindow extends JFrame {
    public MainWindow(JobDatabase db) {
        super("Job Platform Manager");

        setDefaultCloseOperation(EXIT_ON_CLOSE);

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("Companies", new CompaniesPanel(db));
        tabbedPane.addTab("Job Postings", new JobPostingsPanel(db));

        setContentPane(tabbedPane);

        setMinimumSize(new Dimension(400, 300));
        setSize(new Dimension(800, 600));

        setVisible(true);
    }

    public static void main(String[] args) {
        JobDatabase db = new InMemoryJobDatabase();

        try {
            db = new DatabaseAudit(db, "audit.txt");
        } catch (IOException e) {
            System.err.println("Unable to open log file for audit");
            e.printStackTrace();
            System.err.println("No logging will be performed");
        }

        MockUtil.fillDatabaseWithMockData(db, 7);

        // Use the Metal look and feel
        try {
            UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) {
            System.err.println("Unable to use Metal cross-platform L&F, falling back to default");
            e.printStackTrace();
        }

        new MainWindow(db);
    }
}
