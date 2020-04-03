package jobs.ui;

import jobs.DatabaseTest;
import jobs.db.JobDatabase;
import jobs.db.impl.InMemoryJobDatabase;

import java.awt.*;
import javax.swing.*;
import javax.swing.table.AbstractTableModel;

public class MainWindow extends JFrame {
    public MainWindow(JobDatabase db) {
        super("Get a Job");

        setDefaultCloseOperation(EXIT_ON_CLOSE);

        JLabel textLabel = new JLabel("Hello world!");

        //getContentPane().add(textLabel, BorderLayout.WEST);

        JTable jobListTable = new JTable(new JobListTableModel(db));

        getContentPane().add(jobListTable, BorderLayout.CENTER);

        setSize(800, 600);

        setVisible(true);
    }

    public static void main(String[] args) {
        JobDatabase db = new InMemoryJobDatabase();

        DatabaseTest.fillDatabaseWithMockData(db);

        new MainWindow(db);
    }
}
