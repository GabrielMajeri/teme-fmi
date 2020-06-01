package jobs.gui;

import jobs.db.JobDatabase;

import javax.swing.*;

public class JobPostingsPanel extends JPanel {
    public JobPostingsPanel(JobDatabase db) {
        add(new JTable(new JobPostingsTableModel(db)));
    }
}
