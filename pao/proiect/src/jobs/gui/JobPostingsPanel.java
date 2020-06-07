package jobs.gui;

import jobs.db.JobDatabase;

import javax.swing.*;
import java.awt.*;

/**
 * Panel that displays all the available job postings in a table.
 */
public class JobPostingsPanel extends JPanel {
    public JobPostingsPanel(JobDatabase db) {
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

        JTable table = new JTable(new JobPostingsTableModel(db));

        JPanel tablePanel = new JPanel(false);
        tablePanel.setLayout(new BorderLayout());
        tablePanel.add(table.getTableHeader(), BorderLayout.PAGE_START);
        tablePanel.add(table, BorderLayout.CENTER);
        tablePanel.setBorder(BorderFactory.createEmptyBorder(10, 30, 20, 30));

        add(tablePanel);
    }
}
