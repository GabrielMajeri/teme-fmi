package jobs.gui;

import jobs.db.JobDatabase;
import jobs.model.Job;

import javax.swing.table.AbstractTableModel;

public class JobPostingsTableModel extends AbstractTableModel {
    private final Job[] jobs;

    private final static String[] COLUMN_NAMES = {
            "Category",
            "Job title",
            "Company",
            "Time posted",
    };

    public JobPostingsTableModel(JobDatabase db) {
        this.jobs = db.getJobs().toArray(new Job[0]);
    }

    @Override
    public String getColumnName(int column) {
        return COLUMN_NAMES[column];
    }

    @Override
    public int getRowCount() {
        return jobs.length;
    }

    @Override
    public int getColumnCount() {
        return COLUMN_NAMES.length;
    }

    @Override
    public Object getValueAt(int row, int column) {
        Job job = jobs[row];

        switch (column) {
            case 0:
                return job.title;
            case 1:
                return job.timePosted;
            case 2:
                return job.companyId;
            case 3:
                return job.category;
            default:
                return null;
        }
    }
}
