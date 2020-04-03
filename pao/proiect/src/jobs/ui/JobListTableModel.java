package jobs.ui;

import jobs.db.JobDatabase;
import jobs.model.Job;

import javax.swing.table.AbstractTableModel;
import java.util.List;

public class JobListTableModel extends AbstractTableModel {
    private List<Job> jobPostings;

    private final static String COLUMN_NAMES[] = {
            "Category",
            "Job title",
            "Company",
            "Date posted",
    };

    public JobListTableModel(JobDatabase db) {
        this.jobPostings = db.getJobPostings();
    }

    @Override
    public int getColumnCount() {
        return COLUMN_NAMES.length;
    }

    @Override
    public String getColumnName(int column) {
        return COLUMN_NAMES[column];
    }

    @Override
    public int getRowCount() {
        return jobPostings.size();
    }

    @Override
    public Object getValueAt(int row, int column) {
        Job job = jobPostings.get(row);

        switch (COLUMN_NAMES[column]) {
            case "Job title":
                return job.getTitle();
            case "Date posted":
                return job.getDatePosted();
            case "Company":
                return job.getCompany().getName();
            case "Category":
                return job.getCategory();
            default:
                throw new RuntimeException("Unknown column requested");
        }
    }
}
