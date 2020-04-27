package jobs.ui;

import jobs.db.JobDatabase;
import jobs.model.JobPosting;

import javax.swing.table.AbstractTableModel;
import java.util.List;

public class JobListTableModel extends AbstractTableModel {
    private List<JobPosting> jobPostingList;

    private final static String COLUMN_NAMES[] = {
            "Category",
            "Job title",
            "Company",
            "Date posted",
    };

    public JobListTableModel(JobDatabase db) {
        this.jobPostingList = db.getJobPostings();
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
        return jobPostingList.size();
    }

    @Override
    public Object getValueAt(int row, int column) {
        JobPosting jobPosting = jobPostingList.get(row);

        switch (COLUMN_NAMES[column]) {
            case "Job title":
                return jobPosting.getTitle();
            case "Date posted":
                return jobPosting.getDatePosted();
            case "Company":
                return jobPosting.getCompany().getName();
            case "Category":
                return jobPosting.getCategory();
            default:
                throw new RuntimeException("Unknown column requested");
        }
    }
}
