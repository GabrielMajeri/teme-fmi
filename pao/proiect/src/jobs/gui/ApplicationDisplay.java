package jobs.gui;

import jobs.db.JobDatabase;
import jobs.model.Application;
import jobs.model.CV;
import jobs.model.Job;
import jobs.model.User;

import javax.swing.*;

/**
 * Panel displaying info about an application.
 */
public class ApplicationDisplay extends JPanel {
    private final JobDatabase db;
    private final JLabel jobTitle = new JLabel();
    private final JLabel candidateName = new JLabel();
    private final JLabel cvDescription = new JLabel();

    public ApplicationDisplay(JobDatabase db) {
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

        this.db = db;
        add(jobTitle);
        add(candidateName);
        add(cvDescription);
    }

    public void setApplication(Application application) {
        if (application == null) {
            setVisible(false);
            return;
        }

        setVisible(true);

        Job job = db.getJobById(application.jobId);
        CV cv = db.getCVById(application.cvId);
        User user = db.getUserById(cv.candidateId);

        jobTitle.setText("Job: " + job.title);
        candidateName.setText("Applicant: " + user.name.toString());
        cvDescription.setText("CV: " + cv.description);
    }
}
