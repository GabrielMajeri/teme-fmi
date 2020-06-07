package jobs;

import jobs.db.JobDatabase;
import jobs.model.*;

import java.time.Instant;
import java.util.Collection;

import static jobs.utils.AssertUtil.*;

public class DatabaseTest {
    private final JobDatabase db;

    private final Company company1 = new Company(1, "Test Corp.");
    private final Company company2 = new Company(100, "Test Too");

    private final Recruiter recruiter1 = new Recruiter(-100, new Name("First", "Recruiter"), 100);
    private final Recruiter recruiter2 = new Recruiter(-102, new Name("Second", "Recruiter"), 100);
    private final Recruiter recruiter3 = new Recruiter(-101, new Name("Third", "J", "R"), 1);

    private final Candidate candidate1 = new Candidate(105, new Name("Foo", "F", "Bar"));
    private final Candidate candidate2 = new Candidate(1001, new Name("你", "好"));
    private final Candidate candidate3 = new Candidate(101, new Name("X Æ A-XII", "Musk"));

    private final CV cv1 = new CV(candidate1, "Cool CV");
    private final CV cv2 = new CV(candidate3, "I'm good");
    private final CV cv3 = new CV(candidate1, "Another cool CV");

    private final Job job1 = new Job("Foo", Category.Design, company1);
    private final Job job2 = new Job("Senior Baz", Category.Business, company1);

    private final Application application1 = new Application(job1, cv3);
    private final Application application2 = new Application(job1, cv2);
    private final Application application3 = new Application(job2, cv3);

    public DatabaseTest(JobDatabase db) {
        this.db = db;
    }

    public void runAllTests() {
        testCompanies();
        testRecruiters();
        testCandidates();
        testCVs();
        testJobs();
        testApplications();
    }

    private void testCompanies() {
        db.addCompany(company2);
        db.addCompany(company1);

        Collection<Company> companies = db.getCompanies();

        assertContains(companies, company1);
        assertContains(companies, company2);
    }

    private void testRecruiters() {
        db.addUser(recruiter2);
        db.addUser(recruiter1);
        db.addUser(recruiter3);

        Collection<User> users = db.getUsers();

        assertContains(users, recruiter1);
        assertContains(users, recruiter2);
        assertContains(users, recruiter3);
    }

    private void testCandidates() {
        db.addUser(candidate3);
        db.addUser(candidate2);
        db.addUser(candidate1);

        Collection<User> users = db.getUsers();

        assertContains(users, candidate1);
        assertContains(users, candidate3);
        assertContains(users, candidate2);
    }

    private void testCVs() {
        db.addCV(cv1);
        db.addCV(cv2);
        db.addCV(cv3);

        Collection<CV> cvs1 = db.getCVsByCandidate(candidate1);
        assertContains(cvs1, cv1);
        assertContains(cvs1, cv3);

        Collection<CV> cvs2 = db.getCVsByCandidate(candidate2);
        assertEmpty(cvs2);

        Collection<CV> cvs3 = db.getCVsByCandidate(candidate3);
        assertContains(cvs3, cv2);
    }

    private void testJobs() {
        db.addJob(job1);
        db.addJob(job2);

        Collection<Job> jobs = db.getJobs();

        assertContains(jobs, job1);
        assertContains(jobs, job2);

        assertThat(job1.timePosted.compareTo(Instant.now()) < 0, "Instant should be monotonic");
    }

    private void testApplications() {
        db.addApplication(application1);
        db.addApplication(application2);
        db.addApplication(application3);

        Collection<Application> applications = db.getApplications();

        assertContains(applications, application1);
        assertContains(applications, application2);
        assertContains(applications, application3);

        db.removeApplication(application2);
        assertNotContains(db.getApplications(), application2);
    }
}
