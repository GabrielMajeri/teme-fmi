package jobs.utils;

import jobs.db.JobDatabase;
import jobs.model.*;

import java.time.Instant;
import java.util.*;

public final class MockUtil {
    private final static String[] COMPANY_NAMES = {
            "Bitdefender",
            "Avira",
            "Mega Image",
            "Microsoft",
            "Endava",
            "Google",
            "Facebook",
    };

    private final static String[] JOB_TITLE_PREFIXES = {
            "",
            "Junior ",
            "Senior ",
            "Remote ",
            "Assistant ",
            "Manager ",
    };

    private final static String[] JOB_TITLES = {
            "Aquisitions Officer",
            "Bussiness Developer",
            "Database Administrator",
            "Designer",
            "Software Engineer",
            "Operations Staff",
            "Store Clerk",
            "Spokesperson",
    };

    private final static Category[] JOB_CATEGORIES = Category.values();

    private final static String[] FIRST_NAMES = {
            "Airy",
            "Benjamin",
            "John",
            "Jane",
            "Mary",
            "Beth",
            "Marcus",
            "William",
    };

    private final static String[] INITIALS = {
            "",
            "D",
            "G",
            "T",
    };

    private final static String[] LAST_NAMES = {
            "Gatsby",
            "Smith",
            "Miller",
            "Barnaby",
            "Wall",
            "Nobles",
    };

    private final static int NUM_COMPANIES = COMPANY_NAMES.length;
    private final static int NUM_JOBS = 15;
    private final static int NUM_RECRUITERS = 10;
    private final static int NUM_CANDIDATES = 30;
    private final static int NUM_APPLICATIONS = 20;

    private MockUtil() {
        throw new AssertionError();
    }

    public static void fillDatabaseWithMockData(JobDatabase db, long seed) {
        Random rng = new Random(seed);

        List<Company> companies = new ArrayList<>();

        for (int i = 0; i < NUM_COMPANIES; ++i) {
            String name = COMPANY_NAMES[i];

            int id = generateCompanyId(rng);

            Company company = new Company(name, id);

            companies.add(company);
            db.addCompany(company);
        }

        List<Job> jobs = new ArrayList<>();

        for (int i = 0; i < NUM_JOBS; ++i) {
            String prefix = JOB_TITLE_PREFIXES[rng.nextInt(JOB_TITLE_PREFIXES.length)];
            String title = JOB_TITLES[rng.nextInt(JOB_TITLES.length)];
            title = prefix + title;

            Category category = JOB_CATEGORIES[rng.nextInt(JOB_CATEGORIES.length)];

            Company company = companies.get(rng.nextInt(NUM_COMPANIES));

            Job job = new Job(title, category, company);

            jobs.add(job);
            db.addJob(job);
        }

        for (int i = 0; i < NUM_RECRUITERS; ++i) {
            Name name = generateUserName(rng);
            Company company = companies.get(rng.nextInt(NUM_COMPANIES));

            User recruiter = new Recruiter(name, company);
            db.addUser(recruiter);
        }

        List<CV> cvs = new ArrayList<>();

        for (int i = 0; i < NUM_CANDIDATES; ++i) {
            Name name = generateUserName(rng);

            Candidate candidate = new Candidate(name);
            db.addUser(candidate);

            for (int j = 1; j < 1 + rng.nextInt(3); ++j) {
                CV cv = new CV(candidate, "This is my CV");
                cvs.add(cv);
                db.addCV(cv);
            }
        }

        Set<Application> applications = new HashSet<>();
        while (applications.size() < NUM_APPLICATIONS) {
            Job job = jobs.get(rng.nextInt(NUM_JOBS));
            CV cv = cvs.get(rng.nextInt(cvs.size()));

            Application application = new Application(job, cv);
            if (applications.add(application)) {
                db.addApplication(application);
            }
        }
    }

    private static int generateCompanyId(Random rng) {
        return 1_000_000 + rng.nextInt(500_000);
    }
    private static Name generateUserName(Random rng) {
        String firstName = FIRST_NAMES[rng.nextInt(FIRST_NAMES.length)];
        String initialOfFather = INITIALS[rng.nextInt(INITIALS.length)];
        String lastName = LAST_NAMES[rng.nextInt(LAST_NAMES.length)];
        return new Name(firstName, initialOfFather, lastName);
    }
}
