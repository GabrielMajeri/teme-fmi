package jobs.gui;

import jobs.db.JobDatabase;
import jobs.model.Company;

import javax.swing.table.AbstractTableModel;

/**
 * Table model for companies list.
 */
public class CompaniesTableModel extends AbstractTableModel {
    private final static String[] COLUMN_NAMES = {"Name", "ID"};
    private final JobDatabase db;
    private Company[] companies;

    public CompaniesTableModel(JobDatabase db) {
        this.db = db;
        refreshCompaniesList();
    }

    @Override
    public String getColumnName(int column) {
        return COLUMN_NAMES[column];
    }

    @Override
    public int getRowCount() {
        return companies.length;
    }

    @Override
    public int getColumnCount() {
        return COLUMN_NAMES.length;
    }

    @Override
    public Object getValueAt(int row, int column) {
        Company company = companies[row];

        switch (column) {
            case 0:
                return company.name;
            case 1:
                return company.id;
            default:
                return null;
        }
    }

    public void addCompany(Company company) {
        db.addCompany(company);
        refreshCompaniesList();
        fireTableDataChanged();
    }

    public void setCompanyName(int row, String name) {
        Company company = companies[row];
        db.updateCompany(company.id, name);
        refreshCompaniesList();
        fireTableCellUpdated(row, 0);
    }

    private void refreshCompaniesList() {
        companies = db.getCompanies().toArray(new Company[0]);
    }
}
