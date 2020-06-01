package jobs.gui;

import jobs.db.JobDatabase;
import jobs.model.Company;

import javax.swing.table.AbstractTableModel;

public class CompaniesTableModel extends AbstractTableModel {
    private final Company[] companies;

    public final static String NAME_COLUMN = "Name";
    public final static String ID_COLUMN = "Unique ID";

    private final static String[] COLUMN_NAMES = { NAME_COLUMN, ID_COLUMN };

    public CompaniesTableModel(JobDatabase db) {
        this.companies = db.getCompanies().toArray(Company[]::new);
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
                return company.getName();
            case 1:
                return company.getId();
            default:
                return null;
        }
    }
}
