package jobs.gui;

import jobs.db.JobDatabase;
import jobs.model.Company;

import javax.swing.*;
import java.awt.*;

/**
 * Panel that displays the list of companies, and allows editing it.
 */
public class CompaniesPanel extends JPanel {
    public CompaniesPanel(JobDatabase db) {
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

        JButton addButton = new JButton("Add new");
        JButton modifyButton = new JButton("Modify selected");
        modifyButton.setEnabled(false);

        // Create a container for the table header and the table
        JPanel tablePanel = new JPanel();

        // Create a table to display the companies
        CompaniesTableModel tableModel = new CompaniesTableModel(db);
        JTable companiesTable = new JTable(tableModel);
        companiesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        companiesTable.getSelectionModel().addListSelectionListener(listSelectionEvent -> {
            int selectedRow = companiesTable.getSelectedRow();
            modifyButton.setEnabled(selectedRow != -1);
        });

        // Create a new panel for the data modification buttons, with Flow Layout
        JPanel buttonsPanel = new JPanel(false);
        buttonsPanel.add(addButton);
        buttonsPanel.add(modifyButton);

        addButton.addActionListener(actionEvent -> {
            JTextField nameField = new JTextField();
            JTextField idField = new JTextField();
            Object[] message = {"Name:", nameField, "ID:", idField};
            int option = JOptionPane.showConfirmDialog(
                    this,
                    message,
                    "Add new company",
                    JOptionPane.OK_CANCEL_OPTION);
            if (option == JOptionPane.OK_OPTION) {
                int id = Integer.parseInt(idField.getText());
                String name = nameField.getText();
                tableModel.addCompany(new Company(id, name));
            }
        });
        modifyButton.addActionListener(actionEvent -> {
            String newName = JOptionPane.showInputDialog(
                    this,
                    "Name",
                    "Edit company name",
                    JOptionPane.QUESTION_MESSAGE);
            if (newName != null) {
                int selectedRow = companiesTable.getSelectedRow();
                tableModel.setCompanyName(selectedRow, newName);
            }
        });

        tablePanel.setLayout(new BorderLayout());
        tablePanel.add(companiesTable.getTableHeader(), BorderLayout.PAGE_START);
        tablePanel.add(companiesTable, BorderLayout.CENTER);
        tablePanel.setBorder(BorderFactory.createEmptyBorder(10, 30, 20, 30));

        add(buttonsPanel);
        add(tablePanel);
    }
}
