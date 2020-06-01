package jobs.gui;

import jobs.db.JobDatabase;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.*;

public class CompaniesPanel extends JPanel {
    private final JButton modifyButton, deleteButton;

    public CompaniesPanel(JobDatabase db) {
        setLayout(new BoxLayout(this, BoxLayout.PAGE_AXIS));

        // Create a new panel for the data modification buttons, with Flow Layout
        JPanel buttonsPanel = new JPanel(false);
        buttonsPanel.add(new JButton("Add new"));
        buttonsPanel.add(modifyButton = new JButton("Modify selected"));
        buttonsPanel.add(deleteButton = new JButton("Delete selected"));

        modifyButton.setEnabled(false);
        deleteButton.setEnabled(false);

        // Create a container for the table header and the table
        JPanel tablePanel = new JPanel();

        // Create a table to display the companies
        JTable companiesTable = new JTable(new CompaniesTableModel(db));
        companiesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        companiesTable.getColumn(CompaniesTableModel.NAME_COLUMN).setMinWidth(200);
        companiesTable.getColumn(CompaniesTableModel.ID_COLUMN).setMinWidth(80);

        companiesTable.getSelectionModel().addListSelectionListener(listSelectionEvent -> {
            int selectedRow = companiesTable.getSelectedRow();
            if (selectedRow != -1) {
                modifyButton.setEnabled(true);
                deleteButton.setEnabled(true);
            } else {
                modifyButton.setEnabled(false);
                deleteButton.setEnabled(false);
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
