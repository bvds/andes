/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Andes Version 3 Tutoring System
 */
public class TransactionPopulator {
    private final static String fileToProcess =
            "D:\\School\\2009-2010\\Summer 2009\\bvds-andes-e7480d09503936b644b5fab109b9e4138d1301da\\web-UI\\Documentation\\AsuDocs\\nokes-example-json-complete.txt";
    private static void populateTransaction(String fileName) {
        String url =
                "jdbc:mysql://localhost:3306/andes";
        Connection con = null;
        Scanner toScan = null;
        String responsibleParty = "client";
        int i = 1;
        try {
            con = DriverManager.getConnection(url, "root", "rootwdp");
            toScan = new Scanner(new java.io.File(fileName));
            while (toScan.hasNextLine()) {
                String line = toScan.nextLine();
                if (line.startsWith("[")) {
                    responsibleParty = "server";
                    continue;
                } else if (line.startsWith("]")) {
                    responsibleParty = "client";
                    continue;
                }
                PreparedStatement stmt = con.prepareStatement(
                        "INSERT INTO `problem_attempt_transaction` VALUES(?,?,?,?);");
                stmt.setInt(1, i++);
                stmt.setInt(2, 1);
                stmt.setString(3, line);
                stmt.setString(4, responsibleParty);
                System.out.println(stmt.toString());
                stmt.executeUpdate();
            }
            con.close();
        } catch (SQLException ex) {
            System.out.print("A SQL error occurred! " + ex.getSQLState() +
                    ex.getLocalizedMessage());
        } catch (Exception e) {
            System.out.print("fail");
        } finally {
            toScan.close();
        }
    }

    public static void main(String[] args)
    {
        populateTransaction(fileToProcess);
    }
}
