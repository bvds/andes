/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author master
 */
public class DatashopExporterTest {

    public DatashopExporterTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    /**
     * Test of getInstance method, of class DatashopExporter.
     */
    @Test
    public void testGetInstance() {
        System.out.println("getInstance");
        String[] args = {""};
        DatashopExporter expResult = null;
        DatashopExporter result = DatashopExporter.getInstance(args);
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        fail("The test case is a prototype.");
    }

    /**
     * Test of main method, of class DatashopExporter.
     */
    /*@Test
    public void testMain() {
        System.out.println("main");
        String[] args = {""};
        DatashopExporter.main(args);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }*/

    @Test
    public void testGoodInstructorQuery(){
        System.out.println("good instructor");
        String[] args = {"-i Brian Brown"};
        DatashopExporter.main(args);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");

    }

    @Test
    public void testEvilInstructorQuery(){
        System.out.println("main");
        String[] args = {"-i \"OR 1=1; SELECT * FROM PROBLEM_ATTEMPT;\""};
        DatashopExporter.main(args);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @Test
    public void testNoInstructorQuery(){
        System.out.println("No instructor");
        String[] args = {"-i Boob Mcree"};
        DatashopExporter.main(args);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");


    }
}