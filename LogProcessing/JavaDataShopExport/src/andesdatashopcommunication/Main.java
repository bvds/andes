/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package andesdatashopcommunication;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.*;
import edu.cmu.pslc.logging.*;
import edu.cmu.pslc.logging.element.DatasetElement;
import edu.cmu.pslc.logging.element.LevelElement;
import edu.cmu.pslc.logging.element.MetaElement;
import edu.cmu.pslc.logging.element.ProblemElement;
import java.text.DateFormat;
import javax.persistence.*;
import org.json.JSONException;

/**
 *
 * @author Andes Version 3 Tutoring System
 */
public class Main {

    private final static String PERSISTENCE_UNIT_NAME = "AndesDataShopCommunicationPU";

    /**
     * @param args the command line arguments -- unused
     */
    public static void main(String[] args) {
        try {
            EntityManager em =
                    Persistence.createEntityManagerFactory(PERSISTENCE_UNIT_NAME).createEntityManager();
            em.getTransaction().begin();
            Vector<ProblemAttempt> toIterate =
                    new Vector<ProblemAttempt>(em.createQuery("FROM ProblemAttempt").getResultList());
            for (ProblemAttempt e : toIterate) {
                makeDataShopLog(e);
            }
            em.getTransaction().commit();
            em.close();
        } catch (JSONException ex) {
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        } 
    }

    private static void makeDataShopLog(ProblemAttempt e) throws JSONException {
        String fileName = "Andes3_DataShopExport_" + e.getUserName() + "_";
        fileName += DateFormat.getDateInstance().format(new Date(System.currentTimeMillis())) + ".xml";
        FileLogger dataShopExport = FileLogger.create(fileName);
        MetaElement meta = new MetaElement(e.getUserName(), true, e.getSessionID(),
                e.getStartTime().toString(), "Unknown");
        //create the context message element. There is only one per file, per manual
        ContextMessage myMessage = ContextMessage.createStartProblem(meta);
        myMessage.setClassName(e.getClassinformationID().getName());
        myMessage.setSchool(e.getClassinformationID().getSchool());
        myMessage.setPeriod(e.getClassinformationID().getPeriod());
        //no idea what to do about the DFA and Skill -- Don't worry about them
        myMessage.setClassDescription(e.getClassinformationID().getDescription());
        myMessage.addInstructor(e.getClassinformationID().getInstructorName());
        ProblemElement newProblem = new ProblemElement(e.getClassinformationID().getDatasetID().getProblemname());
        LevelElement groupLevel = new LevelElement(
                "group", e.getClassinformationID().getDatasetID().getGroupname(),
                newProblem);
        LevelElement moduleLevel = new LevelElement("module",
                e.getClassinformationID().getDatasetID().getModulename(),
                groupLevel);
        myMessage.setDataset(new DatasetElement(e.getClassinformationID().getDatasetID().getDatasetname(),
                moduleLevel));
        dataShopExport.log(myMessage);
        //now we need to go through each transaction related to this problem and
        // log it
        ClientServerInteractions backAndForth = new ClientServerInteractions(e);
        for (ClientServerInteraction anInteraction : 
            backAndForth.getInteractions().subList(1,
                backAndForth.getInteractions().size()-1)) {
            /* skip the calls to open and close the problem, which don't need 
             * to be logged */
            System.out.println(anInteraction);
            
            ToolMessage toolMsg = ToolMessage.create(myMessage);
            toolMsg.setProblemName(newProblem.getName());
            toolMsg.addSai(anInteraction.getClientSelection(),
                    anInteraction.getClientAction().getCommand().toString(),
                    anInteraction.getClientInput());
            switch(anInteraction.getClientAction().getCommand())
            {
                case SOLUTION_STEP:
                    toolMsg.setAsAttempt();
                    break;
                case SEEK_HELP:
                    toolMsg.setAsHintRequest();
                    break;
                default:
                    break;
            }
            dataShopExport.log(toolMsg);
            TutorMessage tutorMsg = TutorMessage.create(toolMsg);
            tutorMsg.addSai(anInteraction.getServerSelection(),
                    anInteraction.getServerInput(), anInteraction.getServerInput());
            if(anInteraction.isServerHelp())
            {
                tutorMsg.setAsHintResponse();
                tutorMsg.addTutorAdvice(anInteraction.getServerInput());
            }
            else if(anInteraction.wasCorrectClientAction())
                tutorMsg.setAsCorrectAttemptResponse();
            else
                tutorMsg.setAsIncorrectAttemptResponse();
            if(anInteraction.hasSetScore())
                tutorMsg.addCustomField("score",anInteraction.getScore());
            dataShopExport.log(tutorMsg);
            }
            dataShopExport.close();
    }
}
