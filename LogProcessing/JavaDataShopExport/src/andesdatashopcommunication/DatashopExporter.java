/*
 * DatashopExporter: A class that handles pulling data from the Andes database
 * and creating the appropriate XML file for Datashop export based upon manual
 */
package andesdatashopcommunication;

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
 * should this implement the singleton pattern? It is suggested for logging
 * @author Andes 3 Tutoring System
 */
public final class DatashopExporter {

    private static DatashopExporter singleOne = null;
    private static CommandOptions possibilities;

    private static void logProblemAttemptTransaction
            (final ProblemAttempt e,
            ContextMessage myMessage,
            ProblemElement newProblem,
            FileLogger dataShopExport) {
        //now we need to go through each transaction related to this problem and
        // log it
        try {
            ClientServerInteractions backAndForth = new ClientServerInteractions(e);
            if(backAndForth.getInteractions().size() == 0)
                return;
            for (ClientServerInteraction anInteraction : backAndForth.getInteractions().subList(1, backAndForth.getInteractions().size() - 1)) {
                /* skip the calls to open and close the problem, which don't need
                 * to be logged */
                System.out.println(anInteraction);
                ToolMessage toolMsg = ToolMessage.create(myMessage);
                toolMsg.setProblemName(newProblem.getName());
                toolMsg.addSai(anInteraction.getClientSelection(), anInteraction.getClientAction().getCommand().toString(), anInteraction.getClientInput());
                switch (anInteraction.getClientAction().getCommand()) {
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
                tutorMsg.addSai(anInteraction.getServerSelection(), anInteraction.getServerInput(), anInteraction.getServerInput());
                if (anInteraction.isServerHelp()) {
                    tutorMsg.setAsHintResponse();
                    tutorMsg.addTutorAdvice(anInteraction.getServerInput());
                } else if (anInteraction.wasCorrectClientAction()) {
                    tutorMsg.setAsCorrectAttemptResponse();
                } else {
                    tutorMsg.setAsIncorrectAttemptResponse();
                }
                if (anInteraction.hasSetScore()) {
                    tutorMsg.addCustomField("score", anInteraction.getScore());
                }
                dataShopExport.log(tutorMsg);
            }
        } catch (JSONException ex) {
            System.out.printf("%nA JSON Exception occurred for ProblemAttempt%n%s" + "The exception was %s%n", e.getUserProblem(), ex.getLocalizedMessage());
        } finally {
            dataShopExport.close();
        }
    }

    private enum CommandOptions {

        INSTRUCTOR("-i"), DATE("-d");
        public final String option;

        CommandOptions(String value) {
            this.option = value;
        }
    };
    private final static String PERSISTENCE_UNIT_NAME =
            "AndesDataShopCommunicationPU";

    public static DatashopExporter getInstance(String[] args) {
        if (singleOne == null) {
            singleOne = new DatashopExporter(args);
        }
        return singleOne;
    }

    /**
     * @param args the command line arguments -- used to decide what types of
     * exports to do. Expected values are -I instructorName | -d date | nothing
     */
    private DatashopExporter(String[] args) {
        if (args.length > 4) {
            throw new UnsupportedOperationException("Illegal arguments." +
                    " Valid options are -i instructor first Name instructor last Name | -d date ");
        }
        String[] sanitized = (args.length > 0) ? sanitizeUserInput(args) : args;
        if(args.length > 0)
            possibilities = (args[1].equals("-i")) ? CommandOptions.INSTRUCTOR
                    : CommandOptions.DATE;
        EntityManager em =
                Persistence.createEntityManagerFactory(PERSISTENCE_UNIT_NAME).
                createEntityManager();
        try
        {
            em.getTransaction().begin();
            Vector<ProblemAttempt> toIterate = new Vector<ProblemAttempt>(em.createQuery(constructQuery(sanitized)).getResultList());
            for (ProblemAttempt e : toIterate) {
            LogProblemAttempt(e);
        }
        em.getTransaction().commit();

        }
        catch(Exception e)
        {
            System.out.println("Some kind of Monster...");
             e.printStackTrace();
        }
        finally
        {
            em.close();
        }
    }

    private static void LogProblemAttempt(final ProblemAttempt e) {
        String fileName = "Andes3_DataShopExport_" + e.getUserName() + "_";
        fileName += DateFormat.getDateInstance().format(
                new Date(System.currentTimeMillis())) + ".xml";
        FileLogger dataShopExport = FileLogger.create(fileName);
        MetaElement meta = new MetaElement(e.getUserName(), true,
                e.getSessionID(),
                e.getStartTime().toString(),
                "Unknown");
        //create the context message element. There is only one per file, per manual
        ContextMessage myMessage = ContextMessage.createStartProblem(meta);
        myMessage.setClassName(e.getClassinformationID().getName());
        myMessage.setSchool(e.getClassinformationID().getSchool());
        myMessage.setPeriod(e.getClassinformationID().getPeriod());
        //no idea what to do about the DFA and Skill -- Don't worry about them
        myMessage.setClassDescription(e.getClassinformationID().getDescription());
        myMessage.addInstructor(e.getClassinformationID().getInstructorName());
        ProblemElement newProblem = new ProblemElement(
                e.getClassinformationID().getDatasetID().getProblemname());
        LevelElement groupLevel = new LevelElement(
                "group", e.getClassinformationID().getDatasetID().getGroupname(),
                newProblem);
        LevelElement moduleLevel = new LevelElement("module",
                e.getClassinformationID().getDatasetID().getModulename(),
                groupLevel);
        myMessage.setDataset(new DatasetElement(
                e.getClassinformationID().getDatasetID().getDatasetname(),
                moduleLevel));
        dataShopExport.log(myMessage);
        logProblemAttemptTransaction(e, myMessage, newProblem, dataShopExport);
    }

    /**
     * sanitizeUserInput() -- tries to prevent SQL injection attacks by removing
     * ' and ;
     * @param args - user input arguments
     * @return a cleansed array
     */
    private static String[] sanitizeUserInput(String[] args) {
        //String[] cleansed = new String[args.length];
        //for(int i=0; i< args.length; i++)
        System.out.println("Pre sanitized" + args);
        for (String arg : args) {
            //cleansed[i] = args[i].replace("'", "").replace(";", "");
            arg = arg.replace("'", "").replace(";", "");
        }
        //return cleansed;
        System.out.println("Post sanitized" + args);
        return args;
    }

    /**
     * constructQuery() -
     * @param args
     */
    private static String constructQuery(final String[] args) {
        String query = new String();


        if (args.length == 0) {
            query = "FROM ProblemAttempt ";
        } else {
            { switch (possibilities) {
                case INSTRUCTOR:
                    //TO DO: GET INSTRUCTOR QUERY WORKING
                    query = "SELECT attempt FROM ProblemAttempt AS attempt " +
                            "INNER JOIN attempt.classinformationID AS info " +
                            String.format("WITH info.instructorName = '\"%s %s\"'",
                            args[1], args[2]);
                    break;
                case DATE:
                    query = String.format("FROM ProblemAttempt where startTime='%s'"
                            , args[1]);
                    break;
                default:
                    throw new UnsupportedOperationException("Invalid option!");
            }
            }
        }


        return query;
    }

    public static void main(final String[] args) {
        final String[] command = {"-d","2009-10-19 04:17:56"};
        DatashopExporter log = getInstance(command);
    }
}
