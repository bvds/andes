/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;
import java.util.List;
import java.util.ArrayList;
import java.util.ListIterator;
import java.util.Vector;
import javax.persistence.EntityManager;
import javax.persistence.Persistence;
import org.json.JSONException;
/**
 *
 * @author Andes Version 3 Tutoring System
 */
public class ClientServerInteractions {

    private ProblemAttempt toGroup;
    private List<ClientServerInteraction> interactions;

    /**
     * ClientServerInteractions() - Constructor that groups the client and server
     * interactions logically based on a passed in problem
     * @param aProblem a Problem attempt for which to agglomerate the interactions
     * @throws JSONException thrown if JSON in DB is bad
     */
    public ClientServerInteractions(ProblemAttempt aProblem) throws JSONException
    {
        toGroup = aProblem;
        interactions = new ArrayList<ClientServerInteraction>();
        ListIterator<ProblemAttemptTransaction> iter =
                toGroup.getProblemAttemptTransactionCollection().listIterator();
        while(iter.hasNext())
        {
            ProblemAttemptTransaction newOne = iter.next(); //iterators start at -1
            if(newOne.getInitiatingParty().equalsIgnoreCase("client"))
             {
                 ClientServerInteraction anInteraction = new ClientServerInteraction();
                 anInteraction.setClientAction(new Andes3ClientAction(newOne.getCommand()));
                 List<Andes3ServerAction> serverStuff = new ArrayList<Andes3ServerAction>();
                 ProblemAttemptTransaction servy = iter.next();// move past the client one
                 while(iter.hasNext() && servy.getInitiatingParty().equalsIgnoreCase("server"))
                 {
                     serverStuff.add(new Andes3ServerAction(servy.getCommand()));
                     servy = iter.next();//keep going until we see the next client
                 }
                 //special handling of last element. Iterators are hard
                 if(!iter.hasNext() && servy.getInitiatingParty().equalsIgnoreCase("server"))
                     serverStuff.add(new Andes3ServerAction(servy.getCommand()));
                 anInteraction.setServerActions(serverStuff);
                 interactions.add(anInteraction);
                 iter.previous();// go backwards, otherwise we skip a client server interaction
             }
        }
    }

    /**
     * @return the interactions
     */
    public List<ClientServerInteraction> getInteractions() {
        return interactions;
    }

    public static void main(String[] args) throws JSONException
    {
        EntityManager em = Persistence.createEntityManagerFactory("AndesDataShopCommunicationPU").createEntityManager();
            em.getTransaction().begin();
            Vector<ProblemAttempt> toIterate = new Vector<ProblemAttempt>(em.createQuery("FROM ProblemAttempt WHERE attemptID=1").getResultList());
            em.close();
        ClientServerInteractions toTest = new ClientServerInteractions(toIterate.firstElement());
        for(ClientServerInteraction e : toTest.getInteractions())
            System.out.println(e.toString());
    }
}
