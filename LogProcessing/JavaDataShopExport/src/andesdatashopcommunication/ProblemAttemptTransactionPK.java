/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;

import java.io.Serializable;
import javax.persistence.Basic;
import javax.persistence.Column;
import javax.persistence.Embeddable;

/**
 *
 * @author master
 */
@Embeddable
public class ProblemAttemptTransactionPK implements Serializable {
    @Basic(optional = false)
    @Column(name = "tID")
    private int tID;
    @Basic(optional = false)
    @Column(name = "clientID")
    private String clientID;

    public ProblemAttemptTransactionPK() {
    }

    public ProblemAttemptTransactionPK(int tID, String clientID) {
        this.tID = tID;
        this.clientID = clientID;
    }

    public int getTID() {
        return tID;
    }

    public void setTID(int tID) {
        this.tID = tID;
    }

    public String getClientID() {
        return clientID;
    }

    public void setClientID(String clientID) {
        this.clientID = clientID;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (int) tID;
        hash += (clientID != null ? clientID.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof ProblemAttemptTransactionPK)) {
            return false;
        }
        ProblemAttemptTransactionPK other = (ProblemAttemptTransactionPK) object;
        if (this.tID != other.tID) {
            return false;
        }
        if ((this.clientID == null && other.clientID != null) || (this.clientID != null && !this.clientID.equals(other.clientID))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "andesdatashopcommunication.ProblemAttemptTransactionPK[tID=" + tID + ", clientID=" + clientID + "]";
    }

}
