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
 * @author Andes Version 3 Tutoring System
 */
@Embeddable
public class ProblemAttemptTransactionPK implements Serializable {
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Basic(optional = false)
    @Column(name = "tID")
    private int tID;
    @Basic(optional = false)
    @Column(name = "attemptID")
    private int attemptID;

    public ProblemAttemptTransactionPK() {
    }

    public ProblemAttemptTransactionPK(int tID, int attemptID) {
        this.tID = tID;
        this.attemptID = attemptID;
    }

    public int getTID() {
        return tID;
    }

    public void setTID(int tID) {
        this.tID = tID;
    }

    public int getAttemptID() {
        return attemptID;
    }

    public void setAttemptID(int attemptID) {
        this.attemptID = attemptID;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (int) tID;
        hash += (int) attemptID;
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
        if (this.attemptID != other.attemptID) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "andesdatashopcommunication.ProblemAttemptTransactionPK[tID=" + tID + ", attemptID=" + attemptID + "]";
    }

}
