/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;

import java.io.Serializable;
import java.util.List;
import java.util.Date;
import javax.persistence.Basic;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 *
 * @author Andes Version 3 Tutoring System
 */
@Entity
@Table(name = "problem_attempt")
@NamedQueries({@NamedQuery(name = "ProblemAttempt.findAll", query = "SELECT p FROM ProblemAttempt p"), @NamedQuery(name = "ProblemAttempt.findByUserName", query = "SELECT p FROM ProblemAttempt p WHERE p.userName = :userName"), @NamedQuery(name = "ProblemAttempt.findBySessionID", query = "SELECT p FROM ProblemAttempt p WHERE p.sessionID = :sessionID"), @NamedQuery(name = "ProblemAttempt.findByStartTime", query = "SELECT p FROM ProblemAttempt p WHERE p.startTime = :startTime"), @NamedQuery(name = "ProblemAttempt.findByAttemptID", query = "SELECT p FROM ProblemAttempt p WHERE p.attemptID = :attemptID")})
public class ProblemAttempt implements Serializable {
    private static final long serialVersionUID = 1L;
    @Basic(optional = false)
    @Column(name = "userName")
    private String userName;
    @Basic(optional = false)
    @Column(name = "sessionID")
    private String sessionID;
    @Basic(optional = false)
    @Column(name = "startTime")
    @Temporal(TemporalType.TIMESTAMP)
    private Date startTime;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Basic(optional = false)
    @Column(name = "attemptID")
    private Integer attemptID;
    @JoinColumn(name = "classinformationID", referencedColumnName = "classID")
    @ManyToOne(optional = false)
    private Classinformation classinformationID;
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "problemAttempt")
    private List<ProblemAttemptTransaction> problemAttemptTransactionCollection;

    public ProblemAttempt() {
    }

    public ProblemAttempt(Integer attemptID) {
        this.attemptID = attemptID;
    }

    public ProblemAttempt(Integer attemptID, String userName, String sessionID, Date startTime) {
        this.attemptID = attemptID;
        this.userName = userName;
        this.sessionID = sessionID;
        this.startTime = startTime;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getSessionID() {
        return sessionID;
    }

    public void setSessionID(String sessionID) {
        this.sessionID = sessionID;
    }

    public Date getStartTime() {
        return startTime;
    }

    public void setStartTime(Date startTime) {
        this.startTime = startTime;
    }

    public Integer getAttemptID() {
        return attemptID;
    }

    public void setAttemptID(Integer attemptID) {
        this.attemptID = attemptID;
    }

    public Classinformation getClassinformationID() {
        return classinformationID;
    }

    public void setClassinformationID(Classinformation classinformationID) {
        this.classinformationID = classinformationID;
    }

    public List<ProblemAttemptTransaction> getProblemAttemptTransactionCollection() {
        return problemAttemptTransactionCollection;
    }

    public void setProblemAttemptTransactionCollection(List<ProblemAttemptTransaction> problemAttemptTransactionCollection) {
        this.problemAttemptTransactionCollection = problemAttemptTransactionCollection;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (attemptID != null ? attemptID.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof ProblemAttempt)) {
            return false;
        }
        ProblemAttempt other = (ProblemAttempt) object;
        if ((this.attemptID == null && other.attemptID != null) || (this.attemptID != null && !this.attemptID.equals(other.attemptID))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "andesdatashopcommunication.ProblemAttempt[attemptID=" + attemptID + "]";
    }

}
