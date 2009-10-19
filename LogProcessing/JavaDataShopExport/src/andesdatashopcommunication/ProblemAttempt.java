/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import javax.persistence.Basic;
import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
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
 * @author master
 */
@Entity
@Table(name = "PROBLEM_ATTEMPT")
@NamedQueries({@NamedQuery(name = "ProblemAttempt.findAll", query = "SELECT p FROM ProblemAttempt p"), @NamedQuery(name = "ProblemAttempt.findByUserName", query = "SELECT p FROM ProblemAttempt p WHERE p.userName = :userName"), @NamedQuery(name = "ProblemAttempt.findBySessionID", query = "SELECT p FROM ProblemAttempt p WHERE p.sessionID = :sessionID"), @NamedQuery(name = "ProblemAttempt.findByStartTime", query = "SELECT p FROM ProblemAttempt p WHERE p.startTime = :startTime"), @NamedQuery(name = "ProblemAttempt.findByClientID", query = "SELECT p FROM ProblemAttempt p WHERE p.clientID = :clientID"), @NamedQuery(name = "ProblemAttempt.findByUserProblem", query = "SELECT p FROM ProblemAttempt p WHERE p.userProblem = :userProblem"), @NamedQuery(name = "ProblemAttempt.findByUserSection", query = "SELECT p FROM ProblemAttempt p WHERE p.userSection = :userSection")})
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
    @Basic(optional = false)
    @Column(name = "clientID")
    private String clientID;
    @Column(name = "userProblem")
    private String userProblem;
    @Column(name = "userSection")
    private String userSection;
    @JoinColumn(name = "classinformationID", referencedColumnName = "classID")
    @ManyToOne(optional = false)
    private ClassInformation classinformationID;
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "problemAttempt")
    private List<ProblemAttemptTransaction> problemAttemptTransactionList;

    public ProblemAttempt() {
    }

    public ProblemAttempt(String clientID) {
        this.clientID = clientID;
    }

    public ProblemAttempt(String clientID, String userName, String sessionID, Date startTime) {
        this.clientID = clientID;
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

    public String getClientID() {
        return clientID;
    }

    public void setClientID(String clientID) {
        this.clientID = clientID;
    }

    public String getUserProblem() {
        return userProblem;
    }

    public void setUserProblem(String userProblem) {
        this.userProblem = userProblem;
    }

    public String getUserSection() {
        return userSection;
    }

    public void setUserSection(String userSection) {
        this.userSection = userSection;
    }

    public ClassInformation getClassinformationID() {
        return classinformationID;
    }

    public void setClassinformationID(ClassInformation classinformationID) {
        this.classinformationID = classinformationID;
    }

    public List<ProblemAttemptTransaction> getProblemAttemptTransactionList() {
        return problemAttemptTransactionList;
    }

    public void setProblemAttemptTransactionList(List<ProblemAttemptTransaction> problemAttemptTransactionList) {
        this.problemAttemptTransactionList = problemAttemptTransactionList;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (clientID != null ? clientID.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof ProblemAttempt)) {
            return false;
        }
        ProblemAttempt other = (ProblemAttempt) object;
        if ((this.clientID == null && other.clientID != null) || (this.clientID != null && !this.clientID.equals(other.clientID))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "andesdatashopcommunication.ProblemAttempt[clientID=" + clientID + "]";
    }

}
