/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;

import java.io.Serializable;
import java.util.Collection;
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

/**
 *
 * @author The_N_Channel
 */
@Entity
@Table(name = "classinformation")
@NamedQueries({@NamedQuery(name = "Classinformation.findAll", query = "SELECT c FROM Classinformation c"), @NamedQuery(name = "Classinformation.findByClassID", query = "SELECT c FROM Classinformation c WHERE c.classID = :classID"), @NamedQuery(name = "Classinformation.findByName", query = "SELECT c FROM Classinformation c WHERE c.name = :name"), @NamedQuery(name = "Classinformation.findBySchool", query = "SELECT c FROM Classinformation c WHERE c.school = :school"), @NamedQuery(name = "Classinformation.findByPeriod", query = "SELECT c FROM Classinformation c WHERE c.period = :period"), @NamedQuery(name = "Classinformation.findByDescription", query = "SELECT c FROM Classinformation c WHERE c.description = :description"), @NamedQuery(name = "Classinformation.findByInstructorName", query = "SELECT c FROM Classinformation c WHERE c.instructorName = :instructorName"), @NamedQuery(name = "Classinformation.findBySchoolyearInfo", query = "SELECT c FROM Classinformation c WHERE c.schoolyearInfo = :schoolyearInfo")})
public class Classinformation implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Basic(optional = false)
    @Column(name = "classID")
    private Integer classID;
    @Basic(optional = false)
    @Column(name = "name")
    private String name;
    @Basic(optional = false)
    @Column(name = "school")
    private String school;
    @Basic(optional = false)
    @Column(name = "period")
    private String period;
    @Basic(optional = false)
    @Column(name = "description")
    private String description;
    @Basic(optional = false)
    @Column(name = "instructorName")
    private String instructorName;
    @Basic(optional = false)
    @Column(name = "schoolyearInfo")
    private String schoolyearInfo;
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "classinformationID")
    private Collection<ProblemAttempt> problemAttemptCollection;
    @JoinColumn(name = "datasetID", referencedColumnName = "datasetID")
    @ManyToOne(optional = false)
    private StudentDataset datasetID;

    public Classinformation() {
    }

    public Classinformation(Integer classID) {
        this.classID = classID;
    }

    public Classinformation(Integer classID, String name, String school, String period, String description, String instructorName, String schoolyearInfo) {
        this.classID = classID;
        this.name = name;
        this.school = school;
        this.period = period;
        this.description = description;
        this.instructorName = instructorName;
        this.schoolyearInfo = schoolyearInfo;
    }

    public Integer getClassID() {
        return classID;
    }

    public void setClassID(Integer classID) {
        this.classID = classID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSchool() {
        return school;
    }

    public void setSchool(String school) {
        this.school = school;
    }

    public String getPeriod() {
        return period;
    }

    public void setPeriod(String period) {
        this.period = period;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getInstructorName() {
        return instructorName;
    }

    public void setInstructorName(String instructorName) {
        this.instructorName = instructorName;
    }

    public String getSchoolyearInfo() {
        return schoolyearInfo;
    }

    public void setSchoolyearInfo(String schoolyearInfo) {
        this.schoolyearInfo = schoolyearInfo;
    }

    public Collection<ProblemAttempt> getProblemAttemptCollection() {
        return problemAttemptCollection;
    }

    public void setProblemAttemptCollection(Collection<ProblemAttempt> problemAttemptCollection) {
        this.problemAttemptCollection = problemAttemptCollection;
    }

    public StudentDataset getDatasetID() {
        return datasetID;
    }

    public void setDatasetID(StudentDataset datasetID) {
        this.datasetID = datasetID;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (classID != null ? classID.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof Classinformation)) {
            return false;
        }
        Classinformation other = (Classinformation) object;
        if ((this.classID == null && other.classID != null) || (this.classID != null && !this.classID.equals(other.classID))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "andesdatashopcommunication.Classinformation[classID=" + classID + "]";
    }

}
