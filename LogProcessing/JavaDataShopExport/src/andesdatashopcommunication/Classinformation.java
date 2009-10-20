/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package andesdatashopcommunication;

import java.io.Serializable;
import java.util.List;
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
 * @author master
 */
@Entity
@Table(name = "CLASS_INFORMATION")
@NamedQueries({@NamedQuery(name = "ClassInformation.findAll", query = "SELECT c FROM ClassInformation c"), @NamedQuery(name = "ClassInformation.findByClassID", query = "SELECT c FROM ClassInformation c WHERE c.classID = :classID"), @NamedQuery(name = "ClassInformation.findByName", query = "SELECT c FROM ClassInformation c WHERE c.name = :name"), @NamedQuery(name = "ClassInformation.findBySchool", query = "SELECT c FROM ClassInformation c WHERE c.school = :school"), @NamedQuery(name = "ClassInformation.findByPeriod", query = "SELECT c FROM ClassInformation c WHERE c.period = :period"), @NamedQuery(name = "ClassInformation.findByDescription", query = "SELECT c FROM ClassInformation c WHERE c.description = :description"), @NamedQuery(name = "ClassInformation.findByInstructorName", query = "SELECT c FROM ClassInformation c WHERE c.instructorName = :instructorName"), @NamedQuery(name = "ClassInformation.findBySchoolyearInfo", query = "SELECT c FROM ClassInformation c WHERE c.schoolyearInfo = :schoolyearInfo")})
public class ClassInformation implements Serializable {
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
    private List<ProblemAttempt> problemAttemptList;
    @JoinColumn(name = "datasetID", referencedColumnName = "datasetID")
    @ManyToOne(optional = false)
    private StudentDataset datasetID;

    public ClassInformation() {
    }

    public ClassInformation(Integer classID) {
        this.classID = classID;
    }

    public ClassInformation(Integer classID, String name, String school, String period, String description, String instructorName, String schoolyearInfo) {
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

    public List<ProblemAttempt> getProblemAttemptList() {
        return problemAttemptList;
    }

    public void setProblemAttemptList(List<ProblemAttempt> problemAttemptList) {
        this.problemAttemptList = problemAttemptList;
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
        if (!(object instanceof ClassInformation)) {
            return false;
        }
        ClassInformation other = (ClassInformation) object;
        if ((this.classID == null && other.classID != null) || (this.classID != null && !this.classID.equals(other.classID))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "andesdatashopcommunication.ClassInformation[classID=" + classID + "]";
    }

}
