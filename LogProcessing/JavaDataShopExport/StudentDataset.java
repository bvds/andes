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
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 *
 * @author Andes Version 3 Tutoring System
 */
@Entity
@Table(name = "student_dataset")
@NamedQueries({@NamedQuery(name = "StudentDataset.findAll", query = "SELECT s FROM StudentDataset s"), @NamedQuery(name = "StudentDataset.findByDatasetID", query = "SELECT s FROM StudentDataset s WHERE s.datasetID = :datasetID"), @NamedQuery(name = "StudentDataset.findByDatasetname", query = "SELECT s FROM StudentDataset s WHERE s.datasetname = :datasetname"), @NamedQuery(name = "StudentDataset.findByModulename", query = "SELECT s FROM StudentDataset s WHERE s.modulename = :modulename"), @NamedQuery(name = "StudentDataset.findByGroupname", query = "SELECT s FROM StudentDataset s WHERE s.groupname = :groupname"), @NamedQuery(name = "StudentDataset.findByProblemname", query = "SELECT s FROM StudentDataset s WHERE s.problemname = :problemname")})
public class StudentDataset implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Basic(optional = false)
    @Column(name = "datasetID")
    private Integer datasetID;
    @Basic(optional = false)
    @Column(name = "datasetname")
    private String datasetname;
    @Basic(optional = false)
    @Column(name = "modulename")
    private String modulename;
    @Basic(optional = false)
    @Column(name = "groupname")
    private String groupname;
    @Basic(optional = false)
    @Column(name = "problemname")
    private String problemname;
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "datasetID")
    private Collection<Classinformation> classinformationCollection;

    public StudentDataset() {
    }

    public StudentDataset(Integer datasetID) {
        this.datasetID = datasetID;
    }

    public StudentDataset(Integer datasetID, String datasetname, String modulename, String groupname, String problemname) {
        this.datasetID = datasetID;
        this.datasetname = datasetname;
        this.modulename = modulename;
        this.groupname = groupname;
        this.problemname = problemname;
    }

    public Integer getDatasetID() {
        return datasetID;
    }

    public void setDatasetID(Integer datasetID) {
        this.datasetID = datasetID;
    }

    public String getDatasetname() {
        return datasetname;
    }

    public void setDatasetname(String datasetname) {
        this.datasetname = datasetname;
    }

    public String getModulename() {
        return modulename;
    }

    public void setModulename(String modulename) {
        this.modulename = modulename;
    }

    public String getGroupname() {
        return groupname;
    }

    public void setGroupname(String groupname) {
        this.groupname = groupname;
    }

    public String getProblemname() {
        return problemname;
    }

    public void setProblemname(String problemname) {
        this.problemname = problemname;
    }

    public Collection<Classinformation> getClassinformationCollection() {
        return classinformationCollection;
    }

    public void setClassinformationCollection(Collection<Classinformation> classinformationCollection) {
        this.classinformationCollection = classinformationCollection;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (datasetID != null ? datasetID.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof StudentDataset)) {
            return false;
        }
        StudentDataset other = (StudentDataset) object;
        if ((this.datasetID == null && other.datasetID != null) || (this.datasetID != null && !this.datasetID.equals(other.datasetID))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "andesdatashopcommunication.StudentDataset[datasetID=" + datasetID + "]";
    }

}
