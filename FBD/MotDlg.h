// MotDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMotionDlg dialog
#include "LgDialog.h"
#include "Logedit.h"

class CMotionDlg : public CLogDialog
{
// Construction
public:
	virtual int GetTrainerId(int ctrlId);
	CMotionDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CMotionDlg)
	enum { IDD = IDD_MOTION_DIAGRAM };
	CLogEdit	m_editInterval;
	CLogCombo	m_cboUnits;
	CButton m_Ok;
	CLogBtn	m_Cancel;
	CLogList	m_listBodies;
	int		m_nIntervalType;
	CString	m_strName;
	CString	m_strBody;
	CString	m_strUnits;
	UINT	m_nInterval;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMotionDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CMotionDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnDefaultTimes();
	afx_msg void OnMeasuredTimes();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
