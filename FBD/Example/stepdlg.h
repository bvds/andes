// StepDlg.h : header file
//
//
//$Id: stepdlg.h,v 1.1 2005/01/24 16:28:10 bvds Exp $
/////////////////////////////////////////////////////////////////////////////
// CStepDlg dialog


class CStepDlg : public CDialog
{
// Construction
public:
	int m_nLevel;
	int m_nItemID;
	CString m_oldStep;
	CFBDDoc* m_pDoc;
	CStepDlg(CWnd* pParent = NULL);   // standard constructor
	
// Dialog Data
	//{{AFX_DATA(CStepDlg)
	enum { IDD = IDD_STEP1 };
	CButton	m_Cancel;
	CListBox m_listStep;
	CString	m_strStep;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CStepDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	
	// Generated message map functions
	//{{AFX_MSG(CStepDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnDblclkList1();
	afx_msg void OnBack();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#define BACK		3
