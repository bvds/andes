// MCQDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMCQDlg dialog

class CMCQDlg : public CDialog
{
// Construction
public:
	CStringList m_strsChoices;
	CMCQDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CMCQDlg)
	enum { IDD = IDD_CHOICE_GROUP };
	CListBox	m_listChoices;
	CSpinButtonCtrl	m_spinCorrect;
	CString	m_strQuestion;
	UINT	m_nCorrect;
	CString	m_strId;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMCQDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CMCQDlg)
	afx_msg void OnChoiceAdd();
	afx_msg void OnChoiceEdit();
	afx_msg void OnChoiceRemove();
	virtual BOOL OnInitDialog();
	afx_msg void OnChoiceDown();
	afx_msg void OnChoiceUp();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
