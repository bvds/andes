// AuthDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CAuthorDlg dialog

class CAuthorDlg : public CPropertyPage
{
// Construction
public:
	CAuthorDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	CDrawObj* m_pObj;
	void UpdateObj();

// Dialog Data
	//{{AFX_DATA(CAuthorDlg)
	enum { IDD = IDD_AUTHOR_PROPS };
	CEdit	m_editID2;
	CEdit	m_editID1;
	CString	m_strId;
	BOOL	m_bProblemObj;
	CString	m_strName;
	CString	m_linkID2;
	CString	m_linkID1;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAuthorDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CAuthorDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
