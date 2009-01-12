// AreaDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CAreaDlg dialog

class CAreaDlg : public CDialog
{
// Construction
public:
	CAreaDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CAreaDlg)
	enum { IDD = IDD_AREA };
	CString	m_strName;
	BOOL	m_bBorder;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAreaDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CAreaDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
