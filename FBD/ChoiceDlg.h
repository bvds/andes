// ChoiceDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CChoiceDlg dialog

class CChoiceDlg : public CDialog
{
// Construction
public:
	CChoiceDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CChoiceDlg)
	enum { IDD = IDD_CHOICE };
	BOOL	m_bCorrect;
	CString	m_strText;
	BOOL	m_bChosen;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChoiceDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CChoiceDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
