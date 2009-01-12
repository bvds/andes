// EXTxtDlg.h : header file
// 
// $Id: EXTXTDLG.H,v 1.1 2005/01/24 16:28:10 bvds Exp $

/////////////////////////////////////////////////////////////////////////////
// CEXTextDlg dialog

class CEXTextDlg : public CDialog
{
// Construction
public:
	CEXTextDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CEXTextDlg)
	enum { IDD = IDD_EXAMPLE_TEXT };
	CComboBox	m_cboMenuList;
	CString	m_strMenu;
	CString	m_strText;
	BOOL	m_bVisible;
	CString	m_strId;
	//}}AFX_DATA
	LOGFONT m_logFont;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEXTextDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CEXTextDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnChooseFont();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
