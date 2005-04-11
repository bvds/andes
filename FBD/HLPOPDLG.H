// HelpOptsDlg.h : header file
// 
// $Id: HLPOPDLG.H,v 1.2 2005/04/11 18:53:54 anders Exp $

/////////////////////////////////////////////////////////////////////////////
// CHelpOptsDlg dialog

class CHelpOptsDlg : public CDialog
{
// Construction
public:
	CHelpOptsDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CHelpOptsDlg)
	enum { IDD = IDD_HELP_OPTIONS };
	int		m_nFeedback;
	BOOL	m_bCheckEquations;
	BOOL	m_bCheckDiagram;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHelpOptsDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CHelpOptsDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
