// EXHintDg.h : header file
//
#include "LgDialog.h"
#include "LogEdit.h"		// for logging buttons

/////////////////////////////////////////////////////////////////////////////
// CEXHintDlg dialog

class CEXHintDlg : public CLogDialog
{
// Construction
public:
	CString m_strHints;
	CString m_strCode;
	CString m_strId;
	LPCSTR m_pszHintSpec;			  // Set coming in 
	CEXHintDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CEXHintDlg)
	enum { IDD = IDD_EXHINT };
	CLogBtn	m_btnYes;
	CLogBtn	m_btnOK;
	CLogEdit	m_editHint;
	CLogBtn	m_btnNoThanks;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEXHintDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void OnClarify();

	// Generated message map functions
	//{{AFX_MSG(CEXHintDlg)
	afx_msg void OnNothanks();
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	afx_msg void OnYes();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#define	ID_CLARIFY		3
#define ID_NOTHANKS		4
