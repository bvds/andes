// HintDlg.h : header file
//
#include "LgDialog.h"
#include "LogEdit.h"		// for logging buttons
/////////////////////////////////////////////////////////////////////////////
// CHintDlg dialog

class CHintDlg : public CLogDialog
{
	DECLARE_DYNCREATE(CHintDlg)
// Construction
public:
	CHintDlg(CWnd* pParent = NULL);   // standard constructor

	// Hint spec points to quote delimited string owned by another module.
	// Consists of hint message plus optional tilde plus list of followup button letters.
	// May be NULL for failure
	void SetHintSpec(LPCTSTR pszHintSpec);
	
	LPCTSTR m_pszHintSpec;			// NB pointed-to string is owned by caller			   
	
// Dialog Data
	//{{AFX_DATA(CHintDlg)
	enum { IDD = IDD_HINT };
	CButton	m_btnExplMore;
	CButton	m_btnWhy;
	CButton	m_btnHow;
	CEdit	m_editHint;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHintDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void ProcessSpec();

	// Generated message map functions
	//{{AFX_MSG(CHintDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnExplainMore();
	afx_msg void OnBtnHow();
	afx_msg void OnBtnWhy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
