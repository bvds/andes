// ExpBdyDg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CExplainBodyDlg dialog
#include "TemplateDlg.h"

class CFBDDoc;

class CExplainBodyDlg : public CTemplateDlg
{
// Construction
public:
	void ResetTemplate();

	CWnd* m_pParent;
	CFBDDoc* m_pDoc;
	CExplainBodyDlg(CFBDDoc* pDoc = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CExplainBodyDlg)
	enum { IDD = IDD_EXPLAIN_BODY };
	private:
	CStatic	m_stcThen;
	CStatic	m_stcIF;
	CStatic	m_stcExertedBy;
	CStatic	m_stcDueTo;
	CStatic	m_stcAND1;
	CLogCombo	m_cboExertedBy;
	CLogCombo	m_cboDueTo;
	CLogCombo	m_cboAND1;
	CLogCombo	m_cboIF;
	//}}AFX_DATA

//operations
	virtual void InsertCtrlStrs();



// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CExplainBodyDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:


	// Generated message map functions
	//{{AFX_MSG(CExplainBodyDlg)
	afx_msg void OnSubmit();
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
