// LoginDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CLoginDlg dialog
#include "LogEdit.h"
#include "Lgdialog.h"


class CLoginDlg : public CLogDialog
{
// Construction
public:
	CLoginDlg(CFBDDoc* pDoc = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CLoginDlg)
	enum { IDD = IDD_LOGIN };
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CLogCombo	m_listName;
	CString	m_strName;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLoginDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
	virtual void OnCancel();

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CLoginDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnDblclkLogname();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


void AFXAPI DDV_LoginName(CDataExchange* pDX, int nIDC, CString editStr);
