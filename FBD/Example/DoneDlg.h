// DoneDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CDoneDlg dialog
#include "LogEdit.h"
#include "Lgdialog.h"

class CPlanView;

class CDoneDlg : public CLogDialog
{
// Construction
public:
	CString m_btnText;
	CDoneDlg(CFBDDoc* pDoc, CWnd* pParent);   // standard constructor
//	CPlanView* m_pPlanView;
// Dialog Data
	//{{AFX_DATA(CDoneDlg)
	enum { IDD = IDD_DONE };
	CLogBtn	m_Ok;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDoneDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:
	
	// Generated message map functions
	//{{AFX_MSG(CDoneDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
/////////////////////////////////////////////////////////////////////////////
// CExplainDlg dialog

class CExplainDlg : public CLogDialog
{
// Construction
public:
	CExplainDlg(CFBDDoc* pDoc, CWnd* pParent);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CExplainDlg)
	enum { IDD = IDD_EXPLAIN };
	CLogBtn	m_Ok;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CExplainDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CExplainDlg)
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
