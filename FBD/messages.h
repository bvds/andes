// messages.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CInstructMsg dialog
#include "LogEdit.h"
#include "Lgdialog.h"

#include "EQEdit.h"

class CInstructMsg : public CLogDialog
{
// Construction
public:
	CInstructMsg(CFBDDoc* pDoc = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CInstructMsg)
	enum { IDD = IDD_INSTRUCT };
	CLogBtn	m_Ok;
	CString	m_message;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CInstructMsg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CInstructMsg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
/////////////////////////////////////////////////////////////////////////////
// CWarningMsg dialog

class CWarningMsg : public CLogDialog
{
// Construction
public:
	BOOL m_nType;
	BOOL m_bInfo;
	CWarningMsg(CFBDDoc* pDoc = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CWarningMsg)
	enum { IDD = IDD_WARNING };
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CString	m_message;
	CStatic m_picture;
	//}}AFX_DATA
	CRichEditEx m_stcMessage;

// operations
	void CenterButton(CButton* pBtn);


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CWarningMsg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CWarningMsg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
