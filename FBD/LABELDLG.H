// LabelDlg.h : header file
// 
// $Id: LABELDLG.H,v 1.1 2005/01/24 16:28:09 bvds Exp $
#include "LogEdit.h"
#include "LgDialog.h"
/////////////////////////////////////////////////////////////////////////////
// CLabelDlg dialog

class CLabelDlg : public CLogDialog
{
// Construction
public:
	CLabelDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CLabelDlg)
	enum { IDD = IDD_LABEL };
	CEdit	m_editText;
	CString	m_strText;
	BOOL	m_bMask;
	BOOL	m_bVisible;
	//}}AFX_DATA

	// Logical font to use for text:
	LOGFONT m_logFont;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLabelDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CFont m_fontText;
	void UpdateEditFont();

	// Generated message map functions
	//{{AFX_MSG(CLabelDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnChooseFont();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
