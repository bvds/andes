#if !defined(AFX_COMMENTDLG_H__11CA4680_35FE_11D2_807C_B73A8030B756__INCLUDED_)
#define AFX_COMMENTDLG_H__11CA4680_35FE_11D2_807C_B73A8030B756__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// CommentDlg.h : header file
//

#include "Lgdialog.h"
#include "Logedit.h"

/////////////////////////////////////////////////////////////////////////////
// CCommentDlg dialog

class CCommentDlg : public CLogDialog
{
// Construction
public:
	int m_nTimeStart;
	void SetText(LPCTSTR pszText);
	CCommentDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CCommentDlg)
	enum { IDD = IDD_COMMENT };
	CButton	m_btnCancel;
	CButton	m_btnOK;
	CString	m_strText;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCommentDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CCommentDlg)
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	virtual void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_COMMENTDLG_H__11CA4680_35FE_11D2_807C_B73A8030B756__INCLUDED_)
