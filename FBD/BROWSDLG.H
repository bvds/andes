// BrowsDlg.h : header file
// 
// $Id: BROWSDLG.H,v 1.1 2005/01/24 16:28:09 bvds Exp $

/////////////////////////////////////////////////////////////////////////////
#ifndef BROWSDLG_INCLUDED
#define BROWSDLG_INCLUDED 1

// CBrowserDlg dialog
//{{AFX_INCLUDES()
#include "wbrowser.h"
//}}AFX_INCLUDES

#include "LgDialog.h"
#include "LogEdit.h"

class CBrowserDlg : public CLogDialog
{
// Construction
public:
	WINDOWPLACEMENT m_wp;
	CBrowserDlg(CWnd* pParent = NULL);   // standard constructor

	// Custom window creation func for modeless dialogs of this class.
	BOOL Create(CWnd* pParent = NULL) {
		return CDialog::Create(CBrowserDlg::IDD, pParent);
	}

// Dialog Data
	//{{AFX_DATA(CBrowserDlg)
	enum { IDD = IDD_REVIEW_BROWSER };
	CLogBtn	m_btnClose;
	CStatic	m_stcTopMarker;
	CEdit	m_editTopic;
	CLogBtn	m_btnForward;
	CLogBtn	m_btnBack;
	CWebBrowser	m_browser;
	//}}AFX_DATA

	CString m_strUrl;				// URL to go to, also last navigated to
	CRect	m_rcScreenPos;			// saved last position on screen, NULL if unset

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CBrowserDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

	// override CLogDialog dispatcher to handle custom log events:
	virtual BOOL DispatchEvent(EventID id, LPCTSTR pszArgs);

// Implementation
protected:
	BOOL m_bOurNavigate;	// flags programmatic Navigate cmd to event handler

	// Generated message map functions
	//{{AFX_MSG(CBrowserDlg)
	afx_msg void OnBack();
	afx_msg void OnForward();
	virtual BOOL OnInitDialog();
	afx_msg void OnTitleChangeWebBrowser(LPCTSTR Text);
	afx_msg void OnCommandStateChangeWebBrowser(long Command, BOOL Enable);
	afx_msg void OnBeforeNavigateWebBrowser(LPCTSTR URL, long Flags, LPCTSTR TargetFrameName, VARIANT FAR* PostData, LPCTSTR Headers, BOOL FAR* Cancel);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnStatusTextChangeWebBrowser(LPCTSTR Text);
	afx_msg void OnMove(int x, int y);
	DECLARE_EVENTSINK_MAP()
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif // ! BROWSDLG_INCLUDED