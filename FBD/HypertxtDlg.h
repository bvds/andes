#if !defined(AFX_HYPERTXTDLG_H__1FD69523_BA58_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_HYPERTXTDLG_H__1FD69523_BA58_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// HypertxtDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CHypertxtDlg dialog
class CHyperLnk;

class CHypertxtDlg : public CDialog
{
// Construction
public:
	CHypertxtDlg(CHyperLnk* pLnk = NULL, CWnd* pParent = NULL);   // standard constructor
	CHyperLnk* m_pLnk;

	CFont m_fontText;
	LOGFONT m_logFont;

// Dialog Data
	//{{AFX_DATA(CHypertxtDlg)
	enum { IDD = IDD_HYPER_TEXT };
	CButton	m_btnHyperType;
	CEdit	m_editLinkTxt;
	CEdit	m_editHyperTxt;
	CEdit	m_editDefTxt;
	int		m_nHyperType;
	CString	m_strText;
	CString	m_strDef;
	CString	m_strLink;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHypertxtDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CHypertxtDlg)
	afx_msg void OnLinkbtn();
	afx_msg void OnDefbtn();
	virtual BOOL OnInitDialog();
	afx_msg void OnChooseFont();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_HYPERTXTDLG_H__1FD69523_BA58_11D1_A09F_0000C0086DCF__INCLUDED_)
