#if !defined(AFX_VERMSGDLG_H__3F32EB22_AAE5_11D2_B9ED_8A414F43F07B__INCLUDED_)
#define AFX_VERMSGDLG_H__3F32EB22_AAE5_11D2_B9ED_8A414F43F07B__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// VerMsgDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CVerMsgDlg dialog

class CVerMsgDlg : public CDialog
{
// Construction
public:
	CVerMsgDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CVerMsgDlg)
	enum { IDD = IDD_VERSIONMSG_DLG };
	CStatic	m_stcMsg;
	CString	m_strMsg;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVerMsgDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CVerMsgDlg)
	afx_msg void OnYestoall();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VERMSGDLG_H__3F32EB22_AAE5_11D2_B9ED_8A414F43F07B__INCLUDED_)
