#if !defined(AFX_PROGRESSDLG_H__5201E775_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_PROGRESSDLG_H__5201E775_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ProgressDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CProgressDlg dialog

class CProgressDlg : public CDialog
{
// Construction
public:
	CMySheet* m_pSht;
	int CalcNumFiles();
	BOOL CheckCancelButton();
	void PumpMessages();

	BOOL MyCopyDLL(CString strFileName, CString strSourcePath, CString strDestPath);
	BOOL MyCopyFile(CString strFileName, CString strSourcePath, CString strDestPath);
	CProgressDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CProgressDlg)
	enum { IDD = IDD_PROGRESS_DLG };
	CStatic	m_ctrlFileName;
	CProgressCtrl	m_ctrlCopyProgress;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CProgressDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:
	BOOL m_bCancel;

	// Generated message map functions
	//{{AFX_MSG(CProgressDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnCancel();
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PROGRESSDLG_H__5201E775_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
