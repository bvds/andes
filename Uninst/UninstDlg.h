// UninstDlg.h : header file
//

#if !defined(AFX_UNINSTDLG_H__7E891B89_4A0D_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_UNINSTDLG_H__7E891B89_4A0D_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CUninstDlg dialog

class CUninstDlg : public CDialog
{
// Construction
public:
	void DeleteDirectories();
	BOOL BeginUninstall();
	CUninstDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CUninstDlg)
	enum { IDD = IDD_UNINST_DIALOG };
	CProgressCtrl	m_Progress;
	CButton	m_Ok;
	CStatic	m_scChk;
	CStatic	m_regChk;
	CStatic	m_progChk;
	CStatic	m_dirChk;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CUninstDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:
	int CalcNumFiles();
	int CalcNumDirs();
	void MyDeleteFile(CString fName);
	void DeleteFiles();
	void DeleteShortcuts();
	void RemoveRegEntries();
	//helper function to delete registry keys
	//needed for window NT;
	void DeleteSubkeys( HKEY hKey, CString strKey);

	// Generated message map functions
	//{{AFX_MSG(CUninstDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_UNINSTDLG_H__7E891B89_4A0D_11D1_A09F_0000C0086DCF__INCLUDED_)
