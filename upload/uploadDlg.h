// uploadDlg.h : header file
//

#if !defined(AFX_UPLOADDLG_H__0EFAAC87_2A04_11D1_BC04_0000C037C67D__INCLUDED_)
#define AFX_UPLOADDLG_H__0EFAAC87_2A04_11D1_BC04_0000C037C67D__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CUploadDlg dialog

class CUploadDlg : public CDialog
{
// Construction
public:
	BOOL m_bExpanded;
	void ContractDlg();
	void ExpandDlg();
	BOOL m_bQuit;
	BOOL ProcessPendingEvents();
	
	
	
	CUploadDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CUploadDlg)
	enum { IDD = IDD_UPLOAD_DIALOG };
	CStatic	m_stcAcctPrompt;
	CStatic	m_stcMidPrompt;
	CEdit	m_editPassword;
	CStatic	m_stcExArea;
	CStatic	m_textStatusMsg;
	CEdit	m_editUserName;
	CEdit	m_textMsg;
	CComboBox	m_cboHosts;
	CString	m_strPassword;
	CString	m_strHost;
	CString	m_strUserName;
	//}}AFX_DATA

	// config info from registry:
	CString m_strAndesPath;			// full path of andes install directory
	CString m_strLogDir;			// full dir of andes log subdirectory
	CString m_strStudentDir;		// full dir of andes student info subdirectory
#if 0
	// config info from windows:
	CString m_strComputer;			// source computer name
	CString m_strWinUser;			// source windows user id
#endif 0

	BOOL m_bSucceeded;				// records if everything succeeded

	void DoTransfer();
	void MPut(HINTERNET hConnect, CString& strDir, LPCTSTR szWildcard, int& nFiles, int& nFail,
		      BOOL bErrorIfNone=TRUE);
	
	// used during a single file transfer:
	CString m_strFileName;			 // source filename without directory
	CString m_strPathName;			 // full pathname of source file
	CString m_strDstSubdir;			 // destination subdirectory of upload directory
	
	// for output
	CString m_strLog;				// our scrolling message log text
	void AddLog(LPCTSTR pszLine);	// adds line to message log
	void Error(DWORD dwError, LPCTSTR pszMsg); // adds error message to message log

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CUploadDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;
	HICON m_hIconSmall;				// to work around MFC bug loading small icons

	// Generated message map functions
	//{{AFX_MSG(CUploadDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnEnterData();
	afx_msg void OnClose();
	virtual void OnCancel();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_UPLOADDLG_H__0EFAAC87_2A04_11D1_BC04_0000C037C67D__INCLUDED_)
