#if !defined(AFX_TRANSFERDLG_H__043764E0_D230_11D5_8735_00036D1733F4__INCLUDED_)
#define AFX_TRANSFERDLG_H__043764E0_D230_11D5_8735_00036D1733F4__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// TransferDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CTransferDlg dialog

class CTransferDlg : public CDialog
{
// Construction
public:
	CTransferDlg(CWnd* pParent = NULL);   // standard constructor

// Attributes:
	// set by caller coming in:
	BOOL m_bUpload;			// T=> uploading, else downloading
	CString m_strStudent;	// student name

	HINTERNET hInet;
	HINTERNET hFTP;
	CString m_strHost;
	CString m_strUserName;
	CString m_strPassword;

	BOOL m_bLogUploadFailed;
	BOOL m_bDebug;

// Dialog Data
	//{{AFX_DATA(CTransferDlg)
	enum { IDD = IDD_TRANSFER };
	CStatic	m_textStatusMsg;
	CStatic	m_textTransferMsg;
	CEdit	m_textMsg;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTransferDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Operations
	void DoTransfer();
	
	void MPut(HINTERNET hConnect, CString& strDir, LPCTSTR szWildcard, int& nFiles, int& nFail);
	
	// used during a single file transfer:
	CString m_strFileName;			 // source filename without directory
	CString m_strPathName;			 // full pathname of source file
	CString m_strDstSubdir;			 // destination subdirectory of upload directory
	

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CTransferDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnClose();
	virtual void OnCancel();
	afx_msg void OnTimer(UINT nIDEvent);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	BOOL DoDownload(HINTERNET hConnect);
	BOOL DoUpload(HINTERNET hConnect);
	
	// for cancelling:
	BOOL m_bQuit;
	BOOL ProcessPendingEvents();

	// for output
	CString m_strLog;				// our scrolling message log text
	void AddLog(LPCTSTR pszLine);	// adds line to message log
	void Error(DWORD dwError, LPCTSTR pszMsg); // adds error message to message log
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TRANSFERDLG_H__043764E0_D230_11D5_8735_00036D1733F4__INCLUDED_)
