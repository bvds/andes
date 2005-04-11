// PlayDlg.h : header file
// 
// $Id: PLAYDLG.H,v 1.2 2005/04/11 18:53:54 anders Exp $

/////////////////////////////////////////////////////////////////////////////
// CPlayDlg dialog

#include "history.h"	// required for type IPlayerUI.

class CPlayDlg : public CDialog, public IPlayerUI
{
// Construction
public:
/*
	// standard constructor for dlgs of this class saves parent (not used)
	CPlayDlg(CWnd* pParent = NULL);  
*/
	// custom constructor for modeless dialog of this type.
	// saves "controlling" frame to forward commands to and notify on delete.  
	// This frame need *not* be made owner ("parent") of dialog, so can
	// float this dialog as a child of the desktop.
	CPlayDlg(CFrameWnd* pControlFrame);
	
	// Custom window create func for modeless dialogs of this class.
	BOOL Create(CWnd* pParent = NULL);

// Operations
	
// Dialog Data
	//{{AFX_DATA(CPlayDlg)
	enum { IDD = IDD_PLAYER };
	CStatic	m_txtPlayed;
	CSliderCtrl	m_slider;
	CStatic	m_txtTime;
	CStatic	m_txtMessage;
	CString	m_strFileName;
	UINT	m_nSecs;
	UINT	m_nMins;
	UINT	m_nHrs;
	//}}AFX_DATA

	CString m_strPathName;		// full path of logfile (invisible)
public:
	void SetLogFile(LPCTSTR pszPathName);
	void GetPathName(CString& strPathName) { strPathName = m_strPathName; };

// Overrides
	virtual void SetPlaybackTime(LPCTSTR szTime);
	virtual void SetProgress(int nPercent);
	virtual void ShowNextEvent(LPCTSTR szMsg);
	virtual void UpdatePlayerUI();
	virtual void NotifyFinished();

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPlayDlg)
	public:
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CFrameWnd* m_pFrame;

	// Generated message map functions
	//{{AFX_MSG(CPlayDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnClose();
	afx_msg void OnSelectLogfile();
	afx_msg void OnPlayerGoto();
	afx_msg void OnDropFiles(HDROP hDropInfo);
	//}}AFX_MSG
	virtual void OnCancel();
	virtual void OnOK();
	DECLARE_MESSAGE_MAP()
};


