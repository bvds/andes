#if !defined(AFX_DEMODLG_H__0C9C2701_9D5B_11D1_BC04_0000C037C67D__INCLUDED_)
#define AFX_DEMODLG_H__0C9C2701_9D5B_11D1_BC04_0000C037C67D__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// DemoDlg.h : header file
//

#include "history.h"	// for PlayerUI type

/////////////////////////////////////////////////////////////////////////////
// CDemoDlg dialog

class CDemoDlg : public CDialog, public IPlayerUI
{
// Construction
public:
	// custom constructor for modeless dialog of this type.
	// saves "controlling" frame to forward commands to and notify on delete.  
	// This frame need *not* be made owner ("parent") of dialog, so can
	// float this dialog as a child of the desktop.
	CDemoDlg(CFrameWnd* pControlFrame);
	
	// custom window creation func for modeless dialogs of this type
	BOOL Create(CWnd* pParent = NULL) 
		{ return CDialog::Create(IDD, pParent); };
	
// Dialog Data
	//{{AFX_DATA(CDemoDlg)
	enum { IDD = IDD_DEMO_UI };
	CStatic	m_txtTime;
	CSliderCtrl	m_slider;
	//}}AFX_DATA

// Overrides
	virtual void UpdatePlayerUI();
	virtual void SetProgress(int nPercent);
	virtual void SetPlaybackTime(LPCTSTR szTime);
	virtual void NotifyFinished();

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDemoDlg)
	public:
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CFrameWnd* m_pFrame;

	// Generated message map functions
	//{{AFX_MSG(CDemoDlg)
	afx_msg void OnClose();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	virtual void OnCancel() { OnClose(); };	// Treat ESC like Close
	virtual void OnOK() 	{ };			// Eat RETURN w/o closing.
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DEMODLG_H__0C9C2701_9D5B_11D1_BC04_0000C037C67D__INCLUDED_)
