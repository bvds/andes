// 
// LogDialog.h : declarations for CLogDialog, our log-aware dialog class
// 
// $Id: Lgdialog.h,v 1.3 2007/06/22 01:10:40 anders Exp $
//
// Base class for dialogs instrumented to interface with our logging system.
// During recording, these dialogs log startup and size changes. During playback
// they take special action on entry to notify the playback process; they also
// provide the DispatchEvent function for replaying logged dialog control events.
// 
// Dialog classes that use this should include this file in their own headers.
//
// Implementation is currently in history.cpp.
//

/////////////////////////////////////////////////////////////////////////////
// CLogDialog dialog

#ifndef LOG_DIALOG_INCLUDED
#define LOG_DIALOG_INCLUDED 1

class CVariable;

#include "history.h"				// for IEventHandler, EventID

class CLogDialog : public CDialog, public IEventHandler
{
// Construction
public:
	CLogDialog(int id, CWnd* pParent = NULL);   // constructor takes resource id
	DECLARE_DYNAMIC(CLogDialog);
	
// Dialog Data
	//{{AFX_DATA(CLogDialog)
	//}}AFX_DATA

// Operations

	virtual int GetTrainerId(int ctrlId);
	int m_tID;

	// Log event playback support:
	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);
	virtual void PointToObject(LPCTSTR pszObjID);

	// Name-ID mapping table use by log/script event dispatching.
	struct CtlInfo 				// table entry
	{
		const char*	szName;		// ctl name used in log entries
		int		nID;			// child control ID
	};
	// Derived classes override to supply their mapping tables:
	virtual const CtlInfo* GetCtlTbl(int& nEntries) 
			{ nEntries = 0; return NULL; };	// default is no table
	
	int CtlNameToID(LPCSTR pszName);
	const char* CtlIDToName(int nID);

	LPCTSTR GetCtlName(CWnd* pCtl);		// gets string to use in logs

	// To update log player just before dialog ends:
protected:
	void	OnDialogEnd();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLogDialog)
	public:
	virtual int DoModal();
	//}}AFX_VIRTUAL
	virtual void OnOK();
	virtual void OnCancel();

// Implementation
protected:
	CString m_strCboSel;
	BOOL m_bShown;			// set on first show so don't log program moves in init.
	CWnd* GetCtlArg(LPCTSTR szArgs, const char* &pszRest);

	// Generated message map functions
	//{{AFX_MSG(CLogDialog)
	virtual BOOL OnInitDialog();
	afx_msg void OnMove(int x, int y);
	afx_msg BOOL OnHelpInfo(HELPINFO* pHelpInfo);
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

// MFC-style macros for helping derived classes include control info tables
#define DECLARE_CTL_TBL() \
private: \
	static const CtlInfo c_CtlTbl[]; \
	static const int c_nCtlEntries; \
public: \
	virtual const CtlInfo* GetCtlTbl(int& nEntries) \
		{ nEntries = c_nCtlEntries; return c_CtlTbl; }; \

#define BEGIN_CTL_TBL(theClass) \
const CLogDialog::CtlInfo theClass::c_CtlTbl [] = \
{ \

#define END_CTL_TBL(theClass) \
}; \
const theClass::c_nCtlEntries = sizeof(c_CtlTbl)/sizeof(c_CtlTbl[0]); \

#include "FBDDoc.h"
#include "GridCtrl.h"	// Added by ClassView

void AFXAPI DDX_LBMultiSel(CDataExchange* pDX, int nIDC, CStringList& strs);
void AFXAPI DDX_FillList(CDataExchange* pDX, int nIDC, CStringList* pStrList);
void AFXAPI DDX_FillEdit(CDataExchange* pDX, int nIDC, CStringList* pStrList);


/////////////////////////////////////////////////////////////////////////////
// CCheckedDlg dialog -- dialog that enables showing individual control status
//                       with help messages in the hint window
//
// A "pseudo-modal" dialog implemented by creating modeless dialog and
// running a custom message loop that only lets through messages for the 
// dialog and hint window
// 
class CCheckedDlg : public CLogDialog
{
// Construction
public:
	CCheckedDlg(int id, CWnd* pParent = NULL);   // constructor takes resource id
	DECLARE_DYNAMIC(CCheckedDlg);

	// saved constructor parameters for dlg creation:
	int	  m_nId;
	CWnd* m_pParent;

	// To run in special "modeless" way with Hint window enabled
	int  DoModalWithHints();
	BOOL m_bModeless;				// true if running "modeless" i.e. with hints
	BOOL m_bEndModalLoop;
	int  m_nResult;

	void AllowMessages(HWND hwnd) { m_hwndCtrl = hwnd; };

protected:
	//any control of the dialog that needs to receive messages
	//but is not a child of the dialog
	//We need to save its hwnd so we can explicitly pass messages along 
	HWND m_hwndCtrl;

	
public:
	// helpers for dealing with status-bearing colorable controls:
	BOOL   IsCheckedCtrl(CWnd* pCtrl);
	Status GetCtrlStatus(CWnd* pCtrl);
	void   SetCtrlStatus(CWnd* pCtrl, Status status);
	BOOL   GetCtrlEnabled(CWnd* pCtrl);
	void   SetCtrlEnabled(CWnd* pCtrl, BOOL bEnabled);

	BOOL   UpdateStatuses(const CStringList& lstErrors);

	// Get current printable object def for annotating "snapshot" printout
	virtual CString GetPrintDef() { return "???"; }; // must override
	
	// Update appearance on change into/outof tutor mode
	void OnTutorModeChange(BOOL bTutorMode);
	void GreyCtrl(CWnd* pCtrl, BOOL bEnable);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCheckedDlg)
	protected:
		virtual LRESULT DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam);
		virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
	virtual void OnOK();
	virtual void OnCancel();

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CCheckedDlg)
		afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
		afx_msg void OnInitMenuPopup(CMenu* pPopupMenu, UINT nIndex, BOOL bSysMenu);
	afx_msg void OnUpdateDialogWhatswrong(CCmdUI* pCmdUI);
	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif LOG_DIALOG_INCLUDED
