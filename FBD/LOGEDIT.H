// 
// LogEdit.h : definition file for our log-aware control classes
// 
// $Id: LOGEDIT.H,v 1.6 2007/07/25 03:45:13 anders Exp $
//
// We use MFC subclassed control classes which are instrumented to log significant
// events to the app's log file by catching reflected control notifications.
// These classes make use of the global Logf function to do their recording.
// Define the dialog classes to use these classes via ClassWizard. Dialog classes 
// using these controls should include this file in their own header files.
//
// They are supposed to pass the event notifications along to the base class, but
// this is not implemented (none of these events currently needed).
//
// CLogEdit	-- logging edit control. Logs changes, set focus
// CLogCombo -- logging combo box control. Logs dropdown/closeup, selection changes, focus
//
#ifndef LOG_EDIT_INCLUDED	
#define LOG_EDIT_INCLUDED 1		// prevent multiple inclusion

enum Status;
//////////////////////////////////////////////////////////////////////////////////
// CLogEdit window

#include "GridCtrl.h"	// Added by ClassView

class CLogEdit : public CEdit 
{
// Construction
public:
	CLogEdit();
	DECLARE_DYNAMIC(CLogEdit)

		// Attributes
public:
	Status  GetStatus() {return m_status;}
	void	SetStatus(Status status);

	// Flag used for special gray enabled/disabled appearance
	// Note this appearance is independent of windows enabled state.
	// Processing depends on OnCtlColor handling in parent dialog.
	BOOL	GetEnabled() { return m_bEnabled; };
	void	SetEnabled(BOOL bEnable) { m_bEnabled = bEnable; Invalidate(); }

protected:
	Status  m_status;
	BOOL	m_bEnabled;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLogEdit)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CLogEdit();

	// Generated message map functions
protected:
	//{{AFX_MSG(CLogEdit)
	afx_msg BOOL OnChange();
	afx_msg BOOL OnSetfocus();
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////



//////////////////////////////////////////////////////////////////////////////////
// CLogRichEdit window
#include "RichEditEx.h"

class CLogRichEdit : public CRichEditEx
{
// Construction
public:
	CLogRichEdit();
	DECLARE_DYNAMIC(CLogRichEdit)

// Attributes
public:
	Status  GetStatus() {return m_status;}
	void	SetStatus(Status status) 
	{ 
		m_status = status; 
		UpdateColors();
	}
	// Flag used for special gray disabled appearance in tutor mode.
	// Note this appearance is independent of windows enabled state.
	BOOL	GetEnabled() { return m_bEnabled; };
	void	SetEnabled(BOOL bEnable) 
	{ 
		m_bEnabled = bEnable; 
		UpdateColors();
	}

protected:
	Status  m_status;
	BOOL	m_bEnabled;
	
	void UpdateColors()  // reset colors to reflect curent state
	{
		// flag error with red background in both enabled/disabled mode
		if (m_status == statusError)
			SetBkColor(RGB(255, 0, 0));
		else if (!m_bEnabled)	// else gray background when disabled
			SetBkColor(::GetSysColor(COLOR_3DFACE)); 
		else 
			SetSysBkColor();     // use system default when enabled

		// text color: green for correct, black for both error or unknown.
		if (m_status == statusCorrect)
			SetTextColor(RGB(0, 128, 0));
		else	
			SetTextColor(RGB(0, 0, 0));
	}

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLogRichEdit)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CLogRichEdit();

	// Generated message map functions
protected:
	//{{AFX_MSG(CLogRichEdit)
	afx_msg BOOL OnChange();
	afx_msg BOOL OnSetfocus();
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////




/////////////////////////////////////////////////////////////////////////////
// CLogCombo window

class CLogCombo : public CComboBox 
{
// Construction
public:
	CLogCombo();
	DECLARE_DYNAMIC(CLogCombo)

// Attributes
public:
	Status  GetStatus() {return m_status;}
	void	SetStatus(Status status);

	// Flag used for special gray enabled/disabled appearance
	// Note this appearance is independent of windows enabled state.
	// Processing depends on OnCtlColor handling in parent dialog.
	BOOL	GetEnabled() { return m_bEnabled; };
	void	SetEnabled(BOOL bEnable) { m_bEnabled = bEnable; Invalidate(); }

protected:
	Status  m_status;
	BOOL	m_bEnabled;

// Operations
public:
	int		SelectStringExact(CString strFind );

public:
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLogCombo)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CLogCombo();

	// Generated message map functions
protected:
	//{{AFX_MSG(CLogCombo)
	afx_msg BOOL OnEditchange();
	afx_msg BOOL OnDropdown();
	afx_msg BOOL OnSelchange();
	afx_msg BOOL OnCloseup();
	afx_msg BOOL OnSetfocus();
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

// Variant using owner-draw to render Greek text in choice items.
// Code copied from CRichCombo, which doesn't do logging.
class CLogRichCombo : public CLogCombo
{
// Construction
public:
	CLogRichCombo();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLogRichCombo)
	public:
	virtual void MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct);
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:	
	void	DrawDropList(LPDRAWITEMSTRUCT lpdis, UINT nState);
	CRect	OnDrawThisText(CDC* pDC, CRect rcItem, int itemID);

	virtual ~CLogRichCombo();

	// Generated message map functions
protected:
	//{{AFX_MSG(CLogRichCombo)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CLogList window

class CLogList : public CListBox 
{
// Construction
public:
	CLogList();
	DECLARE_DYNAMIC(CLogList)

// Attributes
public:
	Status  GetStatus() {return m_status;}
	void	SetStatus(Status status);

	// Flag used for special gray enabled/disabled appearance
	// Note this appearance is independent of windows enabled state.
	// Processing depends on OnCtlColor handling in parent dialog.
	BOOL	GetEnabled() { return m_bEnabled; };
	void	SetEnabled(BOOL bEnable) { m_bEnabled = bEnable; Invalidate(); }

protected:
	Status  m_status;
	BOOL m_bEnabled;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLogList)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CLogList();

	// for multiple selection boxes:
protected:

	enum { MAXSEL = 32 };		// max size of selection set
	int m_nSelected;			// no. of selected items on last update
	int m_Selected[MAXSEL];		// set of selected indices on last update
	
	// Generated message map functions
public:
	//{{AFX_MSG(CLogList)
	afx_msg BOOL OnSelchange();
	afx_msg BOOL OnSetfocus();
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// CLogBtn window

class CLogBtn : public CButton
{
// Construction
public:
	CLogBtn();
	DECLARE_DYNAMIC(CLogBtn)

// Attributes
public:
	Status  GetStatus() {return m_status;}
	void	SetStatus(Status status) { m_status = status; Invalidate();}

	// Flag used for special gray enabled/disabled appearance
	// Note this appearance is independent of windows enabled state.
	// Processing depends on OnCtlColor handling in parent dialog.
	BOOL	GetEnabled() { return m_bEnabled; }
	void	SetEnabled(BOOL bEnable) { m_bEnabled = bEnable; Invalidate();}

protected:
	Status  m_status;
	BOOL m_bEnabled;

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLogBtn)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CLogBtn();

	// Generated message map functions
protected:
	//{{AFX_MSG(CLogBtn)
	afx_msg BOOL OnClicked();
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
#endif LOG_EDIT_INCLUDED
