#if !defined(AFX_CHATVIEW_H__31A8938A_77D0_11D2_AE5C_A42528598C74__INCLUDED_)
#define AFX_CHATVIEW_H__31A8938A_77D0_11D2_AE5C_A42528598C74__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ChatView.h : header file
//

#ifndef __AFXRICH_H__
#include <afxrich.h>
#endif

#include "Mainfrm.h"
#include "history.h"

/////////////////////////////////////////////////////////////////////////////
// CChatView view

class CChatView : public CRichEditView, public IEventHandler
{
protected: // create from serialization only
	CChatView();
	DECLARE_DYNCREATE(CChatView)

// Attributes
public:
	// current hint state:
	BOOL m_bInHintSequence;
	CString m_strBtns;

protected:
	CString m_strPrompt;
	CString m_strSysPrompt;
	long m_lInputStartIndex;		// where current student message starts
	long m_lCurMsgStartIndex;		// where most recent tutor message starts
	BOOL m_bInputAllowed;			// true if student can type msg now.
	BOOL m_bOnSysReplyLine;			// cursor on system line awaiting reply.
	BOOL m_bEnabled;				// true if whole view is enabled.

// Operations
public:
	void AddText(LPCTSTR);
	void Clear();
	CString GetLastInputStr();
	void AddSystemText(LPCTSTR pszHint = NULL, HintType = Msg);
	
	// Flush typing to log, needed in WOZ case:
	void LogUnsentText(EventID nEvent = EV_UNSENT_CONTENTS);

	// Show popup definition, used by hyperlinks.
	void PopupDef(CString term, CPoint point);// show def in popup wnd at point.

	// Enable/Disable whole pane
	void EnablePane(BOOL bEnable);

	int GetIdealHeight();
	void ScrollToCurrentMsg();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChatView)
	public:
	virtual void OnInitialUpdate();
	virtual BOOL IsSelected(const CObject* pDocItem) const;
	protected:
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	//}}AFX_VIRTUAL

	// Log/demo support
	virtual BOOL DispatchEvent(EventID, LPCTSTR);
	virtual void PointToObject(LPCTSTR) {};

// Implementation
public:
	void DeleteLinks();
	CPoint GetMenuPos();
	void GetQuantityType();
	void GetQuantityDef(int nResult);
	void GetPrinciple();
	void GetEquation(BOOL bSubmit = TRUE);
	void GetMenuSelection(CString& strMenu);
	void SubmitMenuSelection(CString strChosen);
	void DismissMsgMode();
	virtual ~CChatView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	void SetInputLine(LPCTSTR pszText);
	CString GetLastInputLine();		// Get current user line without processing it
	CString m_strLoggedLineState;	// User input line as last logged, for WOZ logging
	
	CString ParseOutFormatText(CString);
	void ClearFormat();
	void SetFormat(char);
	void SetCharLinkColor(char);
	void SetCharPlain();
	void SetCharSymbol();
	void SetCharUnderline();
	void SetParaIndents(int, int);
	void ToggleCharBold();
	void ToggleCharItalic();
	void ToggleCharSymbol();
	void InsertGreekText(CString strText);

	// Hyperlink support (duplicated from HintRichEdit)
	CLinkList m_links;						// list of contained hyperlinks
	BOOL m_bOnHyperText;					// set on mouse move if over hyperlink
	CHyperLnk* HyperAt(CPoint point);		// find link given point
	CHyperLnk* GetLinkFromPos(int poschar); // find link given char pos

	// Generated message map functions
	//{{AFX_MSG(CChatView)
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnProtected(NMHDR*, LRESULT*);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnUpdateEditCopy(CCmdUI* pCmdUI);
	afx_msg void OnEditCopy();
	//}}AFX_MSG
	afx_msg BOOL OnKillFocus();
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CHATVIEW_H__31A8938A_77D0_11D2_AE5C_A42528598C74__INCLUDED_)
