#if !defined(AFX_HINTVIEW_H__4E2F7CC2_AA22_11D1_BC04_0000C037C67D__INCLUDED_)
#define AFX_HINTVIEW_H__4E2F7CC2_AA22_11D1_BC04_0000C037C67D__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// HintView.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CHintView form view

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

// Hdr requires edit class decls to declare embedded objs (not just ptrs to objs):
#include "EQEdit.h"		// class for our hint edit control	

#include "Mainfrm.h"	// for HintType enum defined at application level

class CHintView : public CFormView, public IEventHandler
{
protected:
	CHintView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CHintView)

// Form Data
public:
	//{{AFX_DATA(CHintView)
	enum { IDD = IDD_HINTBAR };
	CButton	m_btnWhy;
	CButton	m_btnHow;
	CButton	m_btnExplMore;
	//}}AFX_DATA
	CHintRichEdit m_editHint;

// Attributes
public:
	CString m_strHint;				// latest hint msg
	CString m_strBtns;				// string coding relevant followup commands
	HintType m_hintType;			// kind of hint message, defined in Mainframe.h
	BOOL m_bInHintSequence;			// set if inside a sequence of hints/followups
 
	/* enum HintType { Hint, WhatsWrong }; */
protected:
	CFont m_fontMsg;				// larger font to use for message text.
	LPCTSTR m_pszHintSpec;			// NB pointed-to data is owned by caller

// Operations
protected:
	void ForceRequestResize();
	
public:
	void EnablePane(BOOL bEnable);
	BOOL m_bEnabled;
	CWnd* m_pPopup;
	// Hint spec points to quote delimited string consisting of hint message plus 
	// optional tilde plus list of relevant followup button letters (w, e, h).
	// May be NULL after help system RPC failure
	void SetHintSpec(LPCTSTR pszHintSpec, HintType type = Msg);
	void ProcessSpec();

	void ResizeHint();
	void ClearHint();
	void HideHint();

	int m_nHeight;
	int GetIdealHeight() {return m_nHeight;};
	int GetMarginHeight();
	int GetMinimumHeight();

	// Helper functions for handling hypertext in view
	void	PopupDef(CString term, CPoint point);
 
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHintView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

	// Log/Demo support
	virtual BOOL DispatchEvent(EventID nEvent, LPCTSTR parms);
	virtual void PointToObject(LPCTSTR pszObjID);

// Implementation
protected:
	virtual ~CHintView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CHintView)
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnBtnHint();
	afx_msg void OnUpdateBtnHint(CCmdUI* pCmdUI);
	afx_msg void OnUpdateBtnHide(CCmdUI* pCmdUI);
	afx_msg void OnBtnWhatswrong();
	afx_msg void OnUpdateBtnWhatswrong(CCmdUI* pCmdUI);
	afx_msg LRESULT OnIdleUpdateCmdUI(WPARAM, LPARAM);
	afx_msg void OnRequestresizeHint(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_HINTVIEW_H__4E2F7CC2_AA22_11D1_BC04_0000C037C67D__INCLUDED_)
