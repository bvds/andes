//
// CEQEdit window -- our subclassed edit control for Equation editing.
// While experimenting, we have two versions, one derived from normal
// edit control, another which uses a Rich Edit control derivative.
//
#ifndef EQEDIT_INCLUDED
#define EQEDIT_INCLUDED 1

class CEQEdit : public CEdit
{
// Construction
public:
	
	CEQEdit();

// Attributes
public:
	void SetTextColor(COLORREF color);
	void SetBkColor(COLORREF color);
	CString m_strId;
	int m_nMenuId;		// client-settable context menu id

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEQEdit)
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
protected:
	COLORREF m_colorText;
	COLORREF m_colorBkgnd;
	CBrush	 m_brBkgnd;
public:
	virtual ~CEQEdit();

	// Generated message map functions
	//{{AFX_MSG(CEQEdit)
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg HBRUSH CtlColor(CDC* pDC, UINT nCtlColor);
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// CEQRichEdit window
#include "LogEdit.h"

class CEQRichEdit : public CLogRichEdit
{
// Construction
public:

	CEQRichEdit();
	DECLARE_DYNAMIC(CEQRichEdit)

// Attributes
public:
	void SetTextColor(COLORREF color);
	void SetBkColor(COLORREF color) { SetBackgroundColor(FALSE, color); }
	void SetSysBkColor() { SetBackgroundColor(TRUE, 0); }
	
	CString m_strId;	// string object id set by client
	BOOL m_bParentMenu;  // set to forward WM_CONTEXTMENU to parent (default FALSE)
	int m_nMenuId;		// if !bParentMenu: custom context menu resource id to use 
	
protected:

	COLORREF m_colorText;
	BOOL	 m_bFormatChange;

	// Operations
public:
//	void SetRichEditText(CString& strEq);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEQRichEdit)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
protected:
	void SetCharNoSymbol();

public:
	virtual ~CEQRichEdit();

	// Generated message map functions
protected:
	//{{AFX_MSG(CEQRichEdit)
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg int OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message);
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg BOOL OnChange();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	afx_msg void OnProtected(NMHDR*, LRESULT* pResult);
	DECLARE_MESSAGE_MAP()
};


class CLabelRichEdit : public CEQRichEdit
{
// Construction
public:

   CLabelRichEdit();
   DECLARE_DYNAMIC(CLabelRichEdit)

// Attributes
public:

// Operations
public:
	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLabelRichEdit)
	//}}AFX_VIRTUAL

// Implementation
public:
    virtual ~CLabelRichEdit();

	void SetRichEditText(CString str);


    void SetPrefix(CString strPrefix); 

	CHARRANGE m_chrgPrefix;

// Implementation
protected:

private:
    CString m_strPrefix;
	BOOL m_bSettingPrefix;

	int GetLeftWord(int nBeg);
	// static initialization functions

    // Generated message map functions
protected:
    //{{AFX_MSG(CLabelRichEdit)
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	//}}AFX_MSG
	afx_msg void OnMsgFilter(NMHDR*, LRESULT* pResult);
	afx_msg void OnPrefix(NMHDR*, LRESULT* pResult);
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
#endif // ! EQEDIT_INCLUDED
/////////////////////////////////////////////////////////////////////////////

