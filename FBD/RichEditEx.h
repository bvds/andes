//RichEditEx.h

#ifndef RICHEDIT_INCLUDED
#define RICHEDIT_INCLUDED 1
/////////////////////////////////////////////////////////////////////////////
//
// CRichEditEx --rich edit control extended with convenient utility methods for 
//               setting formats. Base class for most of our specialized richeds.
//
// Also handles text with symbol character tag ($).
// ANDES convention is $a for alpha, etc.
//
/////////////////////////////////////////////////////////////////////////////

class CRichEditEx : public CRichEditCtrl
{
// Construction
public:
	CRichEditEx();

// Attributes
public:

// Operations
public:
	//Toggles
	void ToggleCharSymbol();
	void ToggleCharItalic();
	void ToggleCharBold();

	virtual void SetRichEditText(CString& strEq);
	virtual void GetRichEditText(CString & strEq);
	void SetCharSymbol();
	void SetCharPlain();

	void InsertGreekText(CString strText);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRichEditEx)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CRichEditEx();

	// Generated message map functions
protected:
	//{{AFX_MSG(CRichEditEx)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
//
// CHintRichEdit -- specialized Richedit to show ANDES hypertext.
//
// Understands the ANDES hypertext tags in addition to Greek.
//////////////////////////////////////////////////////////

class CHyperLnk;//forward declaration
typedef CTypedPtrList<CObList, CHyperLnk*> CLinkList;

class CHintRichEdit : public CRichEditEx
{
// Construction
public:
	CHintRichEdit();

// Attributes
public:
	CHyperLnk* HyperAt(CPoint point);
	BOOL m_bOnHyperText;

	CLinkList m_links;	// list of hyperlinks

	CHyperLnk* FindLink(LPCTSTR pszPrefix); // get link whose text starts w/prefix

// Operations
public:
	void SetParaIndents(int lIndent, int rIndent);
	void SetFormat(char tag = NULL);

protected:
	void ClearFormat();
	void ClearLinks();
	void SetCharLinkColor(char tag);
	void SetCharUnderline();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHintRichEdit)
	//}}AFX_VIRTUAL

// Implementation
public:
	void SetRichEditText(CString & strHint, int nMargin = 500);
	CString ParseOutFmtTxt(CString str);
	virtual ~CHintRichEdit();
	CHyperLnk* GetLinkFromPos(int poschar);
#ifdef _DEBUG
	void DumpLinkPositions();
#endif	
// Generated message map functions
protected:
	//{{AFX_MSG(CHintRichEdit)
	afx_msg int OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
#endif // ! EQEDIT_INCLUDED
/////////////////////////////////////////////////////////////////////////////

