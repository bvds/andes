#if !defined(AFX_RICHCOMBO_H__A99F5D40_EC53_11D2_B260_0000C5465DC1__INCLUDED_)
#define AFX_RICHCOMBO_H__A99F5D40_EC53_11D2_B260_0000C5465DC1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// RichCombo.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CRichCombo window
#define IS_NORMAL			0x1000
#define IS_HIGHLIGHTED		0x1001

class CRichCombo : public CComboBox
{
// Construction
public:
	CRichCombo();

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRichCombo)
	public:
	virtual void MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct);
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	//}}AFX_VIRTUAL

// Implementation
public:
	void	DrawDropList(LPDRAWITEMSTRUCT lpdis, UINT nState);
	CRect	OnDrawThisText(CDC* pDC, CRect rcItem, int itemID);
//	int     DrawMyText(CDC* pDC, CString str, CRect rcText, UINT nType);
	CFont	m_fntGreek;
	virtual ~CRichCombo();

	// Generated message map functions
protected:
	//{{AFX_MSG(CRichCombo)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

class CRichStatic : public CStatic
{
// Construction
public:
	CRichStatic();

	// Implementation
public:
	CString GetRichText();
	void SetRichText(CString& strTaggedText);
	virtual ~CRichStatic();

	// Generated message map functions
protected:
	CString m_strText;
	//{{AFX_MSG(CRichStatic)
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};
/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_RICHCOMBO_H__A99F5D40_EC53_11D2_B260_0000C5465DC1__INCLUDED_)
