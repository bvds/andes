
#ifndef __AFX_INPLACECOMBO_H_INCLUDED__
#define __AFX_INPLACECOMBO_H_INCLUDED__

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

// InPlaceCombo.h : header file
//
//
// Written by Motty Cohen Copyright (c) 1998.
// Based on Chris Maunder's InPlaceEdit class
// 
//
// This code may be used in compiled form in any way you desire. This
// file may be redistributed unmodified by any means PROVIDING it is 
// not sold for profit without the authors written consent, and 
// providing that this notice and the authors name is included. If 
// the source code in  this file is used in any commercial application 
// then acknowledgement must be made to the author of this file 
// (in whatever form you wish).
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability for any damage/loss of business that
// this product may cause.
//
// Expect bugs!
//
// Modified by Ellen Dugan
//
/////////////////////////////////////////////////////////////////////////////
#define IDC_IPMENU 6
#define IDC_IPLIST 7
#define IPLM_FILL  WM_USER + 1000
#define ITEM_HASEXPLANATION		8

#define IS_NORMAL			0x1000
#define IS_HIGHLIGHTED		0x1001

#define LBN_TEXT			1
#define LBN_ARROW			2

class CExpLawDlg;
class CListMenu;
class CGridCtrl;
/////////////////////////////////////////////////////////////////////////////
// CInPlaceList window

class CInPlaceList : public CListBox
{
// Construction
public:
	CInPlaceList(int nRow, int nColumn,  CString sInitText);

// Attributes
public:
	CExpLawDlg* m_pDlg;
	CListMenu* m_pMenu;
	BOOL m_bHasExplanations;
	BOOL m_bHasIcons;
	BOOL m_bCancel;
	CGridCtrl* m_pParent;//this is the list's acting parent.  Although the 
	//actual parent is the view so that the window is showed outside the
	//grid's client area

protected:
	int DrawThisText(CDC* pDC, CRect rcText, int nColumn, int nItem);
	void DrawDropList(LPDRAWITEMSTRUCT lpdis, UINT nState);
	CRect OnDrawThisText(CDC* pDC, CRect rcItem, int itemID);
	


// Operations
public:
	void EndSelect();
	int HitTest(int nIndex, CPoint point);
	void ShowExplanation(int sel, BOOL bShow);
	BOOL Create(CWnd* pParent, CRect& rect, DWORD dwStyle) ;
	LV_DISPINFO m_dispinfo;
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CInPlaceList)
	public:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpdis);
	virtual void MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CInPlaceList();

	// Generated message map functions
protected:
	//{{AFX_MSG(CInPlaceList)
	afx_msg void OnNcDestroy();
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSelchange();
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	//}}AFX_MSG
	afx_msg void OnCloseup();
	DECLARE_MESSAGE_MAP()

private:
	int		m_nNumLines;
	CString m_sInitText;
	int		m_nRow;
	int		m_nCol;
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(__AFX_INPLACECOMBO_H_INCLUDED__)
