// RichCombo.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "RichCombo.h"
#include "GreekOpts.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CRichCombo

CRichCombo::CRichCombo()
{
}

CRichCombo::~CRichCombo()
{
}


BEGIN_MESSAGE_MAP(CRichCombo, CComboBox)
	//{{AFX_MSG_MAP(CRichCombo)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRichCombo message handlers

void CRichCombo::MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct) 
{
	lpMeasureItemStruct->itemHeight = 16;
	
}

void CRichCombo::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	if (lpDrawItemStruct->itemID >= 20)
		return;

	BOOL bFocus = (lpDrawItemStruct->itemAction & ODA_FOCUS);
	BOOL bSelected = (lpDrawItemStruct->itemState & ODS_SELECTED);

	if (bSelected)
		DrawDropList(lpDrawItemStruct, IS_HIGHLIGHTED);
	else
		DrawDropList(lpDrawItemStruct, IS_NORMAL);	
}

void CRichCombo::DrawDropList(LPDRAWITEMSTRUCT lpdis, UINT nState)
{
	CDC* pDC = CDC::FromHandle(lpdis->hDC);
	CRect rcItem(lpdis->rcItem);

	switch (nState){
	case IS_HIGHLIGHTED:
		pDC->SetTextColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
		pDC->SetBkColor(::GetSysColor(COLOR_HIGHLIGHT));
		pDC->FillRect(&lpdis->rcItem, &CBrush(::GetSysColor(COLOR_HIGHLIGHT)) );
		if (lpdis->itemID >=0)
			OnDrawThisText(pDC, rcItem, lpdis->itemID);
		pDC->DrawFocusRect(&rcItem);

		break;

	case IS_NORMAL:
		pDC->SetTextColor(::GetSysColor(COLOR_WINDOWTEXT));
		pDC->SetBkColor(::GetSysColor(COLOR_WINDOW));
		pDC->FillRect(&lpdis->rcItem, &CBrush(::GetSysColor(COLOR_WINDOW)) );

		if (lpdis->itemID >=0)
			OnDrawThisText(pDC, rcItem, lpdis->itemID);

		break;
	}
}

CRect CRichCombo::OnDrawThisText(CDC* pDC, CRect rcItem, int itemID)
{
	CRect rcText = rcItem;
	CRect rcBorder = rcItem;
	rcText.SetRect(rcText.left+3, rcText.top, rcText.right-3, rcText.bottom);
	rcBorder.SetRect(rcItem.left+1, rcItem.top, rcItem.right-1, rcItem.bottom);

	CString str;
	GetLBText(itemID, str);
	return CGreekText::DrawText(pDC, rcText, str);

}

/////////////////////////////////////////////////////////////////////////////
// CRichStatic

CRichStatic::CRichStatic()
{
}

CRichStatic::~CRichStatic()
{
}


BEGIN_MESSAGE_MAP(CRichStatic, CStatic)
	//{{AFX_MSG_MAP(CRichStatic)
	ON_WM_PAINT()
	ON_WM_ERASEBKGND()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRichStatic message handlers

void CRichStatic::OnPaint() 
{
	CPaintDC dc(this); // device context for painting

	// set text drawing attributes: 
	// make sure to use parent dialog's font, since current font may be bold
	CFont* pOldFont = dc.SelectObject(GetParent()->GetFont());
	COLORREF oldBkColor = dc.SetBkColor(::GetSysColor(COLOR_3DFACE));
	COLORREF oldTextColor = dc.SetTextColor(::GetSysColor(COLOR_BTNTEXT));
	int oldAlign = dc.SetTextAlign(TA_LEFT);

	TEXTMETRIC tm;
	dc.GetTextMetrics(&tm);
	int nHeight = tm.tmHeight + tm.tmExternalLeading;

	CRect rcDraw;
	GetClientRect(rcDraw);
	
	// Break text into lines and render each into remaining drawing area
	CString str = m_strText;
	// GetWindowText(str);
	int nBreak = str.Find("\n");
	while (nBreak != -1)
	{
		CString strLine = str.Left(nBreak);
		CString strRest = str.Right(str.GetLength() - nBreak - 1);
		CRect rcUsed = CGreekText::DrawText(&dc, rcDraw, strLine);
		rcDraw += CSize(0, rcUsed.Height());
		str = strRest;
		nBreak = str.Find("\n");
	}
	CGreekText::DrawText(&dc, rcDraw, str);

	// restore previous drawing attributes
	dc.SelectObject(pOldFont);
	dc.SetBkColor(oldBkColor);
	dc.SetTextColor(oldTextColor);
	dc.SetTextAlign(oldAlign);
}

void CRichStatic::SetRichText(CString &strTaggedText)
{
	// !! Not set as window text so can't be retrieved by GetWindowText
	m_strText = strTaggedText;	
	Invalidate(TRUE);
	UpdateWindow();
}

CString CRichStatic::GetRichText()
{
	return m_strText;
}

BOOL CRichStatic::OnEraseBkgnd(CDC* pDC) 
{
	CRect rcClient;
	GetClientRect(rcClient);
	pDC->FillSolidRect(rcClient, ::GetSysColor(COLOR_3DFACE));
	
	// return CStatic::OnEraseBkgnd(pDC);
	return TRUE;
}
