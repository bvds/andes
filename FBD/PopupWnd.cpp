// PopupWnd.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "PopupWnd.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPopupWnd

CPopupWnd::CPopupWnd()
{
}

CPopupWnd::~CPopupWnd()
{
}


BEGIN_MESSAGE_MAP(CPopupWnd, CWnd)
	//{{AFX_MSG_MAP(CPopupWnd)
	ON_WM_SHOWWINDOW()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
	ON_MESSAGE(WM_KILLPOPUP, OnKillPopup)
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CPopupWnd message handlers

BOOL CPopupWnd::Create(CWnd* pParentWnd, CPoint pos) 
{
	

	pos.x = pos.x + 15;
	pos.y = pos.y + 15;
	//tool window prevents taskbar/alt-tab
	LPCTSTR classname = AfxRegisterWndClass( 0, AfxGetApp()->LoadStandardCursor(IDC_ARROW) );
	BOOL bResult = CreateEx(WS_EX_TOOLWINDOW | WS_EX_TOPMOST, classname,
		NULL, WS_POPUP| WS_VISIBLE , pos.x, pos.y, 50, 100, NULL, NULL);
	
	if (bResult)
		SetOwner(pParentWnd);

	return bResult;
}

BOOL CPopupWnd::PreTranslateMessage(MSG* pMsg) 
{
	if (  (pMsg->message > WM_MOUSEFIRST && pMsg->message <= WM_MOUSELAST)  
			|| (pMsg->message >= WM_NCLBUTTONDOWN && pMsg->message <= WM_NCMBUTTONDBLCLK)
			|| (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST) ) 
	{
		if (pMsg->message != WM_LBUTTONUP)
		{
			LogEventf(EV_KILL_POPUP, "");  // changed to be parameterless -- AW
			ReleaseCapture();
			DestroyWindow();
			return TRUE;
		}
	}
	return CWnd::PreTranslateMessage(pMsg);
}


void CPopupWnd::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CWnd::OnShowWindow(bShow, nStatus);
	
	if (bShow)	{
		PostThreadMessage(GetCurrentThreadId(), WM_POPUPDEF, 0, (LPARAM)m_hWnd);
		SetCapture();
		int stcWidth = 200 + SHADOW_WIDTH;
		CClientDC dc(this);
		CSize size = dc.GetTextExtent(m_strDef);
		int height = size.cy;//Add 14 to cx to make up for margins
		int nRows = ((size.cx + 14) / (stcWidth)) + 3;//Add two rows
		//we need a buffer for the fact that words are broken up onto lines and 
		//not max text on each line.
		height = height * nRows + SHADOW_HEIGHT;//for top/bottom margins
		SetWindowPos(NULL, 0, 0, stcWidth, height, SWP_NOZORDER|SWP_NOMOVE);	

	}
}

void CPopupWnd::OnPaint() 
{
	CPaintDC dc(this); // device context for painting
	CRect rect;
	GetClientRect(rect);

	CPen* pPen = new CPen();
	pPen->CreatePen(PS_SOLID, 2, ::GetSysColor(COLOR_WINDOWFRAME));
	CPen* pOldPen = dc.SelectObject(pPen);
	dc.Rectangle(rect.left + 1, rect.top + 1, 
		rect.right - SHADOW_WIDTH, rect.bottom - SHADOW_HEIGHT);
	dc.SelectObject(pOldPen);
	delete pPen;

	CFont* pFont = dc.GetCurrentFont();
	CFont* pOldFont = dc.SelectObject(pFont);
    // Draw text in transparent mode
    int oldBkMode = dc.SetBkMode(TRANSPARENT);
	CRect rcText;
	rcText.left = rect.left + 7;
	rcText.top = rect.top + 7;
	rcText.right = rect.right - SHADOW_WIDTH - 7 ;
	rcText.bottom = rect.bottom - SHADOW_HEIGHT - 7;
    dc.DrawText(m_strDef, rcText, DT_WORDBREAK);
    dc.SetBkMode(oldBkMode);
	dc.SelectObject(pOldFont);

	int aPattern[] = {0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55};
	CBitmap* pBM = new CBitmap();
	pBM->CreateBitmap(8, 8, 1, 1, aPattern);
	CBrush* pBrush = new CBrush();
	pBrush->CreatePatternBrush(pBM);
	pBrush->UnrealizeObject();
	CBrush* pOldBrush = dc.SelectObject(pBrush);
	dc.PatBlt(rect.left + SHADOW_OFFSET, rect.bottom - SHADOW_HEIGHT,
		rect.right - SHADOW_OFFSET, SHADOW_HEIGHT, 0xA000C9);
	dc.PatBlt(rect.right - SHADOW_WIDTH, rect.top + SHADOW_OFFSET,
		SHADOW_WIDTH, rect.bottom, 0xA000C9);
	dc.SelectObject(pOldBrush);
	delete pBM;
	delete pBrush;

	 	
}



void CPopupWnd::PostNcDestroy() 
{
	delete this;
	
	CWnd::PostNcDestroy();
}

// Replay logged events.
BOOL CPopupWnd::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{
//	switch (nEvent)
//	{
//	case EV_KILL_POPUP:
//		{
//			ReleaseCapture();
//			DestroyWindow();
//		}
//		break;
//	default:	// just ignore unknown codes.
//		TRACE("HintView dispatch: unknown event %d, ignored\n", nEvent);
//		break;
//	}
	return TRUE;
}

LRESULT CPopupWnd::OnKillPopup(WPARAM msg, LPARAM hwnd)
{
	ReleaseCapture();
	DestroyWindow();
	return LRESULT(0);
}
