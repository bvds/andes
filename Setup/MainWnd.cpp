// MainWnd.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "MainWnd.h"
#include "Picture.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainWnd

IMPLEMENT_DYNCREATE(CMainWnd, CFrameWnd)

CMainWnd::CMainWnd()
{
	m_text = "Andes Physics Tutor";
    m_nHeight = 50;
    m_bBold = TRUE;
    m_bItalic = TRUE;

    Create (NULL, "Setup", WS_OVERLAPPEDWINDOW, rectDefault, NULL,
        NULL, 0, NULL);
}

CMainWnd::~CMainWnd()
{
}


BEGIN_MESSAGE_MAP(CMainWnd, CFrameWnd)
	//{{AFX_MSG_MAP(CMainWnd)
	ON_WM_PAINT()
	ON_WM_ERASEBKGND()
	ON_WM_CREATE()
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL CMainWnd::OnEraseBkgnd (CDC* pDC)
{
   CRect rect;
   GetClientRect (&rect);

   // DoGradientFill (pDC, &rect) ;
   OnPaint(); 
   return TRUE;
}

void CMainWnd::OnPaint ()
{
    CRect rect;
    GetClientRect (&rect);
	CDC* cdc = GetDC();

    CPaintDC dc (this);


#if 0
	BITMAP bm;
	m_bmpAndes.GetObject(sizeof(BITMAP), &bm);
	CPoint size(bm.bmWidth, bm.bmHeight);
	dc.DPtoLP(&size);

	CPoint org(0,0);
	dc.DPtoLP(&org);

//	CDC* cdc = GetDC();
	CDC dcMem;
	dcMem.CreateCompatibleDC(cdc);
	CBitmap* pOldBitmap = (CBitmap*)dcMem.SelectObject(m_bmpAndes);
	dcMem.SetMapMode(dc.GetMapMode());

	int cx = (rect.Width()-bm.bmWidth) /2;
	int cy = (rect.Height()-bm.bmHeight) - 16;
    DoDrawText (&dc, &rect);
	dc.BitBlt(cx, cy, size.x, size.y, &dcMem, org.x, org.y, SRCCOPY); 
#else
	CPicture pic;
	if (! pic.Load(IDR_COVER)) {
		return;
	}
	CSize sizePic = pic.GetImageSize();
	// 10% * margin on each side
	int cxMargin = .10 * rect.Width();
	int cyMargin = .10 * rect.Height();
	CRect rcImage = CRect(cxMargin, cyMargin, rect.Width() - cxMargin, rect.Height()-cyMargin);
	pic.Render(&dc, rect);
#endif 

}

//void CMainWindow::OnOptionsExit ()
//{
//   SendMessage (WM_CLOSE, 0, 0);
//}

void CMainWnd::DoGradientFill (CDC* pDC, CRect* pRect)
{
    CBrush* pBrush[64];
    for (int i=0; i<64; i++) {
        pBrush[i] = new CBrush (RGB (0, 0, 255 - (i * 4)));
    }

    int nWidth = pRect->Width ();
    int nHeight = pRect->Height ();
    CRect rect;

    for (i=0; i<nHeight; i++) {
        rect.SetRect (0, i, nWidth, i + 1);
        pDC->FillRect (&rect, pBrush[(i * 63) / nHeight]);
    }

    for (i=0; i<64; i++)
        delete pBrush[i];
}

void CMainWnd::DoDrawText (CDC* pDC, CRect* pRect)
{
    CFont font;
    int nHeight = -((pDC->GetDeviceCaps (LOGPIXELSY) * m_nHeight) / 72);

    font.CreateFont (nHeight, 0, 0, 0, m_bBold ? FW_BOLD : FW_NORMAL,
        m_bItalic, 0, 0, DEFAULT_CHARSET, OUT_CHARACTER_PRECIS,
        CLIP_CHARACTER_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH |
        FF_DONTCARE, "Times New Roman");

    pDC->SetBkMode (TRANSPARENT);
    pDC->SetTextColor (RGB (255, 255, 255));

    CFont* pOldFont = pDC->SelectObject (&font);
    pDC->DrawText (m_text, -1, pRect, DT_SINGLELINE | DT_LEFT |
        DT_TOP);

    pDC->SelectObject (pOldFont);
}




int CMainWnd::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	
//	m_bmpAndes.LoadBitmap(IDB_ANDES);

	return 0;
}

