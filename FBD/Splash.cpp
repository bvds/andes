// CG: This file was added by the Splash Screen component.
// Splash.cpp : implementation file
//

#include "stdafx.h"  // e. g. stdafx.h
#include "resource.h"  // e.g. resource.h
#include "FBD.h"
#include "Splash.h"  // e.g. splash.h
#include "Picture.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
//   Splash Screen class

BOOL CSplashWnd::c_bShowSplashWnd;
CSplashWnd* CSplashWnd::c_pSplashWnd;
CSplashWnd::CSplashWnd()
{
}

CSplashWnd::~CSplashWnd()
{
	// Clear the static window pointer.
	ASSERT(c_pSplashWnd == this);
	c_pSplashWnd = NULL;

	// Added for DIB bitmap: Free the GDI palette we allocated
	if ((HPALETTE) m_palette != NULL)
		DeleteObject(m_palette);
}

BEGIN_MESSAGE_MAP(CSplashWnd, CWnd)
	//{{AFX_MSG_MAP(CSplashWnd)
	ON_WM_CREATE()
	ON_WM_PAINT()
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CSplashWnd::EnableSplashScreen(BOOL bEnable /*= TRUE*/)
{
	c_bShowSplashWnd = bEnable;
}

void CSplashWnd::ShowSplashScreen(CWnd* pParentWnd /*= NULL*/)
{
	if (!c_bShowSplashWnd || c_pSplashWnd != NULL)
		return;

	// Allocate a new splash screen, and create the window.
	c_pSplashWnd = new CSplashWnd;
	if (!c_pSplashWnd->Create(pParentWnd))
		delete c_pSplashWnd;
	else
		c_pSplashWnd->UpdateWindow();
}

BOOL CSplashWnd::PreTranslateAppMessage(MSG* pMsg)
{
	if (c_pSplashWnd == NULL)
		return FALSE;

	// If we get a keyboard or mouse message, hide the splash screen.
	if (pMsg->message == WM_KEYDOWN ||
	    pMsg->message == WM_SYSKEYDOWN ||
	    pMsg->message == WM_LBUTTONDOWN ||
	    pMsg->message == WM_RBUTTONDOWN ||
	    pMsg->message == WM_MBUTTONDOWN ||
	    pMsg->message == WM_NCLBUTTONDOWN ||
	    pMsg->message == WM_NCRBUTTONDOWN ||
	    pMsg->message == WM_NCMBUTTONDOWN)
	{
		c_pSplashWnd->HideSplashScreen();
		return TRUE;
	}
	return FALSE;
}

BOOL CSplashWnd::Create(CWnd* pParentWnd /*= NULL*/)
{
/*  Original code

	if (!m_bitmap.LoadBitmap(IDB_SPLASH))
		return FALSE;
*/

#if 0 // Code to create DIB Section to enable custom palette construction

	// Use LoadImage to set flag so as to create a DIB Section in a GDI hBITMAP.
	// See Prosise, pp821 - 823.
#ifndef ATLAS
	HBITMAP hBitmap = (HBITMAP) ::LoadImage(AfxGetInstanceHandle(),
		MAKEINTRESOURCE(IDB_SPLASH), IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
#else
	HBITMAP hBitmap = (HBITMAP) ::LoadImage(AfxGetInstanceHandle(),
		MAKEINTRESOURCE(IDB_ATLAS_SPLASH), IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION);
#endif
	if (hBitmap == NULL)
		return FALSE;

	// Attach our MFC CBitmap wrapper object to it
	m_bitmap.Attach(hBitmap);

	// Try to create our custom palette for the DIB
	CreatePalette();
#endif // DIB SECTION CODE

#if 0 // common bitmap/DIB section code
	// Create the window using the bitmap's size
	BITMAP bm;
	m_bitmap.GetBitmap(&bm);
	return CreateEx(0,
		AfxRegisterWndClass(0, AfxGetApp()->LoadStandardCursor(IDC_ARROW)),
		NULL, WS_POPUP | WS_VISIBLE, 0, 0, bm.bmWidth, bm.bmHeight, pParentWnd->GetSafeHwnd(), NULL);

#else // New CPicture based code for jpg

	if (! m_pic.Load(IDR_SPLASH_JPG)) {
		return FALSE;
	}
	CSize sizePic = m_pic.GetImageSize();
	
	return CreateEx(0,
		AfxRegisterWndClass(0, AfxGetApp()->LoadStandardCursor(IDC_ARROW)),
		NULL, WS_POPUP | WS_VISIBLE, 0, 0, sizePic.cx, sizePic.cy, pParentWnd->GetSafeHwnd(), NULL);
#endif 
}

void CSplashWnd::HideSplashScreen()
{
	// Destroy the window, and update the mainframe.
	DestroyWindow();
	AfxGetMainWnd()->UpdateWindow();
}

void CSplashWnd::PostNcDestroy() // auto-delete of C++ obj on window destruction
{
	// Free the C++ class.
	delete this;
}

int CSplashWnd::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

	// Center the window. 
	CenterWindow();

	// Set a timer to destroy the splash screen.
	SetTimer(1, 2000, NULL);
	return 0;
}

void CSplashWnd::OnPaint()
{
	CPaintDC dc(this);
#if 0 
	// Create memory DC to hold the bitmap
	CDC dcImage;
	if (!dcImage.CreateCompatibleDC(&dc))
		return;
	CBitmap* pOldBitmap = dcImage.SelectObject(&m_bitmap);

	//Here we are painting the current version number on the splash screen
	CFont font;
    font.CreateFont(15,0,0,0,FW_DONTCARE,0,0,0,ANSI_CHARSET,0,0,0,
    		VARIABLE_PITCH|FF_DONTCARE,"Times New Roman");
    CFont* pOldFont = dcImage.SelectObject(&font);
	dcImage.SetBkColor(RGB(192,192,192));

	CRect rect = CRect(0,0,0,0);
	dcImage.DrawText(theApp.m_strAndesVersion, rect, DT_CALCRECT);
#ifndef ATLAS
	// rect.OffsetRect(169, 246); // splsh16.bmp
	rect.OffsetRect(140,265);
#else
	rect.OffsetRect(355, 64);
#endif
	dcImage.DrawText(theApp.m_strAndesVersion, rect, DT_LEFT);
	dcImage.SelectObject(pOldFont);

	// Select and realize our custom palette 
	CPalette* pOldPalette;
	if ((HPALETTE) m_palette != NULL) // non-NULL handle => palette creation succeeded
	{
		pOldPalette = dc.SelectPalette(&m_palette, FALSE);
		dc.RealizePalette();
	}

	// Blit the image from memory DC to screen
	BITMAP bm;
	m_bitmap.GetBitmap(&bm);
	dc.BitBlt(0, 0, bm.bmWidth, bm.bmHeight, &dcImage, 0, 0, SRCCOPY);
	dcImage.SelectObject(pOldBitmap);

	// Restore old palette to dc
	if ((HPALETTE) m_palette != NULL)
		dc.SelectPalette(pOldPalette, FALSE); 

#else // New CPicture-based code to use jpg image
	
	// draw the jpg image
	m_pic.Render(&dc);

    // Paint the current version number on the splash screen,
	// at location determined experimentally for image.
	CFont font;
    font.CreateFont(15,0,0,0,FW_DONTCARE,0,0,0,ANSI_CHARSET,0,0,0,
    		VARIABLE_PITCH|FF_DONTCARE,"Times New Roman");
    CFont* pOldFont = dc.SelectObject(&font);
	dc.SetBkColor(RGB(192,192,192)); // more robust to draw transparently!

	CRect rect = CRect(0,0,0,0);
	dc.DrawText(theApp.m_strAndesVersion, rect, DT_CALCRECT);
	rect.OffsetRect(140, 265);
	dc.DrawText(theApp.m_strAndesVersion, rect, DT_LEFT);
	dc.SelectObject(pOldFont);
#endif 
}

void CSplashWnd::OnTimer(UINT nIDEvent)
{
	// Destroy the splash screen window.
	HideSplashScreen();
}

// Create custom palette for our DIB Section bitmap.
// Uses m_bitmap, sets m_palette
// Code taken from Prosise book, pp 821- 822.
void CSplashWnd::CreatePalette()
{
	// if palettes aren't supported, return and do nothing
	CClientDC dc (AfxGetMainWnd());
	if((dc.GetDeviceCaps (RASTERCAPS) & RC_PALETTE) == 0) return;        
	
	// Create a palette to go with the DIB section 
	if ((HBITMAP) m_bitmap != NULL)				// have a non-NULL bitmap handle
	{
		DIBSECTION ds;
		m_bitmap.GetObject (sizeof (DIBSECTION),&ds);
		int iNumberOfColors;
		if (ds.dsBmih.biClrUsed != 0)
			iNumberOfColors = ds.dsBmih.biClrUsed;
		else
			iNumberOfColors = 1 << ds.dsBmih.biBitCount;
		
		if (iNumberOfColors > 256) // too many colors, create a halftone palette
		{
			m_palette.CreateHalftonePalette (&dc);
		}
		else// create a custom palette from the DIB section's own color table  
		{
			// Allocate RGBQUAD table to hold copy of DIB's color info
			RGBQUAD * pRGBQuad = new RGBQUAD[iNumberOfColors];
			
			// Create memDC around DIB and extract color table into pRGBQUAD
			CDC memDC;
			memDC.CreateCompatibleDC (&dc);
			CBitmap * pOldBitmap = memDC.SelectObject (&m_bitmap);
			::GetDIBColorTable ((HDC) memDC, 0, iNumberOfColors, pRGBQuad);
			memDC.SelectObject (pOldBitmap);
			
			// Allocate a LOGPALETTE struct to represent our custom palette
			UINT nSize = sizeof (LOGPALETTE) + 
						(sizeof (PALETTEENTRY) * (iNumberOfColors - 1));
			LOGPALETTE * pLogPalette = (LOGPALETTE *) new BYTE[nSize];
			pLogPalette->palVersion = 0x300;// fixed magic number
			pLogPalette->palNumEntries = (WORD)iNumberOfColors;			
			
			// copy color entries from DIB's RGBQUAD table into our LOGPALETTE
			for (int i = 0; i < iNumberOfColors; i++) 
			{
				pLogPalette->palPalEntry[i].peRed = pRGBQuad[i].rgbRed;
				pLogPalette->palPalEntry[i].peGreen = pRGBQuad[i].rgbGreen;
				pLogPalette->palPalEntry[i].peBlue = pRGBQuad[i].rgbBlue;
				pLogPalette->palPalEntry[i].peFlags = 0;
			}   
			
			// create our GDI Palette object from LOGPALETTE structure
			m_palette.CreatePalette (pLogPalette);
			
			// clean up
			delete[] pLogPalette;
			delete[] pRGBQuad;
		}
	}
}
