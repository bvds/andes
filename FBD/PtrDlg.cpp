// PtrDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "fbddoc.h"			// !!! must pull in document and all drawobj defs just 
							// to enable CVector arrow utilities -- fix!
#include "fbdobj.h"			// for CVector arrow utils 
#include "PtrDlg.h"
#include <math.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPtrWnd dialog


CPtrWnd::CPtrWnd()	
{
}

BEGIN_MESSAGE_MAP(CPtrWnd, CWnd)
	//{{AFX_MSG_MAP(CPtrWnd)
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_SHOWWINDOW()
	ON_WM_MOVE()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPtrWnd message handlers

enum { PtrColor = RGB(255, 0, 255) };	// MAGENTA
int CPtrWnd::PolySizes[2] ={5, 4};// 5-pt arrowhead then shaft rectangle

BOOL CPtrWnd::Create()
{
	CClientDC dc(NULL);
	int cxSide = dc.GetDeviceCaps(LOGPIXELSX)/2; // size is 1/2 logical inch per side

	// use WNDCLASS w/default attributes (MFC registers on first use, then reuses)
	// TOOLWINDOW prevents taskbar/alt-tab. Note created invisible
	BOOL bSuccess = CreateEx(WS_EX_TOOLWINDOW | WS_EX_TOPMOST, AfxRegisterWndClass(NULL),
		/*name*/ NULL, /*dwStyle*/ WS_POPUP, /*pos*/ CRect(0, 0, cxSide, cxSide), 
		/*Parent*/ AfxGetMainWnd(), /*IDorMenu*/ NULL);

	return bSuccess;
}

int CPtrWnd::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	// Set custom clipping region for the whole window to give appearance of an
	// irregularly shaped object on the screen. Window region coords are relative to
	// window origin, not client-area origin. But this window should be borderless.
	CRect rcWnd;
	GetWindowRect(rcWnd);
	
	// ensure window is square, neatly divisible into 8X8 units for math below
	int nSide = max(rcWnd.Width(), rcWnd.Height());
	nSide += nSide % 8;  
	MoveWindow(rcWnd.left, rcWnd.top, nSide, nSide, FALSE);
	GetWindowRect(rcWnd);		// update with new size for calcs below
	
	// define arrow-shaped region:
/*	// use CVector helpers to define an arrow -- not good
	int shaftWidth = rcWnd.Width()/4;
	CPoint ptEndShaft;
	CVector::CalcArrowPts(rcWnd.BottomRight(), rcWnd.TopLeft(), shaftWidth,
						  ptEndShaft, &m_pts[0]);
	CVector::CalcLinePts(rcWnd.BottomRight(), ptEndShaft, shaftWidth, &m_pts[3]); */

	// arrowhead 5-pointed polygon at beginning of array of points
	m_pts[0] = CPoint(rcWnd.left + nSide/4, rcWnd.bottom - nSide/8);
	m_pts[1] = CPoint(rcWnd.left, rcWnd.top);
	m_pts[2] = CPoint(rcWnd.right - nSide/8, rcWnd.top + nSide/4);
	m_pts[3] = CPoint(rcWnd.right - 3*nSide/8, rcWnd.top + 3*nSide/8); 
	m_pts[4] = CPoint(rcWnd.left + 3*nSide/8, rcWnd.bottom - 3*nSide/8);
	// arrow shaft rectangle in next four points
	m_pts[5] = m_pts[3];
	m_pts[6] = m_pts[4];
	m_pts[7] = CPoint(rcWnd.right - nSide/4, rcWnd.bottom);
	m_pts[8] = CPoint(rcWnd.right, rcWnd.bottom - nSide/4);

	m_dir = UpLeft;		// initial direction of pointer
	
	// create appropriate GDI region as union of shaft and arrowhead polygons
	CRgn rgn;
	rgn.CreatePolyPolygonRgn(m_pts, PolySizes, /* nPolys:*/ 2, ALTERNATE);
	
	// Following call passes ownership of the region object to Windows. 
	SetWindowRgn((HRGN) rgn.Detach(), TRUE);

	// allocate our custom background brush
	// Possible for this same C++ object to be reused across multiple Windows 
	// create/init/destroy cycles. Brush we allocate is deleted OnDestroyWindow.. 
	m_bkBrush.CreateSolidBrush(PtrColor);

	// create the message balloon window. It is created invisibly (from template).
	// as is the ptrdlg itself. Client will control visibility of ptr, and we
	// control visibility of message window.
	m_wndMsg.Create(this);
	// Place it beside our current position. (Good practice, although by default
	// MFC moves dialogs after InitDialog to center them before they are shown.)
	PlaceMsg();

	// and move to center of frame
	CenterWindow(AfxGetMainWnd());

	return 0;
}

void CPtrWnd::FlipLeftRight()
{
	CRect rcWnd;
	GetWindowRect(rcWnd);
	int Width = rcWnd.Width();

	for (int i = 0; i < ARROWPTS; i++) {
		m_pts[i].x = Width - m_pts[i].x;
	}
	m_dir = (PtrDir) ! int(m_dir); // toggle one-bit value

	// create appropriate GDI region as union of shaft and arrowhead polygons
	CRgn rgn;
	rgn.CreatePolyPolygonRgn(m_pts, PolySizes, /* nPolys:*/ 2, ALTERNATE);
	// Following call passes ownership of the region object to Windows. 
	SetWindowRgn((HRGN) rgn.Detach(), TRUE);

	Invalidate(FALSE);
}

void CPtrWnd::SetDir(PtrDir dir)
{
	if (dir == m_dir) return;
	// else flip left-right direction 
	FlipLeftRight();
}


void CPtrWnd::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CWnd::OnShowWindow(bShow, nStatus);

	// if going invisible, hide message window with us
	if (! bShow && m_wndMsg.GetSafeHwnd())
		m_wndMsg.ShowWindow(SW_HIDE);
	// if showing, ensure message window is correctly placed.
	if (bShow)
		PlaceMsg();
}

#if 0	// no way to close it now
void CPtrWnd::OnClose() // user closing the window
{
	// treat this as "Stop" command on Demo.
	// Send command to mainframe to stop demo player
	AfxGetMainWnd()->SendMessage(WM_COMMAND, MAKEWPARAM(ID_PLAYER_STOP, 0), 0); 
	// will that command handler manage hiding of window?
	
	CWnd::OnClose();
	/* dangerous:
	DestroyWindow(); */
	// just hide us:?
	m_wndMsg.ShowWindow(SW_HIDE);
	ShowWindow(SW_HIDE);
}
#endif 0

void CPtrWnd::OnDestroy() 
{
	m_bkBrush.DeleteObject();	// free custom brush we created
	
	CWnd::OnDestroy();
}


// move window by steps to screen location, animating the process.
void CPtrWnd::MoveTo(int xNew, int yNew) // all in screen coords of new top left
{
	// if msg balloon is showing, hide it during the move, saving state flag.
	BOOL bMsgVisible = m_wndMsg.IsWindowVisible();
	if (bMsgVisible) {
		m_wndMsg.ShowWindow(SW_HIDE);
		m_wndMsg.UpdateWindow();
	}

	// Move the ptr window to dest through a series of intermediate steps
	CRect rcWnd;
	GetWindowRect(rcWnd);
	int cxWnd = rcWnd.Width();	// w,h for MoveWindow below
	int cyWnd = rcWnd.Height();

	// if pointing rightward, adjust xNew to hold dst of top-left corner
	if (m_dir == UpRight)
		xNew -= cxWnd;

	int dx = xNew - rcWnd.left;
	int dy = yNew - rcWnd.top;
	int distance = (int)sqrt(dx * dx + dy * dy);
	
	int nSteps = distance/16;	// = 6 steps an inch at 96dpi
	int cxStep, cyStep;
	if (nSteps > 0) {			// may be rounded to 0 -- don't divide by it!
		cxStep = dx/nSteps;
		cyStep = dy/nSteps;
	}
	
	for (int i = 1; i <= nSteps - 1; i++) { // final step is outside loop
		rcWnd.left += cxStep;
		rcWnd.top += cyStep;
		MoveWindow(rcWnd.left, rcWnd.top, cxWnd, cyWnd);
	}
	// last step places at exact destination.
	MoveWindow(xNew, yNew, cxWnd, cyWnd);

	// if msg balloon was up at start, move and show it
	if (bMsgVisible) {
		PlaceMsg();
		m_wndMsg.ShowWindow(SW_SHOWNA);
	}
}

void CPtrWnd::OnMove(int x, int y) 
{
	CWnd::OnMove(x, y);
	
	// If msg is showing, ensure it comes along after moves of this window.
	if (m_wndMsg.GetSafeHwnd() && m_wndMsg.IsWindowVisible())
		PlaceMsg();
}

void CPtrWnd::OnPaint() 
{
	CPaintDC dc(this); // device context for painting
	
	// Get current window clip region. CWnd::GetWindowRgn takes a handle to an
	// existing GDI object,  replacing its data to make it copy of window rgn.
	HRGN hRgn = ::CreateRectRgn(0, 0, 1, 1); // create GDI obj w/temp junk values
	GetWindowRgn(hRgn);						// fill it in with window rgn info
	CRgn* pRgnWnd = CRgn::FromHandle(hRgn);	// allocate temp CRgn to wrap it.  

	// Fill with our custom background brush.
	dc.FillRgn(pRgnWnd, &m_bkBrush);
	// Highlight border of region in black
	CBrush brBlack(RGB(0, 0, 0));
	dc.FrameRgn(pRgnWnd, &brBlack, 1, 1);
}
	
// Place msg window with respect to the pointer
void CPtrWnd::PlaceMsg()
{
	if (! m_wndMsg.GetSafeHwnd()) return;	// doesn't exist

	// simple method: try to place along top at side of pointer rect
	CRect rcPtr;
	GetWindowRect(rcPtr);
	CRect rcMsg;
	m_wndMsg.GetWindowRect(rcMsg);	// fetch current w,h of message
	int cxMsg = rcMsg.Width();
	int cyMsg = rcMsg.Height();

	// prefer to place it top-aligned offset from right or left of pointer,
	// depending on ptr direction.
	int yNewTop = rcPtr.top;
	int xNewLeft = (m_dir == UpLeft) ? rcPtr.right + 5 : rcPtr.left - (cxMsg + 5);

	// Adjust placement to ensure it's not placed over the edge of the screen.
	// following API fetches usable desktop area after subtracting taskbar.
	CRect rcWorkArea;
	if (SystemParametersInfo(SPI_GETWORKAREA, 0, (PVOID)&rcWorkArea, 0)) 
	{
		if (xNewLeft + cxMsg > rcWorkArea.right) 
			xNewLeft = rcPtr.left - (cxMsg + 5);// put it left of ptr instead
		else if (xNewLeft < rcWorkArea.left) 
			xNewLeft = rcPtr.right + 5;			// put it right of ptr instead
		
		if (yNewTop + cyMsg > rcWorkArea.bottom) 
			yNewTop = rcWorkArea.bottom - cyMsg;// move it up from bottom to fit
	} // else syscall failed, ignore.
	
	// place it with its current size.
	m_wndMsg.SetWindowPos(NULL, xNewLeft, yNewTop, 0, 0,
		SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE);
}

void CPtrWnd::ShowMsg(LPCTSTR pszMsg)
{
	// empty or NULL string means hide the message window:
	if (! pszMsg || pszMsg[0] == '\0')
		if (m_wndMsg.GetSafeHwnd()) {
			m_wndMsg.ShowWindow(SW_HIDE);
			return;
		}
	// else have new message to show
	
	// ensure msg window created, placed and showing as needed
	if (m_wndMsg.GetSafeHwnd() == NULL) {
		m_wndMsg.Create(this);
	}
	PlaceMsg();
	m_wndMsg.ShowWindow(SW_SHOWNA);

	// set the message to it.
	m_wndMsg.SetMsg(pszMsg);

	// note: possible ptr itself is currently hidden -- this method doesn't show it,
	// so client can show ptr-less message window if desired.
}


/////////////////////////////////////////////////////////////////////////////
// CMsgBalloon: Modeless roundrect popup msg balloon for demo pointer.
// 
// The PtrDlg window functions as the "Agent" during demo playback, and
// this window functions as its popup msg balloon. Pointer clients manipulate the 
// msg window via methods in the Ptr window, which uses an embedded object of
// this class to do the work.
//
// Currently this is a fixed-size modeless dialog box with a single read-only edit 
// control in the center of the template. We set a standard large bold font and background 
// color like Winhelp popups; methods could be added to change these if we wanted 
// to change these for use in other contexts. Might also be desirable to size 
// window to text contents.
//
// There is no particular reason for this to be a dialog anymore (earlier versions it 
// had some button controls), as opposed to a subclassed edit control, say, or
// just a CWnd. 
//
CMsgBalloon::CMsgBalloon(CWnd* pParent /*=NULL*/)
	: CDialog(CMsgBalloon::IDD, pParent)
{
	//{{AFX_DATA_INIT(CMsgBalloon)
	//}}AFX_DATA_INIT
}


void CMsgBalloon::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMsgBalloon)
	DDX_Control(pDX, IDC_DEMO_MSG, m_editMsg);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CMsgBalloon, CDialog)
	//{{AFX_MSG_MAP(CMsgBalloon)
	ON_WM_CREATE()
	ON_WM_CTLCOLOR()
	ON_WM_PAINT()
	ON_WM_DESTROY()
	ON_WM_ACTIVATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMsgBalloon message handlers
enum { MsgCornerW = 32, MsgCornerH = 32};	// larger than for little pointer

int CMsgBalloon::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CDialog::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	// Set window region to roundrect. Window region coords are relative to
	// window origin, not client-area origin. 
	CRect rcWnd;
	GetWindowRect(rcWnd);
	CRgn rgn;
	rgn.CreateRoundRectRgn(0, 0, rcWnd.Width(), rcWnd.Height(), 
		                   MsgCornerW, MsgCornerH);

	// Following call passes ownership of the region object to Windows. 
	SetWindowRgn((HRGN) rgn.Detach(), TRUE);

	// allocate our custom background brush
	m_bkBrush.CreateSolidBrush(RGB(255, 255, 226));

	return 0;
}

BOOL CMsgBalloon::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// allocate and set edit control font. 
	// Use large (12 point)  Sans Serif for readability.
	CClientDC dc(this);
	int nHeight = -((dc.GetDeviceCaps(LOGPIXELSY) * 12) / 72);
	m_fontMsg.CreateFont(nHeight, 0, 0, 0, FW_BOLD, 0, 0, 0,
		DEFAULT_CHARSET, OUT_CHARACTER_PRECIS, CLIP_CHARACTER_PRECIS,
		DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE,
		"MS Sans Serif" 
		// "Arial"
	);

	m_editMsg.SetFont(&m_fontMsg, FALSE);
		
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CMsgBalloon::OnDestroy() 
{
	CDialog::OnDestroy();

	// free GDI resources we allocated on creation
	m_bkBrush.DeleteObject();
	m_fontMsg.DeleteObject();
}

//
// Set the message text
//
void CMsgBalloon::SetMsg(LPCTSTR pszMsg)
{
	// ensure Windows window exists
	if (GetSafeHwnd() == 0) return;
	
	// else update text.
	m_editMsg.SetWindowText(pszMsg);

	UpdateWindow();	// necessary?
}

// 
// Painting:
//
HBRUSH CMsgBalloon::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	HBRUSH hbr = CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	
	// use custom color for edit control and dialog window background.
	if (pWnd->m_hWnd == m_editMsg.m_hWnd ||
		nCtlColor == CTLCOLOR_DLG) {
		pDC->SetBkMode(TRANSPARENT);
		hbr = (HBRUSH) m_bkBrush;
	}
	return hbr;
}

void CMsgBalloon::OnPaint() 
{
	CPaintDC dc(this); // device context for painting

#if 0 
	// draw a black border around the inside of the client area roundrect
	CPen penFrame(PS_INSIDEFRAME, 2, RGB(0, 0, 0));
	CRect rcClient;
	GetClientRect(rcClient);
	dc.SelectObject(penFrame);
	dc.SelectStockObject(NULL_BRUSH);
	//  -1 adjustments are needed to get right and top outline showing.
	// KB Q43596 reports: The coordinates returned by GetClientRect() are not 
	// inclusive. For example, to draw a border around the edge of the client area,
	// draw it from the coordinates (Rectangle.left, Rectangle.top) to 
	// (Rectangle.right-1, Rectangle.bottom-1). 
	dc.RoundRect(0, 0, rcClient.Width() - 1, rcClient.Height() - 1,
				MsgCornerW, MsgCornerH);
#endif

	// Get current window clip region. CWnd::GetWindowRgn takes a handle to an
	// existing GDI object,  replacing its data to make it copy of window rgn.
	HRGN hRgn = ::CreateRectRgn(0, 0, 1, 1); // create GDI obj w/temp junk values
	GetWindowRgn(hRgn);						// fill it in with window rgn info
	CRgn* pRgnWnd = CRgn::FromHandle(hRgn);	// allocate temp CRgn to wrap it.  

	// Draw border of region using FrameRgn API.
	CBrush brBlack(RGB(0, 0, 0));
	dc.FrameRgn(pRgnWnd, &brBlack, 2, 2);

	// Do not call CDialog::OnPaint() for painting messages
}

// Embedded edit control is readonly, but still shows annoying caret if it gets the focus.
// To block that, redirect activation on any attempt to activate this window, 
// (can come from Windows, or on user click in window or in edit control).
void CMsgBalloon::OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized) 
{
	if (nState != WA_INACTIVE && ! bMinimized) {
		AfxGetMainWnd()->SetActiveWindow();
	}
}












