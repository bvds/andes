// MDIFixedSizeFrame.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "MDIFixedSizeFrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMDIFixedSizeFrame

IMPLEMENT_DYNCREATE(CMDIFixedSizeFrame, CMDIChildWnd)

CMDIFixedSizeFrame::CMDIFixedSizeFrame()
{
}

CMDIFixedSizeFrame::~CMDIFixedSizeFrame()
{
}


BEGIN_MESSAGE_MAP(CMDIFixedSizeFrame, CMDIChildWnd)
	//{{AFX_MSG_MAP(CMDIFixedSizeFrame)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMDIFixedSizeFrame message handlers

BOOL CMDIFixedSizeFrame::PreCreateWindow(CREATESTRUCT& cs) 
{
	// turn off resizing border, MAX, MIN boxes
	cs.style &= ~(WS_THICKFRAME | WS_MAXIMIZEBOX | WS_MINIMIZEBOX);
	return CMDIChildWnd::PreCreateWindow(cs);
}
