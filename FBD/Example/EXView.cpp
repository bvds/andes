/////////////////////////////////////////////////////////////////////////////
//
// EXView.cpp : implementation file for example study mode view
//
/////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "ChildFrm.h"
#include "Exp2Dlg.h"
#include "Exp4Dlg.h"
#include "ExpBdyDg.h"
#include "EXView.h"
#include "EXPlanVw.h"
#include "Expmenu.h"			// Explain menu IDs
#include "MyMenu.h"
#include "BrowsDlg.h"
#include "HelpIfc.h"
#include "VwOptDlg.h"
#include "DoneDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

//
// szDiagram, szSketch: strings identifying special masked regions
//
static const char szDiagram[] = "Diagram";
static const char szSketch[] = "Sketch";

// for experimenting with different appearances:
static BOOL bHighlightBox = FALSE;
static COLORREF colorBox = RGB(128, 128, 128);

/////////////////////////////////////////////////////////////////////////////
// CEXView

IMPLEMENT_DYNCREATE(CEXView, CBaseView)

CEXView::CEXView()
{
#ifdef _DEBUG
  //  theApp.m_oldMemState.Checkpoint();
#endif
	m_pVisibleObj = NULL;
	m_pSelection = NULL;
	m_bExplaining = FALSE;
	m_pFindObj = NULL;
	m_pHintObj = NULL;
	m_pExpObj  = NULL;
	m_pExpSel  = NULL;
	m_pVisible2nd = NULL;
	m_pVisible3rd = NULL;
	m_bInitScroll = FALSE;		// flags scroll view initialization to OnSize handler
	m_bScaleToFit = FALSE;		// Sets scale to fit mode 
	m_bCoachMode = FALSE;		// When Cristina offers advice on what to explain
	m_bCtrlMode = FALSE;
}

#define EXBTN_WIDTH 100		// !!! ought to be computed from text metrics
#define EXBTN_HEIGHT 25

int CEXView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CView::OnCreate(lpCreateStruct) == -1)
		return -1;
	// create question mark bitmap for menu
	m_pQmarkBmp = new CBitmap();
	m_pQmarkBmp->LoadBitmap(IDB_QMARK);

	CFBDDoc* pDoc = GetDocument();
	CFrameWnd* pFrame = GetParentFrame();
	// see if allow example study help
	if ( theApp.m_wHelpFlags & fExample){
		// Create the explain button (a modeless dialog) 
		m_pDlg = new CExplainDlg(pDoc, pFrame);
		m_pDlg->Create(IDD_EXPLAIN, pFrame);
	}
	return 0;
}

CEXView::~CEXView()
{// see if allowed example study help
	if ( theApp.m_wHelpFlags & fExample){
	//when example window destroyed, must destroy explain button 
		m_pDlg->DestroyWindow();
	}
	m_fontText.DeleteObject();	// safe if never set
	delete m_pQmarkBmp;
/* #ifdef _DEBUG
	theApp.m_newMemState.Checkpoint();
    if( theApp.m_diffMemState.Difference( theApp.m_oldMemState, theApp.m_newMemState ) )    {
        TRACE( "Memory leaked!\n" );  
		theApp.m_diffMemState.DumpStatistics();}
#endif */
}

BEGIN_MESSAGE_MAP(CEXView, CBaseView)
	//{{AFX_MSG_MAP(CEXView)
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_RBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_COMMAND(ID_VIEW_OPTIONS, OnViewOptions)
	ON_WM_CREATE()
	ON_UPDATE_COMMAND_UI(ID_EXPLAIN, OnUpdateExplain)
	ON_WM_SIZE()
	ON_WM_PAINT()
	ON_WM_ERASEBKGND()
	ON_COMMAND(ID_VIEW_FONT, OnViewFont)
	ON_WM_ENABLE()
	ON_COMMAND(ID_EXPLAIN, OnExplainBtn)
	ON_WM_KEYDOWN()
	ON_WM_KEYUP()
	//}}AFX_MSG_MAP
	ON_COMMAND_RANGE(ID_EXP_FIRST, ID_EXP_LAST, OnExplainCmd)
	ON_UPDATE_COMMAND_UI_RANGE(ID_EXP_FIRST, ID_EXP_LAST, OnUpdateExplainCmd)
END_MESSAGE_MAP()

// 
// Scrolling support as in FBDView.
//

// helper inits or updates scroll view  from document size
void CEXView::UpdateScrollSize()
{
	CSize size = GetDocument()->GetSize();

	if (! m_bScaleToFit)
	{
		//Adapted from DRAWCLI code to set scroll sizes using MM_TEXT mapping mode
		CClientDC dc(this);
		CSize size = GetDocument()->GetSize(); 
		// For now, set tiny size to prevent scroll bars from showing
		//CSize size(1 * nLUsPerInch, 8 * nLUsPerInch);
		size.cx = MulDiv(size.cx, dc.GetDeviceCaps(LOGPIXELSX), nLUsPerInch);
		size.cy = MulDiv(size.cy, dc.GetDeviceCaps(LOGPIXELSY), nLUsPerInch);
		SetScrollSizes(MM_TEXT, size);
	}
	else // scale to fit mode
		SetScaleToFitSize(size); 
}

void CEXView::OnInitialUpdate() 
{
	CBaseView::OnInitialUpdate();
	
	// Save pointers to Diagram and Sketch
	m_pDiagram = GetMask(szDiagram);
	m_pSketch = GetMask(szSketch);

	// Ensure Diagram and Sketch masks are on top
	// Suppress set modified in MoveToFront else user will be prompted to save at end.
	BOOL bWasModified = GetDocument()->IsModified();
	if (m_pDiagram) GetDocument()->MoveToFront(m_pDiagram);
	if (m_pSketch) GetDocument()->MoveToFront(m_pSketch);
	GetDocument()->SetModifiedFlag(bWasModified);
	 
	// Record initial object ids into log
	/* LogInitObjects(); */

	//  create a suitable font for example viewing.
	// choose small font to work on 800 X 600 student monitors
	// NB: must also be used by FBDView when authoring to get layout right
	CClientDC dc(this);
	LOGFONT logFont;
	memset(&logFont, '\0', sizeof(logFont));
	// want 8 point Arial plain. 
	strcpy(logFont.lfFaceName, "Arial");
	logFont.lfPitchAndFamily = VARIABLE_PITCH | FF_SWISS;
	logFont.lfWeight = FW_DONTCARE;
	// Need size in LUs. ?? OK to just convert points to LUs?
	logFont.lfHeight = -Round(dc.GetDeviceCaps (LOGPIXELSY) / 9.0);  /* = 8pts/ 72ppi */
	m_fontText.CreateFontIndirect(&logFont);

// Initially Cristina wanted to force the student to view the help 
// example file whenever they first opened an example.

/*	LPCTSTR pszResult =	HelpSystemExecf("(show-help)");
	if (pszResult && (strcmp(pszResult, "T") == 0))
		AfxGetApp()->WinHelp(IDR_EXAMPLETYPE, HELP_CONTEXT); */

#ifdef EX_SCROLL_VIEW	// scrollview code

	UpdateScrollSize();
	m_bInitScroll = TRUE;	// used to avoid assertion failure in OnSize
#endif	EX_SCROLL_VIEW	// scrolling view code
}

// init dc for scrolling:
void CEXView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
{
	// In CScrollView,  base class adjusts viewport origin to match 
	// current scrolled position in the document:
	CBaseView::OnPrepareDC(pDC, pInfo);

	if (! m_bScaleToFit) 
	{
		// Now set up to map logical units to this device:
		pDC->SetMapMode(MM_ANISOTROPIC);
		pDC->SetViewportExt(pDC->GetDeviceCaps(LOGPIXELSX),
							pDC->GetDeviceCaps(LOGPIXELSY));
		pDC->SetWindowExt(nLUsPerInch, nLUsPerInch);
	}
	// else base class sets up scaling mapping mode
	
	// if custom font set, select it into DC before all drawing.
	// mainly for testing by authors, esp with scale to fit mode
	if ((HFONT) m_fontText)
		pDC->SelectObject(&m_fontText);
}

BOOL CEXView::OnScrollBy(CSize sizeScroll, BOOL bDoScroll) 
{
#ifndef EX_SCROLL_VIEW // non-scrolling view code

	return CBaseView::OnScrollBy(sizeScroll, bDoScroll);

#else //  scrolling view code

	// Do the scroll
	if (! CBaseView::OnScrollBy(sizeScroll, bDoScroll))
		return FALSE;

	// update the position of button:
	if ((bDoScroll)&&(theApp.m_wHelpFlags & fExample)){
		if (m_pDlg->IsWindowVisible()){//if done button visible
			if (m_pDiagram && m_pVisibleObj == m_pDiagram && m_pSelection)
				// inside diagram and have a selected object:
				UpdateBtnPos(m_pSelection);
			else if (m_pVisibleObj)
				// have a visible item: (assume it has explain menu, since btn visible).
				UpdateBtnPos(m_pVisibleObj);
			Invalidate();
		}
	}
	return TRUE;

#endif // Scrolling view code
}

// Coordinate conversions:
void CEXView::ClientToDoc(CPoint& point)
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.DPtoLP(&point);
}

void CEXView::ClientToDoc(CRect& rect)	// verifies result is normalized
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.DPtoLP(&rect);

	// ASSERTS adapted from DRAWCLI to use our y orientation.
	// Not clear if this is appropriate requirement for us, might use to
	// convert non-normalized client rects.
	// (Result will be normalized just in case input client rect is.)
	ASSERT(rect.left <= rect.right);
	ASSERT(rect.top <= rect.bottom);
}

void CEXView::DocToClient(CPoint& point)
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.LPtoDP(&point);
}

void CEXView::DocToClient(CRect& rect)	// Note: returns normalized rect
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.LPtoDP(&rect);
	// Following from DRAWCLI, not clear if appropriate for us.
	// Guarantees result is normalized client rect, useable in GDI computations.
	// Loses orientation of doc rect if it was needed.
	rect.NormalizeRect();
}


/////////////////////////////////////////////////////////////////////////////
// CEXView drawing

// 
// For drawing we render the document objects in hidden mode using the "DrawMasked"
// function in the document (which probably should be moved to the view). This
// only makes a difference for certain text label objects; all other 
// objects, including diagram objects, wind up being drawn normally in this 
// mode. To hide the diagram, the author must have defined "custom masks" 
// around regions in the document. 
//
// "Custom masks" are currently represented by text objects with the m_bMask 
// flag set. They are functioning as author-defined regions to screen off areas in the diagram, 
// mainly the problem sketch and free-body-diagram. Their bounding box defines the
// region.
//
// This is a bit of a hack to see us through until we get the diagram and sketch
// defined as distinct objects -- currently the document is just a flat list of 
// graphic objects. Overloading the text label object with this function is also a
// bit ugly but lets us consider only text items specially in example mode and avoid 
// introducing a new object class. !!! But probably should be new class.
//
// Text can also be marked with the m_bVisible flag to remain visible even in 
// example mode, used for visible equation labels, say. Note this flag has the 
// *opposite* meaning for masks: a visible mask *hides* the objects beneath it, an 
// invisible mask doesn't.
//
// Added for mouse button processing: The text in a custom mask may begin with
// "Sketch" (for the graphic) or "Diagram" (for the free body diagram) so that
// the code can identify these when processing mouse clicks to show or hide these
// regions while viewing a text line.
//
// Added EXText type to hold example text items. Should make m_bVisible flag obsolete:
// EXText instances are masked or hiddent, regular CLabel text always displays normally.
//


// IsMask -- helper tells if object is used to define a masked region
inline  BOOL CEXView::IsMask(CDrawObj* pObj)		
{
	 return pObj->IsKindOf(RUNTIME_CLASS(CLabel)) &&
	        ((CLabel*)(pObj))->m_bMask ;
}

// IsHideable: identifies "Hideable objects", those that can be shown or hidden.
inline  BOOL CEXView::IsHideable(CDrawObj* pObj)
{
	// Text object that's either a mask or not marked visible
	return pObj != NULL && pObj->IsKindOf(RUNTIME_CLASS(CLabel)) &&
		     ( ((CLabel*)pObj)->m_bMask || 
			   ! ((CLabel*)pObj)->m_bVisible );
}


// GetMask: find mask with name beginning with given string, NULL if none.
CLabel* CEXView::GetMask(LPCTSTR pszName)
{
	POSITION pos = GetDocument()->m_objects.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = GetDocument()->m_objects.GetNext(pos);
		if (IsMask(pObj) &&
			(strncmp(((CLabel*)pObj)->m_strName, pszName, strlen(pszName)) == 0) )
			return (CLabel*) pObj;
	}
	return NULL;
}

//
// OnPaint -- repaint the currently invalid region
//
// Sets up offscreen bitmap for repainting dirty region, uses OnDraw to paint it
// then blits it onto the screen. Calls to CWnd:Invalidate should specify FALSE
// to prevent Windows from erasing the background, which is what causes flashing.
// This routine fairly generic, hence re-usable. Needs DocToClient helper and
// appropriate code for drawing background.
//
void CEXView::OnPaint() 
{
	CPaintDC PaintDC(this); // device context set up for repainting update rect
	OnPrepareDC(&PaintDC);

	CDC memDC;				// dc for offscreen bitmap
	CBitmap bitmap;
	CBitmap* pOldBitmap;
	CDC* pDrawDC = &PaintDC;		// holds dc we actually drew into

	// only repaint the invalid rect = clipping region of PaintDC
	CRect docDirty;
	PaintDC.GetClipBox(docDirty);	// docDirty = invalid region, logical coords
	CRect cliDirty = docDirty;			
	DocToClient(cliDirty);		// = invalid region, client area device coords
	/* TRACE("Repainting doc rect: left %d, top %d, right %d, bottom %d\n",
			docDirty.left, docDirty.top, docDirty.right, docDirty.bottom); */

	// draw to offscreen bitmap ("in-memory device") for non-flashing repaints
	if (memDC.CreateCompatibleDC(&PaintDC))
	{
		// memory device allocates only enough bits to hold the dirty rectangle
		if (bitmap.CreateCompatibleBitmap(&PaintDC, cliDirty.Width(), 
			                                        cliDirty.Height()) )
		{
			pOldBitmap = memDC.SelectObject(&bitmap);
				
			// Set up dc for in-memory drawing. First set up normal doc-screen mapping
			OnPrepareDC(&memDC, NULL); 
			// Offset viewport of memory screen more because entire bitmap 
			// holds only dirty piece out of the drawing
			memDC.OffsetViewportOrg(-cliDirty.left, -cliDirty.top);

			// clip offscreen drawing to the same rectangle
			memDC.IntersectClipRect(docDirty);

			memDC.SetBrushOrg(cliDirty.left % 8, cliDirty.top % 8); // not yet needed

			pDrawDC = &memDC;	// now it's ready for drawing into
		}
	}

	// Paint background, since we have asked Windows not to erase it
	pDrawDC->FillSolidRect(docDirty, IsWindowEnabled()? 
		::GetSysColor(COLOR_WINDOW) :/*else*/ ::GetSysColor(COLOR_INACTIVEBORDER)); 

	// Now render the view:
	OnDraw(pDrawDC);

	// if succeeded in drawng offscreen, now blt bitmap onto the screen
	if (pDrawDC != &PaintDC) 
	{
		PaintDC.SetViewportOrg(0,0);
		PaintDC.SetWindowOrg(0,0);
		PaintDC.SetMapMode(MM_TEXT);
		memDC.SetViewportOrg(0,0);
		memDC.SetWindowOrg(0,0);
		memDC.SetMapMode(MM_TEXT);
		PaintDC.BitBlt(cliDirty.left, cliDirty.top, cliDirty.Width(), cliDirty.Height(),
			        &memDC, 0, 0, SRCCOPY);

		memDC.SelectObject(pOldBitmap);
	}
}
//
// OnDraw: handle example mode repainting. Note we also do some drawing from event
// handlers without forcing a repaint.
// This routine not sensitive to whether mouse buttons are down for showing
// diagram/sketch, but that's OK because those are short-lived transient modes
// while tracking (capturing) the mouse, during which time it's unusual to have 
// to repaint.
//
void CEXView::OnDraw(CDC* pDC)
{
	CFBDDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	
	// First ask document to render all objects in hidden mode into our DC
	// Drawmasked draws example text hidden but draws diagram objects in 
	// normal mode. They will be covered when custom masks get drawn on top.
	// Rendering objects underneath masks is redundant, but we don't care for now.
	pDoc->DrawMasked(pDC);

	// Draw the FIND statement normally if it is being shown.
	if (m_pFindObj != NULL)
	{
		// first erase the region's background 
		pDC->FillSolidRect(m_pFindObj->m_position, ::GetSysColor(COLOR_WINDOW));
		m_pFindObj->Draw(pDC);
	}
	
	// Draw the object the student is explaining
	// in purple and normally if it is being shown.
	if (m_pExpObj != NULL)
	{
		// first erase the region's background 
		if (m_pExpObj == m_pDiagram){
		//	ShowRegion(m_pDiagram);
		}
		else{
			pDC->FillSolidRect(m_pExpObj->m_position, ::GetSysColor(COLOR_WINDOW));
			pDC->SetTextColor(RGB(128, 0, 128));// change text to purple
			m_pExpObj->Draw(pDC);
			pDC->SetTextColor(RGB(0, 0, 0));// change text back to black
		}
	
	}

	// Then draw the visible region normally, if any. This is either text piece or masked 
	// area. If visible region is defined by a mask, do nothing, as stuff underneath
	// should already have been drawn normally above (but see below for diagram).
	if (m_pVisibleObj != NULL && ! IsMask(m_pVisibleObj)) // not a mask
	{
		// first erase the region's background 
		pDC->FillSolidRect(m_pVisibleObj->m_position, IsWindowEnabled()? 
			::GetSysColor(COLOR_WINDOW)   :/*else*/  ::GetSysColor(COLOR_INACTIVEBORDER));
		//hint objects and explain objects are drawn in purple
		if ((m_pHintObj==m_pVisibleObj)||(m_pExpObj==m_pVisibleObj))
			pDC->SetTextColor(RGB(128, 0, 128));//change text to purple
		
		// draw the object underneath in normal mode
		m_pVisibleObj->Draw(pDC);
		pDC->SetTextColor(RGB(0, 0, 0));//change text back to black

	}
	// If the control button is being held down, 
	// there can be up to three visible objects
	// Draw the 2nd visible object normally if it is being shown.
	if (m_pVisible2nd != NULL && ! IsMask(m_pVisible2nd))
	{
		// first erase the region's background 
		// first erase the region's background 
		pDC->FillSolidRect(m_pVisible2nd->m_position, IsWindowEnabled()? 
			::GetSysColor(COLOR_WINDOW)   :/*else*/  ::GetSysColor(COLOR_INACTIVEBORDER));
		if ((m_pHintObj==m_pVisible2nd) ){
			pDC->SetTextColor(RGB(128, 0, 128));
		}
		m_pVisible2nd->Draw(pDC);
	}
	// Draw the 3rd visual object normally if it is being shown.
	if (m_pVisible3rd != NULL && ! IsMask(m_pVisible3rd))
	{
		// first erase the region's background 
		pDC->FillSolidRect(m_pVisible3rd->m_position, IsWindowEnabled()? 
			::GetSysColor(COLOR_WINDOW)   :/*else*/  ::GetSysColor(COLOR_INACTIVEBORDER));
		if ((m_pHintObj==m_pVisible3rd) ){
			pDC->SetTextColor(RGB(128, 0, 128));
		}
		m_pVisible3rd->Draw(pDC);
	}

	// if mouse is moving in diagram region we lighten everything so that only 
	// selected object is clearly visible
	if (m_pDiagram && ((m_pDiagram == m_pVisibleObj) || (m_pExpSel)) )
	{
		// merge grey rectangle with pixels on screen to lighten them
		int nGreyLevel = theApp.m_nGreyLevel;
		COLORREF color = RGB(nGreyLevel, nGreyLevel, nGreyLevel);
		CBrush brushMask;
		(void) brushMask.CreateSolidBrush(color);
		int oldROP = pDC->SetROP2(R2_MERGEPEN);
		CBrush* pOldBrush = pDC->SelectObject(&brushMask);
		CPen* pOldPen = (CPen*)pDC->SelectStockObject(NULL_PEN);
		pDC->Rectangle(m_pDiagram->m_position);
		pDC->SelectObject(pOldBrush);
		pDC->SetROP2(oldROP);

		// now highlight the selected diagram object, if any
		if (m_pSelection) 
		{
			
			m_pSelection->Draw(pDC);		// draws normally
		//	pDC->SetTextColor(RGB(0, 0, 0));
			if (bHighlightBox)
				DrawHighlightBox(m_pSelection);	// show selection border too

			// show button if necessary
			if ( theApp.m_wHelpFlags & fExample)//see if allow example study help
			{
				if (GetExMenu(FALSE, m_pSelection) != -1) // has explain menu	
				{
					UpdateBtnPos(m_pSelection);
				//	m_pDlg->ShowWindow(TRUE);	// ensure button is showing
				}
				else 
					m_pDlg->ShowWindow(SW_HIDE);//  just hide button.
			}
		}
		// Draw the object the student is explaining
		// in purple and normally if it is being shown.
		if (m_pExpSel){
			pDC->SetTextColor(RGB(128, 0, 128));
			m_pExpSel->Draw(pDC);
			pDC->SetTextColor(RGB(0, 0, 0));
		}
	}
}

void CEXView::InvalObjRegion(CDrawObj* pObj)
{
	if (pObj == NULL) return;

	CRect pos = pObj->m_position;
	DocToClient(pos);
	InvalidateRect(pos, FALSE);
}

//
// Show/Hide region: Helpers for manipulating visibility of regions on the screen.
//
// A region is defined by a text label object. It is either a piece of example text
// to be shown or hidden, *or* a masked region defined by the bounding box of 
// the text object. We don't call these Show/Hide Object to avoid confusion:
// *Showing* a masked region actually means *hiding* the mask to let the objects 
// underneath be visible.
//
// These routines do not update m_pVisible object so they can be used for the
// button commands for showing/hiding diagram. Both also accept the NULL region and
// do nothing. 
//
void CEXView::HideRegion(CDrawObj* pObj)
{
	if (pObj == NULL) return;
	
	// for masked regions: mark mask as visible again so it will be drawn
	if (IsMask(pObj)) 
		((CLabel*) pObj)->m_bVisible = TRUE;
	// redraw region
	InvalObjRegion(pObj);
}

void CEXView::ShowRegion(CDrawObj* pObj)
{
	if (pObj == NULL) return;

	// This function only affects text objects and masks. Other objects are
	// always drawn normally and have visibility affected only by mask operations.
	if (! pObj->IsKindOf(RUNTIME_CLASS(CLabel)) )
		return;

	if ( IsMask(pObj) )	// show masked region
	{
		// mark mask invisible so it won't be drawn over objects beneath
		// !!! storing mask state in document violates doc/view, since
		// presumably document could be open in other views. Ignore for now
		((CLabel*)pObj)->m_bVisible = FALSE;
	}
	
	// force a redraw of region. 
	InvalObjRegion(pObj);
}

// Helper to manage updating of the one currently visible region:
// Handles showing/hiding of explain button as well.
void CEXView::SetVisibleObj(CDrawObj* pObj)
{	
	// nothing to do if no change
	if (pObj == m_pVisibleObj) return;

	// if showing explain button near item, take it down
	/* if (!IsMask(pObj)) */
	/* m_pDlg->ShowWindow(SW_HIDE); // change to move w/o hiding where possible */

	// if hiding the diagram, clear its selection too
	if (m_pDiagram && m_pDiagram == m_pVisibleObj)
		m_pSelection = NULL;

	// cover up the old region as long as the student isn't explaining
	// an object in the diagram
	if (! (m_pDiagram && m_pDiagram == m_pVisibleObj && 
		(m_pVisibleObj == m_pExpObj)) ){
		HideRegion(m_pVisibleObj);
		//Notify help system
		if ((m_pVisibleObj != NULL)&&(m_pVisibleObj != m_pExpObj)){
			
			CString oldId = IsMask(m_pVisibleObj) ? MaskTypeStr(m_pVisibleObj) : m_pVisibleObj->m_strId;
			if (m_bCoachMode)
				CheckStudyItem(oldId);
			else
				(void)HelpSystemSendf("(cover-item |%s|)", oldId);
		}
	}
	
	
	// and show the new one
	ShowRegion(pObj);
	m_pVisibleObj = pObj;

	// 
	// Update explain button appropriately.

	BOOL bHasButton = FALSE;	// true if new item has explain button
	// check if over example text which has an explain menu
	if (pObj->IsKindOf(RUNTIME_CLASS(CEXText)) ){
		// make sure an explain menu has been specified
		CString strMenu = ((CEXText*) m_pVisibleObj)->m_strMenu;
		bHasButton = ! (strMenu.IsEmpty() || strMenu == "(None)");
	}
	// !!! also check if entered diagram and over an object
	if ( theApp.m_wHelpFlags & fExample){//see if allow example study help
		if (bHasButton)	// place button by item and show it
		{
			UpdateBtnPos(m_pVisibleObj);
		//	m_pDlg->ShowWindow(SW_SHOW);
		}
		else // new visible item doesn't have one, so just hide it.
			m_pDlg->ShowWindow(SW_HIDE);
	}

}

/////////////////////////////////////////////////////////////////////////////
// CEXView diagnostics

#ifdef _DEBUG
void CEXView::AssertValid() const
{
	CView::AssertValid();
}

void CEXView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CFBDDoc* CEXView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return (CFBDDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CEXView message handlers


//
// Handle mouse move: 
//   Normal action is to adjust visible object appropriately.
//
void CEXView::OnMouseMove(UINT nFlags, CPoint point) 
{	//if mouse moves over the EXView in the control group (SE coach not configured)
	//we disable plan window and notify help system 
	if (!(theApp.m_wHelpFlags  & fExample)){
		CEXPlanVw* pView = theApp.GetEXPlanVw();
		if (pView->m_ctrlPlan.IsWindowEnabled()){
			pView->CollapseTree();
			pView->m_ctrlPlan.EnableWindow(FALSE);
			(void) HelpSystemSendf("(end-peek-plan)");
			SetFocus();
		}
	}
	CPoint local = point;
	ClientToDoc(local);

	// determine if mouse is on an object
	CDrawObj* pHitObj = GetDocument()->ObjectAt(local);
	
	// If mouse is on a hideable object and it's different than the currently 
	// shown one, update visibility accordingly.
	//
	// Note no change on exiting old region if don't hit a new object. This allows 
	// users to move the cursor out of the way of text, 
	// as long as it is still on blank space and doesn't hit anything else
	if (pHitObj != NULL && m_pVisibleObj != pHitObj && IsHideable(pHitObj)) 
	{
		// Notify Help system of change in visibility
		CString strId = IsMask(pHitObj) ? MaskTypeStr(pHitObj) : pHitObj->m_strId;
		(void)HelpSystemSendf("(uncover-item |%s|)", strId);
	
		if (m_pVisibleObj!=NULL){
			if (strId.Find("plan")!=-1)
				m_pExpObj = m_pVisibleObj;

			if (m_pVisibleObj->m_strId.Find("plan") != -1){
				HideRegion(m_pExpObj);
				m_pExpObj = NULL;
			}
		}
		//check if in control mode (i.e. CTRL key pressed)
		if ((m_bCtrlMode)&&(!m_bExplaining)){//do not allow this option if 
										//we are explaining
			if (m_pVisible2nd == NULL){//allow 2nd visible object
				m_pVisible2nd = pHitObj;
				ShowRegion(m_pVisible2nd);
			}
			else if ((m_pVisible3rd == NULL) && (pHitObj != m_pVisible2nd) ){
				m_pVisible3rd = pHitObj;//allow third visible object
				ShowRegion(m_pVisible3rd);
			}
		}
		else//otherwise, only one visible object
			SetVisibleObj(pHitObj);

		// Log SetVisible operation. Note no need to log hiding of objects. 
		LogEventf(EV_VIEW_OBJ, "%s %d %d", strId, local.x, local.y);
	}
	// Note we don't log mouse movements with no effect on visibility
	// Handle mouse move in visible diagram area 
	if (m_pDiagram && m_pDiagram == m_pVisibleObj  // diagram is visible 
		&& m_pDiagram->m_position.PtInRect(local) ) // & mouse is in it 
	{
		// hit test ignoring masks to find underlying FBD object
		CDrawObj* pFBDObj = GetDocument()->ObjectAt(local, 
											/* ignore:*/ NotSelectable);
		if ((pFBDObj)== NULL) //no longer can be null
			return;
		SetSelectedFBDObj(pFBDObj);
	}
}

// Change visible FBD object. Now used on mouse over object in diagram
void CEXView::SetSelectedFBDObj(CDrawObj* pFBDObj)
{
	// if is same as current selection, nothing to do
	if (m_pSelection == pFBDObj)
		return;

	// if already had selection, deselect it.
	if (m_pSelection)
	{
		// undo highlighting on old selection
	//	 DrawHighlightBox(m_pSelection); 
	
		LogEventf(EV_DESELECT_OBJ, "%s", m_pSelection->m_strId);//log deselection
		//Notify help system
		if (m_bCoachMode)
			CheckStudyItem(m_pSelection->m_strId);
		else
			(void)HelpSystemSendf("(deselect-obj |%s|)", m_pSelection->m_strId);

			
		// hide button
	//	 m_pDlg->ShowWindow(SW_HIDE); // changed to move w/o hiding if possible */
	}

	// Notify help system
	if (pFBDObj)
		(void)HelpSystemSendf("(selected-obj |%s|)", pFBDObj->m_strId);

	// update selection
	m_pSelection = pFBDObj;

#if 0 // old code didn't check if had menu
	if (theApp.m_wHelpFlags & fExample)//see if allowing example help
		UpdateBtnPos(m_pSelection);		// Move button beside selected object 
		/* m_pDlg->ShowWindow(SW_SHOW); */
#else // new code checks if has button
	if ( theApp.m_wHelpFlags & fExample)	//see if allow example study help
	{
		if (GetExMenu(FALSE, m_pSelection) != -1) // Get menu, but don't show it
			UpdateBtnPos(m_pSelection);// place button by item and show it
		else 
			m_pDlg->ShowWindow(SW_HIDE);//  just hide button.
	}
#endif 
	
	// highlight new selected object
	/* DrawHighlightBox(pFBDObj); */
	
	// Redraw the diagram 
	CRect clientPos = m_pDiagram->m_position;
	DocToClient(clientPos);
	InvalidateRect(clientPos, FALSE);

	// Log change in selected obj
	if (pFBDObj)
		LogEventf(EV_SELECT_OBJ, "%s", pFBDObj->m_strId);
}

// Paint selection rect around object in inverse video: second call undoes
void CEXView::DrawHighlightBox(CDrawObj* pFBDObj) 
{
	CRect rcBox =pFBDObj-> m_position;
	rcBox.NormalizeRect();
	rcBox.InflateRect(12, 12);
	CClientDC dc(this);
	OnPrepareDC(&dc);
	CPen penHighlight(PS_SOLID, 2, colorBox);
	CPen* pOldPen = dc.SelectObject(&penHighlight);
	dc.SelectStockObject(NULL_BRUSH);
	int oldROP2 = dc.SetROP2(R2_COPYPEN /* R2_XORPEN*/);
	dc.Rectangle(rcBox);
	dc.SelectObject(pOldPen);
	dc.SetROP2(oldROP2);
}

//
// For allowing mouse button to simultaneously show/hide diagram and sketch
//


//	Handle left mouse click:
//   Normal action is to show free body diagram.
//   But if mouse is already moving inside free body diagram region it selects
//   an object: we highlight the diagram object and show explain button for them
//
void CEXView::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CPoint local = point;
	ClientToDoc(local);

	// check if mouse already inside diagram: will be current region
	// if so, hand off to object click handler.
	if (m_pDiagram && m_pDiagram == m_pVisibleObj)
		/* OnClickFBDObj(nFlags, point)*/ ;
	else if (m_pDiagram)
	{	// show free body diagram
		ShowRegion(m_pDiagram);
		// capture mouse to ensure we get button up
		SetCapture();
		// Notify help system:
		(void) HelpSystemSendf("(peek-diagram)"); 
		// Log ViewDiagram command. 

		LogEventf(EV_VIEW_DIAGRAM, "%s %d %d", m_pDiagram->m_strId, local.x, local.y);
	}
}


BOOL CEXView::NotSelectable(CDrawObj* pObj)
{
	// can't select visible text or masks
	return (IsMask(pObj) 
		    || ( pObj->IsKindOf(RUNTIME_CLASS(CLabel))
		         && ((CLabel*)pObj)->m_bVisible) ); 
}

//
// Process click inside diagram: (now unused)
//
void CEXView::OnClickFBDObj(UINT nFlags, CPoint point)
{
	CPoint local = point;
	ClientToDoc(local);

	// determine if mouse is on an object
	CDrawObj* pHitObj = GetDocument()->ObjectAt(local, /* ignore:*/ NotSelectable);

	// update selection -- could NULL it if didn't hit anything
	m_pSelection = pHitObj;
	Invalidate(FALSE);	// !!!should be smarter about redrawing here

	if ( theApp.m_wHelpFlags & fExample){//see if allow example study help
		// update diagram explain button:
		if (m_pSelection != NULL)				// no selection
//			m_pDlg->ShowWindow(SW_HIDE);	// so hide it
//		else // new selection: place button and show it 
//		{
			UpdateBtnPos(m_pSelection);
		//	m_pDlg->ShowWindow(SW_SHOW);
//		}
	}
	
	// Notify help system
	HelpSystemSendf("(selected-obj |%s|)", pHitObj? pHitObj->m_strId : "NIL");
	// Log change in selected obj
	LogEventf(EV_SELECT_OBJ, "%s", pHitObj->m_strId);
}

void CEXView::OnLButtonUp(UINT nFlags, CPoint point) 
{
	CPoint local = point;
	ClientToDoc(local);

	// End lbutton peek at Diagram region by hiding it again. But only if it is not
	// also the currently visible object because mouse is over it
	if (!m_pDiagram) return; 

	if (m_pVisibleObj != m_pDiagram) 
	{
		HideRegion(m_pDiagram);
	}
	ReleaseCapture();

	// Notify help system
	(void) HelpSystemSendf("(end-peek-diagram %s)", m_pDiagram == m_pVisibleObj ? "T": "NIL");
	
	// Log EndDiagram command, noting final visibility for convenience. 
	LogEventf(EV_END_DIAGRAM, "%s %d %d %c", m_pDiagram->m_strId, local.x, local.y, 
			m_pDiagram == m_pVisibleObj ? 'V' : 'H' );
	
}

//
// Right button does the same for Sketch as Left button for diagram
//
void CEXView::OnRButtonDown(UINT nFlags, CPoint point) 
{
	if (! m_pSketch) return;// do nothing if no sketch
	CPoint local = point;
	ClientToDoc(local);
	
	ShowRegion(m_pSketch);
	m_pFindObj = GetDocument()->Lookup("find");
	if (m_pFindObj != NULL)
		ShowRegion(m_pFindObj);
	// capture mouse to ensure get button up
	SetCapture();
	// Notify Help System
	(void) HelpSystemSendf("(peek-sketch)");
	if (m_pFindObj != NULL){
		(void) HelpSystemSendf("(uncover-item |%s|)", m_pFindObj->m_strId );
		LogEventf(EV_VIEW_OBJ, "%s %d %d", m_pFindObj->m_strId, local.x, local.y);
	}

	// Log ViewSketch command. 
	LogEventf(EV_VIEW_SKETCH, "%s %d %d", m_pSketch->m_strId, local.x, local.y);
	// Log SetVisible operation. Note no need to log hiding of objects. 
}

void CEXView::OnRButtonUp(UINT nFlags, CPoint point) 
{
	if (! m_pSketch) return;// do nothing if no sketch
	CPoint local = point;
	ClientToDoc(local);

	// End peek at sketch region by hiding it again. But only if it is not
	// also the visible object because mouse is over it
	if (m_pVisibleObj != m_pSketch) 
	{
		HideRegion(m_pSketch);

	}

	if ((m_pVisibleObj != m_pFindObj)&&(m_pFindObj != NULL)){
		HideRegion(m_pFindObj);
	}
	ReleaseCapture();

	// Notify Help System
	(void) HelpSystemSendf("(end-peek-sketch %s)", m_pSketch == m_pVisibleObj ? "T" : "NIL");
//	if (m_bCoachMode)
//		CheckStudyItem(m_pFindObj->m_strId);
//	else
	if (m_pFindObj != NULL)
		(void) HelpSystemSendf("(cover-item |%s|)", m_pFindObj->m_strId); 

	m_pFindObj = NULL;

	// Log EndSketch command, noting final visibility just to check. 
	LogEventf(EV_END_SKETCH, "%s %d %d %c", m_pSketch->m_strId, local.x, local.y, 
		   m_pSketch == m_pVisibleObj ? 'V' : 'H' );


}
//
// Allow changes in view options
// 
void CEXView::OnViewOptions() 
{

	int oldMode = theApp.m_nMaskMode;
	int oldGreyLevel = theApp.m_nGreyLevel;
	int oldHighlightBox = bHighlightBox;
	COLORREF oldColorBox = colorBox;
	BOOL bOldScaleToFit = m_bScaleToFit;
	/* //
	// Use code from app object so that it's in one place
	//
	theApp.OnViewOptions(); */
	CViewOptsDlg dlg;
	dlg.m_nMaskMode = theApp.m_nMaskMode;
	dlg.m_nGreyLevel = theApp.m_nGreyLevel;
	dlg.m_bHighlightBox = bHighlightBox;
	dlg.m_colorBox = colorBox;
	dlg.m_bScaleToFit = m_bScaleToFit;
	
	if (dlg.DoModal() == IDOK) {
		theApp.m_nMaskMode = dlg.m_nMaskMode;
		theApp.m_nGreyLevel = dlg.m_nGreyLevel;
		bHighlightBox = dlg.m_bHighlightBox;
		colorBox = dlg.m_colorBox;
		m_bScaleToFit = dlg.m_bScaleToFit;
	}
	// if scaleto fit changed, reset scroll view
	if (m_bScaleToFit != bOldScaleToFit) {
		UpdateScrollSize();
		Invalidate();
	}

	// redraw if options changed
	if (theApp.m_nMaskMode != oldMode ||
		theApp.m_nGreyLevel != oldGreyLevel ||
		bHighlightBox != oldHighlightBox ||
		colorBox != oldColorBox) 
		Invalidate(FALSE);
}

// Logging

//
// Log initialization 
// Dump list of text object IDs to the log file so log messages can refer
// to them by ID.
//
void CEXView::LogInitObjects()
{
	
	CFBDDoc* pDoc = GetDocument();
	POSITION pos = pDoc->m_objects.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = pDoc->m_objects.GetNext(pos);
		if ( pObj->IsKindOf(RUNTIME_CLASS(CLabel)) )
		{
			if (IsMask(pObj))
				LogEventf(EV_OBJ_MASK, "%s  %d %d %d %d |%s|", pObj->m_strId, 
					  pObj->m_position.left, pObj->m_position.top,
					  pObj->m_position.bottom, pObj->m_position.right, 
					  (LPCTSTR) MaskTypeStr(pObj));
			else 
				LogEventf(EV_OBJ_TEXT, "%s %d %d %d %d %c |%s|", pObj->m_strId, 
					  pObj->m_position.left, pObj->m_position.top,
					  pObj->m_position.bottom, pObj->m_position.right, 
					  ((CLabel*) pObj)->m_bVisible ? 'V' : 'H', 
					  (LPCTSTR) pObj->m_strName);
		}
	}
}

// Return name defining type of mask object for logging
LPCTSTR CEXView::MaskTypeStr(CDrawObj* pObj)
{
	if (!IsMask(pObj))
		return "";

	if ( strncmp(pObj->m_strName, szDiagram, strlen(szDiagram)) == 0 )
		return (LPCTSTR) szDiagram;
	if ( strncmp(pObj->m_strName, szSketch, strlen(szSketch)) == 0 )
		return (LPCTSTR) szSketch;
	return "";
}

// Dispatch log event on playback
BOOL CEXView::DispatchEvent(EventID nEvent, LPCTSTR parms)
{
	int x, y;
	char szId[32];
	CDrawObj* pObj;

	switch (nEvent) 
	{
	case EV_OBJ_MASK:		// define ID of mask object
	case EV_OBJ_TEXT:		// define ID of text object -- also used in FBDView!
		// these record object ids, but we don't currently need them
		break;

	case EV_VIEW_OBJ:		// Set visible region
		// LogEventf(nEvent, "%x %d %d", OBJ_ID(pHitObj), point.x, point.y);
		if (sscanf(parms, "%*s %d %d", &x, &y) != 2)
			return FALSE;
		SetVisibleObj(GetDocument()->ObjectAt(CPoint(x, y)));
		break;

	case EV_SELECT_OBJ:
		/* LogEventf(nEvent, "%s", pFBDObj->m_strId); */
		if (sscanf(parms, "%s", szId) != 1)
			return FALSE;
		pObj = GetDocument()->Lookup(szId);
		if (pObj == NULL)
			return FALSE;
		SetSelectedFBDObj(pObj);
		break;

	case EV_VIEW_DIAGRAM:	// View diagram object
		// LogEventf(nEvent, "%x %d %d", OBJ_ID(pMask), point.x, point.y);
		if (m_pDiagram)
			ShowRegion(m_pDiagram);
		break;

	case EV_END_DIAGRAM:	// End view of diagram object
		/* LogEventf(nEvent, "%x %d %d %c", OBJ_ID(pMask), point.x, point.y, 
		   pMask != NULL && pMask == m_pVisibleObj ? 'V' : 'H' ); */
		if (m_pDiagram && m_pVisibleObj != m_pDiagram) 
			HideRegion(m_pDiagram);
			// could check against logged visibility flag!
		break;

	case EV_VIEW_SKETCH:	// View sketch object
		// LogEventf(nEvent, "%x %d %d", OBJ_ID(pMask), point.x, point.y);
		if (m_pSketch)
			ShowRegion(m_pSketch);
		break;

	case EV_END_SKETCH:		// End view of sketch object
		/* LogEventf(nEvent, "%x %d %d %c", OBJ_ID(pMask), point.x, point.y, 
		   pMask != NULL && pMask == m_pVisibleObj ? 'V' : 'H' ); */
		if (m_pSketch && m_pVisibleObj != m_pSketch) 
			HideRegion(m_pSketch);
			// could check against logged visibility flag!
		break;

	case EV_EXPLAIN:
		break;

	default:
		return FALSE;
		break;
	}
	return TRUE;
}

////////////////////////////////////////////////////////////////////////////
//
// Handling self-explanation commands:
//
////////////////////////////////////////////////////////////////////////////
void CEXView::OnExplainBtn()	// Handles push of explain button
{
	TRACE("Clicked Explain\n");

	HideCtrlObjs();

	ASSERT(theApp.m_wHelpFlags & fExample);//shouldn't be called if not allowing example help
	
	if (m_bCoachMode && (m_pVisibleObj != m_pDiagram)){
		//when in coachmode, need to know what is left to explain for
		//this particular item.  This changes as we explain other items
		LPCTSTR lpszResult = HelpSystemExecf("(left-to-explain |%s|)", m_pVisibleObj->m_strId);
		m_pVisibleObj->m_pEXInfo->m_checkList.RemoveAll();//if "L" nothing checked
		if (lpszResult){
			if (strcmp(lpszResult, "NIL")==0){//nothing left to explain
				m_pVisibleObj->m_pEXInfo->m_bExplained = TRUE;
				((CLabel*)m_pVisibleObj)->m_bHint = FALSE;
				//hint changes mask color
			}
			else{
				m_pVisibleObj->m_pEXInfo->m_bExplained = FALSE;
				CString strResult = (CString)lpszResult;
				if( (strResult.Find("R")>=0)||(strResult.Find("T")>=0) )
				{//need to explain rule or template
					int	menuID = GetExMenu(FALSE, m_pVisibleObj);//Get menu, but don't show it
					m_pVisibleObj->m_pEXInfo->m_checkList.AddTail(menuID);
				}
				if(strResult.Find("P")>=0)
				{//need to explain plan
					m_pVisibleObj->m_pEXInfo->m_checkList.AddTail(40006);
				}
			}
		}
	}
	if (m_pVisibleObj != m_pDiagram){
	//Show appropriate menu bShow=TRUE;
		GetExMenu(TRUE, m_pVisibleObj);
	}
	else
		GetExMenu(TRUE, m_pSelection);
	
	
}

// 
// Handler for commands fired from an explain menu
// nID is menu item id from explain-line menu
//
void CEXView::OnExplainCmd( UINT nID )
{
	m_bExplaining = TRUE;// We are EXplaining an item
	InvalObjRegion(m_pVisibleObj);//force redraw to show purple color of hint obj.

	m_pExpObj = m_pVisibleObj;//the item we are explaining, only one

	CDrawObj* pExpObj;//the text/drawn object that we are explaining
	if (m_pDiagram && m_pDiagram == m_pVisibleObj){// diagram is visible object
		m_pExpSel = m_pSelection;//we are explaining an object 
		pExpObj = m_pExpSel;
	}
	else{//we are explaining a text object		
		pExpObj = m_pExpObj;
	}
	m_pDlg->ShowWindow(SW_HIDE);//hide the explain button

	//notify help system which menu item we chose
	LPCTSTR pszResult = HelpSystemExecf("(selected-menu-item %d %s)", nID - ID_EXP_FIRST, pExpObj->m_strId);
	BOOL bIsMenuItemExp = FALSE;//initial default - nothing explained
	BOOL bOnlyRuleExp = FALSE;//initial default - nothing explained

	if ((pszResult) && (strcmp(pszResult, "T")==0))//totally explained
		bIsMenuItemExp = TRUE;						//rule & template
	else if ((pszResult) && (strcmp(pszResult, "NIL")!=0)){//partially explained
		bOnlyRuleExp = TRUE;						//rule only
	}//otherwise both are false (nothing explained)

	LogEventf(EV_EXPLAIN, "%d", nID - ID_EXP_FIRST);
	ASSERT(theApp.m_wHelpFlags & fExample);//shouldn't be called if not allowing example help


	//Activate the plan view
	CMDIChildWnd* pChild=
		((CMDIFrameWnd*)AfxGetApp()->m_pMainWnd)->MDIGetActive();
	CFBDDoc* pDoc = GetDocument();
	ASSERT(pDoc != NULL);
	CView* pView = (CView*)theApp.GetEXPlanVw();
	pChild->SetActiveView(pView);
	CEXPlanVw* ppView=(CEXPlanVw*)pView;

	//get menu name
	CString strMenuName;//the name of the menu
	pExpObj->GetTypeName(strMenuName);
	strMenuName.MakeLower();
	if (strMenuName == "why true")
		strMenuName = "fact";
	
	CString strForm;//instruction string (top of plan window/rule browser)
	ppView->m_bExplained  = bIsMenuItemExp;
	ppView->m_bRuleExp = bOnlyRuleExp;

	InvalObjRegion(pExpObj);//force redraw to show purple color of hint obj.

	switch(nID)
	{
		
	case ID_EXP_ROLE:{//explaining role in plan
		Logf("Start-Plan: %s", pExpObj->m_strId);
		strForm.Format(IDS_PLAN_FORM, strMenuName);
		ppView->m_txtInstruct.SetWindowText(strForm);

		ppView->m_bPlan = TRUE;
		ppView->EnableWindow(TRUE); //may use a function to know which node to select
				 
	}break;

	default:{//explaining why true or why this choice
		strForm.Format(IDS_RULE_FORM, strMenuName);
		ppView->m_txtInstruct.SetWindowText(strForm);

		CString strIndexPrompt;
		CString strMenuItem;//the string of the chosen menu item
		strMenuItem.LoadString(nID);
		strIndexPrompt.Format(IDS_RULE_HEAD, strMenuItem);
		
		
		ppView->m_txtHeading.SetWindowText(strIndexPrompt);
		CString str;
		str.LoadString(IDS_RULE_DIRECT);
		ppView->m_txtDirect.SetWindowText(str);			 
		
		int menuItemPos;
		int rule;
		if (strMenuItem.Find("direction") == -1){
			menuItemPos = 0;
			rule = pExpObj->m_pEXInfo->m_rule;
		}
		else{
			menuItemPos = 1;
			rule = pExpObj->m_pEXInfo->m_rule1;
			str = strMenuName + " direction";
		}
		if (bIsMenuItemExp){
			ppView->ShowTemplate(rule, menuItemPos, TRUE);
		}
		else {
			//************************
			//Need to go to rule browser in plan
			Logf("Start-Rule-Browser: %s", pExpObj->m_strId);
			ppView->m_bBrowser = TRUE;
			ppView->m_nPos = menuItemPos;
			ppView->EnableWindow(TRUE); //may use a function to know which node to select

		}
	
	}break;
	}//end switch
}//end OnExpCmd


// 
// currently, following a nice idea but is not ever called by the framework
// for the EXPLAIN button, since EXPLAIN cmd not on a toolbar or menu item. 
// I believe we would need to have the button window trap the MFC-defined 
// ON_IDLE_UPDATE_COMMAND_UI message and call UpdateDlgCtrls, or something like that, 
// to wire this button into MFC's automatic idle time CmdUI updating process.
//
void CEXView::OnUpdateExplain(CCmdUI* pCmdUI) 
{
	TRACE("Updating Explain command UI\n");
	// check if current region is diagram
	CLabel* pFBDMask = GetMask(szDiagram);
	if (pFBDMask != NULL && m_pVisibleObj != pFBDMask) 
	{
		// if so, only enable command if an object is selected
		pCmdUI->Enable(m_pSelection != NULL);
	}
	else
		pCmdUI->Enable(TRUE);
}


void CEXView::OnUpdateExplainCmd(CCmdUI* pCmdUI)
{
//	POSITION pos = m_checkList.GetHeadPosition();
//	while (pos != NULL){
//		int nChkID = m_checkList.GetNext(pos);
//		if (int(pCmdUI->m_nID) == nChkID)
//			pCmdUI->SetCheck(TRUE);
	
//	}


}




void CEXView::OnSize(UINT nType, int cx, int cy) 
{
	CBaseView::OnSize(nType, cx, cy);
	if( theApp.m_wHelpFlags & fExample){//see if allowing example help
		if (m_pDlg->IsWindowVisible()){//if done button visible
			// deduce which item it goes by (we haven't saved that anywhere!)
			if (m_pDiagram && m_pVisibleObj == m_pDiagram && m_pSelection)
				// inside diagram and have a selected object:
				UpdateBtnPos(m_pSelection);
			else if (m_pVisibleObj)
				// have a visible item: (assume it has explain menu, since btn visible).
				UpdateBtnPos(m_pVisibleObj);
		}
	}

	if (m_bScaleToFit)	// scaling to fit in view
	{
		// recalc text extents for new scaled size of view. (They don't scale evenly?)
		if (m_bInitScroll)			// Can't use Scrollview::OnPrepareDc before initted.
			RecalcTextExtents();
	}
}

// Place button beside obj, does affect show state
void CEXView::UpdateBtnPos(CDrawObj* pObj)
{
	ASSERT(theApp.m_wHelpFlags & fExample);//shouldn't be called if not allowing example help
	if (m_bCoachMode)
		RevampBtn(pObj);//change it to instruct user on what left to do
	// get current button rect (for width and height
	CRect rcBtn;					// current button rect.		
	m_pDlg->GetWindowRect(&rcBtn);	// NB: screen coords
	CRect posBtn(rcBtn);			// "pos" means page coords
	ScreenToClient(&posBtn);
	ClientToDoc(posBtn);

	// calc new top left for button window (NB: in page coords)
	CPoint ptNew;
	// default placement is at right, top of object's bounding rect
	CRect posObj = pObj->m_position;
	posObj.NormalizeRect();			// now undirected bounding box

	ptNew = pObj->GetBtnPos(rcBtn);
	
	// relocate the button to new position
	DocToClient(ptNew);
	ClientToScreen(&ptNew);
	m_pDlg->MoveWindow(ptNew.x, ptNew.y, rcBtn.Width(), rcBtn.Height(), TRUE);

	//check if explain button outside of frame window 
	CRect frmRect, btnRect;
	CFrameWnd* pWnd = GetParentFrame();
	pWnd->GetWindowRect(&frmRect);
	m_pDlg->GetWindowRect(&btnRect);
	if ((btnRect.bottom > frmRect.bottom)||(btnRect.top < frmRect.top))//if it is
		m_pDlg->ShowWindow(SW_HIDE);//don't show it
	else if (!m_bExplaining)
		m_pDlg->ShowWindow(SW_SHOWNA);
}



BOOL CEXView::OnEraseBkgnd(CDC* pDC) 
{
	// Eat this message to prevent default repainting of background.
	return TRUE;
	// TRACE("Got EraseBackground message\n");
}

// Cmd to set display font. Mainly for testing appearance when scaling:
void CEXView::OnViewFont() 
{
	LOGFONT logFont;
	if (! (HFONT) m_fontText) // no font selected yet
	{
		CClientDC dc(this);
		CFont* pScrnFont = dc.GetCurrentFont();
		pScrnFont->GetLogFont(&logFont);
	}
	else
		m_fontText.GetLogFont(&logFont);
	// initialize FontDialog's CHOOSEFONT struct m_cf to contain
	// a pointer to our LOGFONT, so result goes there.
	CFontDialog dlg(&logFont);
	
	if (dlg.DoModal() != IDOK)	// font choice cancelled
		return;

	// Change font and invalidate document.
	m_fontText.DeleteObject();
	m_fontText.CreateFontIndirect(&logFont);
	// need to recalc extent of all text pieces and
	// update document size on font change. Note text layout
	// may no longer be appropriate.
	RecalcTextExtents();
	Invalidate();
}

// recalc text item extents on change in font and update
// document size. These determine size of position boxes drawn.
// (if use this, really should be a method in document or drawobj's
void CEXView::RecalcTextExtents()
{
	CClientDC dc(this);
	OnPrepareDC(&dc);

	POSITION pos = GetDocument()->GetObjects()->GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = GetDocument()->GetObjects()->GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CLabel)) && ! ((CLabel*)pObj)->m_bMask) {
			// use DC method to recalc m_position box:
			dc.DrawText(pObj->m_strName, pObj->m_position, DT_CALCRECT);
		}
	}

	// Update scrolling view size to reflect new size of document
	UpdateScrollSize();
}

CDrawObj* CEXView::GetEXObj()
{
	if (m_pExpObj == m_pDiagram)
		return m_pExpSel;
	else
		return m_pExpObj;

}



void CEXView::HighlightHint(CString strHints)
{
	
	BOOL bNothingFound = TRUE;
	CDrawObj* pObj;
	CString strId;
	CString strCode;
	CString strRest;
	CString strHint;

	// During the asynchronous help call in coach mode, a bad value
	// is returned from lisp.  When the user has just finished 
	// explaining everything, the very next help call returns ||
	// instead of NIL.  This check prevents Andes from crashing.
	if (strHints.IsEmpty())
  		return;

	if (strHints[0] == '\"')//should not happen   
	{
		int len = strHints.GetLength();
	    strHints = strHints.Mid(1, len - 2);
	}
	int nSepPos = strHints.Find('~');
	if (nSepPos == 0){
		strHints = strHints.Mid(1);
		nSepPos = strHints.Find('~');
	}
	while (nSepPos != -1){
		strId = strHints.Mid(0, nSepPos);
		strRest = strHints.Mid(nSepPos + 1);
		nSepPos = strRest.Find('~');
		if (nSepPos == -1 )
			strCode = strRest.Mid(0);
		else
			strCode = strRest.Mid(0, nSepPos);
		POSITION pos = GetDocument()->m_objects.GetHeadPosition();
		while (pos != NULL){
			 pObj  = GetDocument()->m_objects.GetNext(pos);
			 if (pObj->m_strId == strId){
				bNothingFound = FALSE;
				if (pObj->IsKindOf(RUNTIME_CLASS(CLabel)))
					((CLabel*)pObj)->m_bHint = TRUE;

				// and find the selected string in the appropriate pop-up menu
				pObj->m_pEXInfo->m_checkList.RemoveAll();//if "L" nothing checked

				int	menuID = GetExMenu(FALSE, pObj);//Get menu, but don't show it
				if ((strCode.Find('T') != -1) || (strCode.Find('R')!=-1)){
					pObj->m_pEXInfo->m_checkList.AddTail(menuID);
				}
				if (strCode.Find('P') != -1){
					pObj->m_pEXInfo->m_checkList.AddTail(40006);
				}
				
				
				break;
			}//need invalidate?? button by last item. right item uncovered
		}
			
		if (bNothingFound)
		{
			TRACE("No match found for %s\n", strId);
			return;
		}
		
		strHints = strRest.Mid(nSepPos + 1);
		nSepPos = strHints.Find('~');
	}
	
	Invalidate();
	
}

/* Not used anymore, was only used when hinting one object at a time

void CEXView::EnsureObjectVisible(CDrawObj* pObj)
{
	CRect objPos = pObj->m_position;
	CRect rect;
	GetClientRect(&rect);
	ClientToDoc(rect);
	if (objPos.bottom > rect.bottom){
		while (objPos.bottom > rect.bottom){
			SendMessage(WM_VSCROLL, SB_LINEDOWN, ((LONG)(GetScrollBarCtrl(SB_VERT))));
			Invalidate();
			GetClientRect(&rect);
			ClientToDoc(rect);
		}
	}
	else if (objPos.top < rect.top){
		while (objPos.top < rect.top){
			SendMessage(WM_VSCROLL, SB_LINEUP, ((LONG)(GetScrollBarCtrl(SB_VERT))));
			Invalidate();
			GetClientRect(&rect);
			ClientToDoc(rect);
		}
	}
	return;
}
*/

// Note this code gets explain menu resource id of "current" item (visible region
// or selected graphic object if inside diagram), 0 if none. 
int CEXView::GetExMenu(BOOL bShow, CDrawObj* pObj)//if bShow == True, shows menu
{	

	CString menuItemStr[2];//these two strings need to persist in memory 
	//until our owner drawn menu is drawn
	//we pass a reference to the strings in ModifyMenu;

	// Implemented in GetTypeName
	// for text items, get identifying menu string into strMenu.
	// if in diagram mode, choose menu based on object type
	CString strMenu;
	pObj->GetTypeName(strMenu);
	
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_EXPLAIN));
	UINT nItems = menu.GetMenuItemCount();
			
	// Search to find submenu in popup
	CString strItem;
	UINT nSubMenu;
	for (nSubMenu = 0; nSubMenu < nItems; nSubMenu++) {
	menu.GetMenuString(nSubMenu, strItem, MF_BYPOSITION);
		if (strItem == strMenu)
			break;
	}
	if (nSubMenu >= nItems) {	// Didn't find it
		TRACE("Didn't find explain menu |%s|\n", strMenu);
		return -1;
	}
	// and find the selected string in the appropriate pop-up menu
	CMenu* pPopup = menu.GetSubMenu(nSubMenu);

	CPinkMenu mnuPink;
	mnuPink.Attach(pPopup->GetSafeHmenu());
	
	if (bShow){//show the menu
		CEXInfo* pInfo = pObj->GetEXInfo();
		if (!pInfo->m_checkList.IsEmpty()){
			int i =0;
			POSITION pos = pInfo->m_checkList.GetHeadPosition();
			while (pos != NULL){
				//only checked menu items are owner-draw
				int nChkID = pInfo->m_checkList.GetNext(pos);
				mnuPink.GetMenuString(nChkID, menuItemStr[i], MF_BYCOMMAND);
				mnuPink.ChangeMenuItem(nChkID, (LPCTSTR)menuItemStr[i]);
				i++;
			}
		}
		
		CRect btnPos;
		m_pDlg->GetWindowRect(&btnPos);
		// and do the appropriate pop-up menu
		ASSERT(mnuPink != NULL);
		CWnd* pWndPopupOwner = this;
		while (pWndPopupOwner->GetStyle() & WS_CHILD)
			pWndPopupOwner = pWndPopupOwner->GetParent();
		//returns non-zero if succeeds, 0 if fails
		return mnuPink.TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, 
			btnPos.left, btnPos.bottom, pWndPopupOwner);
	}
	else//returns menu id of first item in the popup menu.
		return  pPopup->GetMenuItemID(0);
}


void CEXView::ResetView()
{
	m_bExplaining = FALSE;
	CDrawObj* pExpObj;
	if (m_pExpObj == m_pDiagram)
		pExpObj = m_pExpSel;
	else
		pExpObj = m_pExpObj;
	
	LPCTSTR lpszResult = HelpSystemExecf("(is-item-explained |%s|)", pExpObj->m_strId);
	if (lpszResult){
		if (strcmp(lpszResult, "T")==0){
			if (pExpObj->IsKindOf(RUNTIME_CLASS(CLabel)))
				((CLabel*)pExpObj)->m_bHint = FALSE;
			pExpObj->m_pEXInfo->m_bExplained = TRUE;				
		}
		else{
			pExpObj->m_pEXInfo->m_bExplained = FALSE;
		}
	}

	if (m_bCoachMode){
		//when in coachmode, need to know what items still need 
		//to be explained.  This changes as we explain other items
		LPCTSTR lpszResult = HelpSystemExecf("(update-hints)");
		if (lpszResult){
			POSITION pos = GetDocument()->m_objects.GetHeadPosition();
			while (pos != NULL){//reset all hints to FALSE
			 CDrawObj* pObj  = GetDocument()->m_objects.GetNext(pos);
			 if (pObj->IsKindOf(RUNTIME_CLASS(CLabel)))
				((CLabel*)pObj)->m_bHint = FALSE;
			}
			CString strHint = lpszResult;
			int lastpos = strHint.GetLength() - 1;
			if (strHint[1] == '|')
				strHint = strHint.Mid(1, lastpos);	//start after "|
			HighlightHint(strHint);//highlight new ones

		}
	}
	m_pVisibleObj = m_pExpObj;
	if (m_pVisibleObj == m_pDiagram){
		m_pSelection = m_pExpSel;
		UpdateBtnPos(m_pExpSel);
	}
	else
		UpdateBtnPos(m_pVisibleObj);
	m_pDlg->ShowWindow(SW_SHOWNA);
	CRect rect;
	rect.CopyRect(m_pExpObj->m_position);
	DocToClient(rect);
//	if (m_pExpObj != m_pVisibleObj){
//		HideRegion(m_pExpObj);
//	}
	m_pExpObj = NULL;
	m_pExpSel = NULL;
	HideCtrlObjs();
	InvalidateRect(&rect, FALSE);
	SetFocus();
//	UpdateWindow();
}

void CEXView::HideCtrlObjs()
{
	m_bCtrlMode = FALSE;
	if ((m_pVisibleObj != m_pVisible2nd) && (m_pVisible2nd != NULL)){
		HideRegion(m_pVisible2nd);
		//Notify help system
		CString strId = IsMask(m_pVisible2nd) ? MaskTypeStr(m_pVisible2nd) : m_pVisible2nd->m_strId;
		if (m_bCoachMode)
			CheckStudyItem(strId);
		else
			(void)HelpSystemSendf("(cover-item |%s|)", strId);
		m_pVisible2nd = NULL;

	}
	if ((m_pVisibleObj != m_pVisible3rd) && (m_pVisible3rd != NULL)){
		HideRegion(m_pVisible3rd);
		//Notify help system
		CString strId = IsMask(m_pVisible3rd) ? MaskTypeStr(m_pVisible3rd) : m_pVisible3rd->m_strId;
		if (m_bCoachMode)
			CheckStudyItem(strId);
		else
			(void)HelpSystemSendf("(cover-item |%s|)", strId);
		m_pVisible3rd = NULL;
	}
}


//
// OnExResult: callback function for handling asynchronous DDE completions.
//
// Note because it is a callback, it can't be a non-static member fn, which
// requires an implicit "this" argument.
// 
	
//
// EXCTX: Example query context record. Pointer is passed to async 
// completion routine. Packages the info the callback needs to process result.
//
typedef struct 
{
	CEXView* pView;				// The view that generated the query
	CString	 strId;				// example item string ID
} EXCTX;

//
// CheckStudyItem:	When we are in coach mode, we check the item we
//					are studying to see if we have read it 
//					carefully enough.  And if we have, how does 
//					that affect the others we are required to study
//
void CEXView::CheckStudyItem(CString idExStr)
{
	// queue request using ASYNC communication
	CString strCmd;
	strCmd.Format("(cover-item |%s|)", idExStr);
	
	// package up context record for completion function arg
	EXCTX* pCtx = new EXCTX; 
	pCtx->pView = this;
	pCtx->strId = idExStr;
	HelpSystemExecAsync(strCmd, CEXView::OnExResult, (DWORD) pCtx);
	// !!! Might want to save list of pending transactions
}

//
// OnEqResult: Callback to handle async validation completion: 
// arg is ptr to EXCTX context record 
//
void CEXView::OnExResult (DWORD dwContext, LPCTSTR pszResult)	
{
	// unpack arg to restore context
	EXCTX* pCtx = (EXCTX *) dwContext;
	CEXView* pView = pCtx->pView;
	CString idEx = pCtx->strId;
	delete pCtx; 			// Now done with it.

	// Make sure the view of the query is still valid!
	if (pView != theApp.GetEXView()) {
		TRACE("OnExResult completion: XACT not for current view; ignored\n");
		return;
	}

	// Log async result
	TRACE("DDE: Got Async Result: |%s|\n", pszResult ? pszResult : "");

	if (pszResult == NULL || strlen(pszResult) == 0)
		return;
	if (strcmp(pszResult, "NIL") == 0)
		return;
	else{
		POSITION pos = pView->GetDocument()->m_objects.GetHeadPosition();
		while (pos != NULL){//reset all hints to FALSE
		 CDrawObj* pObj  = pView->GetDocument()->m_objects.GetNext(pos);
		 if (pObj->IsKindOf(RUNTIME_CLASS(CLabel)))
			((CLabel*)pObj)->m_bHint = FALSE;
		}
		pView->HighlightHint(pszResult);
	}
}

void CEXView::RevampBtn(CDrawObj* pObj)
{
	CRect rect;
	int width = 68;
	CString btnText = "Self-Explain";
	m_pDlg->GetWindowRect(&rect);
	if (pObj->IsKindOf(RUNTIME_CLASS(CLabel))){
		CLabel* pTxtObj = (CLabel*)pObj;
		if (pTxtObj->m_bHint){
			int nCount = pObj->m_pEXInfo->m_checkList.GetCount();
			if (nCount == 0){
				width = 130;
				btnText = "Read Carefully";
			}
			else{ 
				width = 140;
				if (nCount == 1){
					int id = pObj->m_pEXInfo->m_checkList.GetHead();
					CString str;
					if (id == 40006)
						str = "Plan Browser";
					else
						str = "Rule Browser";
					btnText.Format("Self-Explain with the %s", str);
				}
				else{
					btnText ="Self-Explain with BOTH Rule and Plan Browser";
				}
			}
		}
	}
	CDC* pDC = GetDC();
	//so that we get the proper text extent
	CRect rcText = CRect(0, 0, width, 10);
	pDC->DrawText(btnText, -1, rcText, DT_CALCRECT|DT_WORDBREAK );

	m_pDlg->MoveWindow(rect.left, rect.top, rcText.right+4, rcText.bottom+4);
	m_pDlg->m_Ok.MoveWindow(0, 0, rcText.right+4, rcText.bottom+4);
	m_pDlg->m_Ok.SetWindowText(btnText);
		
}

void CEXView::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if (nChar == VK_CONTROL)
	{
		m_bCtrlMode = TRUE;
		
	}
	
	CBaseView::OnKeyDown(nChar, nRepCnt, nFlags);
}

void CEXView::OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if (nChar == VK_CONTROL)
	{
		HideCtrlObjs();
		
	}
	CBaseView::OnKeyUp(nChar, nRepCnt, nFlags);
}

