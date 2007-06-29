// ChildFrm.cpp : implementation of the CChildFrame class
//
// $Id: ChildFrm.cpp,v 1.7 2007/06/29 21:43:19 anders Exp $
    
#include "stdafx.h"
// #include <afxrich.h>
    
#include "FBD.h"
#include "history.h"
#include "Mainfrm.h"
#include "FBDDoc.h"
#include "FBDView.h"
#include "EQView.h"
#include "EXPlanVw.h"
#include "EXView.h"
#include "TabView.h"
#include "HintView.h"
#include "ChatView.h"
#include "PrincView.h"
#include "ChildFrm.h"
#include "VarView.h"
/*    
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
    static char THIS_FILE[] = __FILE__;
#endif*/
    
/////////////////////////////////////////////////////////////////////////////
// CChildFrame
    
IMPLEMENT_DYNCREATE(CChildFrame, CMDIChildWnd)
    
BEGIN_MESSAGE_MAP(CChildFrame, CMDIChildWnd)
	//{{AFX_MSG_MAP(CChildFrame)
	ON_WM_CREATE()
	ON_COMMAND(ID_WINDOW_MINIMIZERIGHT, OnWindowMinimizeright)
	ON_COMMAND(ID_ZDIR_MENU, OnZDirMenu)
	ON_UPDATE_COMMAND_UI(ID_VIEW_PROBLEMBAR, OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_PROBLEMBAR, OnBarCheck)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
    
/////////////////////////////////////////////////////////////////////////////
// CChildFrame construction/destruction
    
CChildFrame::CChildFrame()
{
	m_bActivated = FALSE;
	m_pDoc = NULL;
}
    
CChildFrame::~CChildFrame()
{
	if (AfxGetThreadState()->m_pRoutingFrame == this)
	{
		AfxGetThreadState()->m_pRoutingFrame = NULL;
	}
    	
}
    
BOOL CChildFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

#if 0 // following don't work as desired, not sure why.
	// problems should come up maximized -- now handled in ActivateFrame
	cs.style |= WS_MAXIMIZE;

	if (! theApp.m_bAuthorMode) // student-opened: shouldn't minimize or maximize
	{
		cs.style &= ~WS_THICKFRAME;
		cs.style |= WS_BORDER;
		// remove the minimize and maximize buttons
		cs.style &= ~(WS_MINIMIZEBOX|WS_MAXIMIZEBOX);
	}
#endif 0

	return CMDIChildWnd::PreCreateWindow(cs);
}

//
// OnCreateClient::Create client area contents for a new document frame.
// Used to set up appropriate splitter panes based on file type.
// 
BOOL CChildFrame::OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext) 
{
	// save ptr to our document for easy access when activating frame (see below)
	ASSERT(pContext->m_pCurrentDoc->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	m_pDoc = (CFBDDoc*) pContext->m_pCurrentDoc;

	// Switch on 3 major cases: quant problem, qual problem, or example. 
	// !!! Could change to use subclasses, but for now this is fine

	// Examples distinguished by file extension set in template (most reliable)
	CString strExt;			
	pContext->m_pNewDocTemplate->GetDocString(strExt, CDocTemplate::filterExt);
	if (strExt == ".APX")	
		return CreateExamplePanes(lpcs, pContext);

	// Else get type from problem type field set in document
	// Make sure opened with our custom doc template that preloads document
 	ASSERT(pContext->m_pNewDocTemplate->IsKindOf(RUNTIME_CLASS(CFBDDocTemplate)));
 	if (m_pDoc->m_nProblemType == PROB_QUAL) 
 		return CreateQualProbPanes(lpcs, pContext);
 	
	// else assume QUANT problem. Use as default in case author left unset in doc
	if (m_pDoc->m_nProblemType != PROB_QUANT) {
		TRACE("CChildFrame::OnCreateClient -- Bad problem type %d\n", m_pDoc->m_nProblemType);
	}
	return  CreateQuantProbPanes(lpcs, pContext);
}

// Max size used to grab all space in pane layout algorithm:
#define MAX_PANE_SIZE 32767

BOOL CChildFrame::CreateQuantProbPanes(LPCREATESTRUCT lpcs, CCreateContext* pContext)
{
/*	OLD:	
	// Problem files require two splitters, three views
	// Create diagram view in the left pane (row 0, column 0) 
	// Initial width is used in layout algorithm which goes from
	// left to right. Height shouldn't matter, since will expand to fill row
*/
	// Now problem files split four ways. Top-level split is left-right.
	// We will use nested splitters to put diagram aabove hint pane on left,
	// variable above equation pane on right.
	
	// create top-level (left-right) splitter with 1 row, 2 columns  
	if (! m_splitLeftRight.CreateStatic(this, 1, 2)) {
		TRACE0("Failed to create left-right splitter\n");
		return FALSE;
	}
    
/*	// Following creates an FBDView in the left half of top-level splitter
	if (! m_splitLeftRight.CreateView(0,0, RUNTIME_CLASS(CFBDView),
									CSize(500, 0), pContext)){
		TRACE0("Failed to create first view pane\n");
		return (FALSE);
	} 
*/
	// Now add the left nested splitter pane. It is itself a top/bottom 
	// splitter(2 rows, 1 col) and goes in first row, first column of parent
	if (!m_splitTopBotLeft.CreateStatic(
		&m_splitLeftRight,					// our parent window is the first splitter
		2, 1,								// the new splitter has 2 rows, 1 column
		WS_CHILD | WS_VISIBLE | WS_BORDER,  // style, WS_BORDER is needed
		m_splitLeftRight.IdFromRowCol(0, 0)	// magic, sets pane id to that of 
											//first row, 2nd column of first splitter
	   )) {
		TRACE0("Failed to create left nested splitter\n");
		return FALSE;
	}
	// Now create and install two views inside the left half splitter
	// Layout algorithm assigns space from left to right and top to bottom, so only
	// initial height set for the top one will affect initial top-bot layout.
	// Set init top-pane height to huge so hint view is initially hidden -- will popup when needed
	// !!! can now get "ideal" width based on drawing extent from document -- 
	// left-right split size is now adjusted in OnActivateFrame, could be done here.
#if 0 // was: !ATLAS.   But now we use chat view in all cases.
	if (! m_splitTopBotLeft.CreateView(0,0, RUNTIME_CLASS(CFBDView),
									CSize(500, MAX_PANE_SIZE), pContext)){
		TRACE0("Failed to create top-left pane\n");
		return (FALSE);
	}
	if (! m_splitTopBotLeft.CreateView(1,0, RUNTIME_CLASS(CHintView),
									CSize(0, 0), pContext)) {
		TRACE0("Failed to create bottom-left view pane\n");
		return (FALSE);
	}
#else // using dialog interface.
	// For some odd reason the following bit of code gives a nise sized window height for the FBDView with
	// the ChatView.  I played with a couple of multipliers and 1 was the best under a range of different
	// window sizes. --MAR
	CRect wndRect;
	GetClientRect(wndRect);
	wndRect.NormalizeRect(); // Safety feature (to make sure Widths and Heights are not negative)
	int rectHeight = wndRect.Height();

	if (! m_splitTopBotLeft.CreateView(0,0, RUNTIME_CLASS(CFBDView),
									CSize(500, rectHeight), pContext)){
		TRACE0("Failed to create top-left pane\n");
		return (FALSE);
	}
	if (! m_splitTopBotLeft.CreateView(1,0, RUNTIME_CLASS(CChatView),
									CSize(0, 0), pContext)) {
		TRACE0("Failed to create bottom-left view pane\n");
		return (FALSE);
	}
#endif // using dialog interface

	// Now add the right nested splitter pane. It is itself a top/bottom 
	// splitter(n rows, 1 col) and goes in first row, second column of parent

	// Until ready, experimental princ wnd requires registry option to turn on
	int nRightRows = theApp.m_bUsePrincipleWnd ? 3 : 2;
	if (!m_splitTopBotRight.CreateStatic(
		&m_splitLeftRight,						// our parent window is the first splitter
		nRightRows, 1,							// the new splitter has n rows, 1 column
		WS_CHILD | WS_VISIBLE | WS_BORDER,  // style, WS_BORDER is needed
		m_splitLeftRight.IdFromRowCol(0, 1)	// magic, sets pane id to that of 
											//first row, 2nd column of first splitter
	   )){
		TRACE0("Failed to create right nested splitter\n");
		return FALSE;
	}
	int iRow = 0;	// index of next row to fill
	//
	// Now create and install two views inside the nested splitter
	// Only the initial height of the first one will affect initial layout.
	// 
///	if (! m_splitTopBotRight.CreateView(0,0, RUNTIME_CLASS(CPlanView),
///									 CSize(0, 250), pContext)){
///		TRACE0("Failed to create second view pane\n");
///		return (FALSE);
///	}
    /*
	if (! m_splitTopBotRight.CreateView(0,0, RUNTIME_CLASS(CVarView),
									 CSize(0, 130), pContext)){
		TRACE0("Failed to create right-top view pane\n");
		return (FALSE);
	}*/
	if (theApp.m_bUsePrincipleWnd) {
		if (! m_splitTopBotRight.CreateView(iRow++,0, RUNTIME_CLASS(CPrincView),
									 CSize(0, 100), pContext)){
			TRACE0("Failed to create right-top view pane\n");
			return (FALSE);
		}
	}

	if (! m_splitTopBotRight.CreateView(iRow++,0, RUNTIME_CLASS(CTabView),
									 CSize(0, 200), pContext)){
		TRACE0("Failed to create right-top view pane\n");
		return (FALSE);
	}
/*// code to create simple EQView pane
	if (! m_splitTopBotRight.CreateView(2,0, RUNTIME_CLASS(CEQView),
									CSize(0, 0), pContext)) {
		TRACE0("Failed to create left-top view pane\n");
		return (FALSE);
	} */
	// create dynamic splitter to hold EQ pane, parented to static split, with rowcol id
	// Variant CreateContext tells dyn splitter to create initial CEQView
	CCreateContext ctxEQ = *pContext;				// init by member-wise copy of original.
	ctxEQ.m_pNewViewClass = RUNTIME_CLASS(CEQView);	// modify to create EQView.
	if (! m_splitEQ.Create(&m_splitTopBotRight, 2, 1, CSize(1, 1), &ctxEQ,
		      WS_CHILD | WS_VISIBLE | WS_VSCROLL | SPLS_DYNAMIC_SPLIT /*|WS_HSCROLL*/,
			  m_splitTopBotRight.IdFromRowCol(iRow++, 0))){
		TRACE0("Failed to create eq pane dynamic splitter\n");
		return FALSE;
	}

	// else everything worked:
	return TRUE;
}

BOOL CChildFrame::CreateQualProbPanes(LPCREATESTRUCT lpcs, CCreateContext * pContext)
{
	// For Qual problems, just have diagram view and hint pane in a top/bottom split.

	// We put them inside m_splitTopBotLeft so that same hint pane showing code 
	// which manipulates this works in both quant and qual prob cases. Note that
	// in the Qual case this is the top-level splitter, not a nested one, and there 
	// is no TopBotRight.
	
	// create top-level top-bottom) splitter with 2 rows, 1 columns  
	if (! m_splitTopBotLeft.CreateStatic(this, 2, 1)) {
		TRACE0("Failed to create top-bottom splitter\n");
		return FALSE;
	}
	// Now create and install two views inside the  splitter, as in quant problem
	// Layout algorithm assigns space from left to right and top to bottom, so only
	// initial height set for the top one will affect initial top-bot layout.
	// Set init top-pane height to huge so hint view is initially hidden -- will popup when needed
	// width irrelevant since expands right to fill top row.
#if 0 // old
	if (! m_splitTopBotLeft.CreateView(0,0, RUNTIME_CLASS(CFBDView),
									CSize(500, MAX_PANE_SIZE), pContext)){
		TRACE0("Failed to create top-left view pane\n");
		return (FALSE);
	}
	if (! m_splitTopBotLeft.CreateView(1,0, RUNTIME_CLASS(CHintView),
									CSize(0, 0), pContext)) {
		TRACE0("Failed to create bottom-left view pane\n");
		return (FALSE);
	}
#else // NEW
	// For some odd reason the following bit of code gives a nice sized window height for the FBDView with
	// the ChatView.  I played with a couple of multipliers and 1 was the best under a range of different
	// window sizes. --MAR
	CRect wndRect;
	GetClientRect(wndRect);
	wndRect.NormalizeRect(); // Safety feature (to make sure Widths and Heights are not negative)
	int rectHeight = wndRect.Height();

	if (! m_splitTopBotLeft.CreateView(0,0, RUNTIME_CLASS(CFBDView),
									CSize(500, rectHeight), pContext)){
		TRACE0("Failed to create top-left pane\n");
		return (FALSE);
	}
	if (! m_splitTopBotLeft.CreateView(1,0, RUNTIME_CLASS(CChatView),
									CSize(0, 48), pContext)) {
		TRACE0("Failed to create bottom-left view pane\n");
		return (FALSE);
	}
#endif // NEW
	
	return TRUE;
}

BOOL CChildFrame::CreateExamplePanes(LPCREATESTRUCT lpcs, CCreateContext* pContext)
{
//	if ( theApp.m_wHelpFlags & fExample){//see if allow example study help
    
		// create first (left-right) splitter with 1 row, 2 columns  
		if (! m_splitLeftRight.CreateStatic(this, 1, 2)) {
			TRACE0("Failed to create first splitter\n");
			return FALSE;
		}
		//if example edit view, create FBDView in first pane
		if (pContext->m_pNewDocTemplate==theApp.m_ptmplExEdit){
			if (!m_splitLeftRight.CreateView(0,0, RUNTIME_CLASS(CFBDView),
				CSize(600,0), pContext)){
				TRACE0("Failed to create first view pane\n");
				return (FALSE);
			}
			//create plan view in second row, second pane (column1)
			if (! m_splitLeftRight.CreateView(0,1, RUNTIME_CLASS(CPlanView),
										 CSize(0, 0), pContext)){
				TRACE0("Failed to create third view pane\n");
				return (FALSE);
			}
		}


		//if not editing an example or creating a new example
		//Create example view in first pane (column0)
		else{
			if (! m_splitLeftRight.CreateView(0,0, RUNTIME_CLASS(CEXView),
											CSize(675, 0), pContext)){
				TRACE0("Failed to create first view pane\n");
				return (FALSE);
			}
			//create plan view in second row, second pane (column1)
			if (! m_splitLeftRight.CreateView(0,1, RUNTIME_CLASS(CEXPlanVw),
									 CSize(0, 0), pContext)){
				TRACE0("Failed to create third view pane\n");
				return (FALSE);
			}
		}						
//	}
//	else // no splitters, single view
//	{
//		return CMDIChildWnd::OnCreateClient(lpcs, pContext);
//	}
		return (TRUE);//everything worked
}

 
// 
// Following method copied from FAQ to maximize MDI child windows on
// first "ActivateFrame" event. Now extended to adjust problem frame and splitter 
// attributes based on problem type. Too much switching on problem type and it might 
// make sense to create another document type with a different frame class. (Currently 
// qualitative problems use same document type as quantitative, but have slightly 
// different interface).
//
void CChildFrame::ActivateFrame(int nCmdShow) 
{
	if (m_bActivated) 		// has already been activated once:
	{
		CMDIChildWnd::ActivateFrame(nCmdShow);
		return;				// don't do anything more.
	}

	// Else problem frame is being activated for the first time:
	// make problem-dependent adjustments now that problem is being shown
	m_bActivated = TRUE;	// don't do this again. (Note actual activation is at end)

	// set to come up maximized in MDI Frame
	nCmdShow = SW_SHOWMAXIMIZED;
    
	// Add problem-specific toolbar to child frame. 

	// We cannot use the methods we put in FBDApp to find our document from the MDI 
	// infrastructure at this point in the document open sequence: although the doc has 
	// been opened and the frame window has been created for it (invisibly), the frame does not become 
	// the current (i.e. active) MDI frame (and so our doc does not become the "active" 
	// doc) until *after* this call returns. Our quick solution is to save a ptr to our 
	// doc in the OnCreateClient method called earlier in the sequence.
	ASSERT(m_pDoc);
	int nID = 0;
	if (m_pDoc->m_nProblemType == PROB_QUAL)	// Qualitative problem
	{
	// In Andes2 qual problem, no tools.
	/*	nID = IDR_QUALTOOLS;		// bar with motion diagram tools. */
	}
	else if (m_pDoc->m_nProblemType == PROB_QUANT				// Quantitative problem
		     && !(m_pDoc->m_wConcept & ID_PROB_PROBABILITY))    // but no drawing tools for probability
	{
		if (CVarView::HasFeature(CString("DIPOLE"))) // new: EM problems w/dipole vectors
			nID = IDR_DIPOLETOOLS;
		else if (m_pDoc->m_wConcept & ID_PROB_ROTKINEMATICS)
			nID = IDR_ROTKINTOOLS;	// bar with special tools for rotational probs
		else if (CVarView::HasFeature(CString("OPTICS")) ||
			     CVarView::HasFeature(CString("NEW-OPTICS")) ) 
			nID = IDR_OPTICSTOOLS;
		else  if (m_pDoc->m_wConcept & ID_PROB_EM)
			nID = IDR_EMTOOLS;      // bar with special tools for E&M problems
		else 
			nID = IDR_QUANTBAR;		// standard quantitative problem bar
	}
			
	if (nID != 0) // have a problem-type-specific bar
	{
		if (!m_wndProblemBar.Create(this, WS_CHILD | WS_VISIBLE |
			 CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC, 
			 (UINT) IDW_PROBLEMBAR) 
			|| ! m_wndProblemBar.LoadToolBar(nID))
			TRACE0("Failed to create problem toolbar\n");
		else	// created problem bar
		{
			// Initially docked at the left edge.
			m_wndProblemBar.EnableDocking(CBRS_ALIGN_ANY);
			DockControlBar(&m_wndProblemBar, AFX_IDW_DOCKBAR_LEFT);
		}
	}

    			
	// Adjust frame border widgets for example mode:
	if (m_pDoc->m_nProblemType == PROB_EXAMPLE) 
	{
		// Tell mainframe to dump tool- & status bars since we need maximum space.
		((CMainFrame*) AfxGetMainWnd())->HideBars();
	}
	else // ensure they're showing, since we may have hidden them earlier
		((CMainFrame*) AfxGetMainWnd())->ShowBars();
    
	// Adjust splitter pane layout: 
	// !!! Could be done at splitter creation now that we load problem data first.
    CSize size = m_pDoc->GetSize();

	// For problems & examples, set main splitter width based on problem size
	// Note that in some conditions, splitter not created (examples w/o exhelp)
	if (m_pDoc->m_nProblemType == PROB_EXAMPLE ||
		m_pDoc->m_nProblemType == PROB_QUANT)
	{
		// Adjust width of problem/example pane to max x extent of drawings.
		
		// ensure a reasonable minimum width for new empty problems
		if (size.cx <=  nLUsPerInch)
			size.cx = 4 * nLUsPerInch;
		// size is in logical units, splitter needs pixels
		CClientDC dc(this);
		int cxPixels = MulDiv(size.cx, dc.GetDeviceCaps(LOGPIXELSY), nLUsPerInch);
		// set "ideal" size of splitpane, leaving room for vert. scroll bar width,
		// plus one more scrollbar width as a border.
		if (m_splitLeftRight.GetSafeHwnd() != NULL) // make sure splitter exists! 
		{
			m_splitLeftRight.SetColumnInfo(0, cxPixels + 2*::GetSystemMetrics(SM_CXVSCROLL), 100);
			m_splitLeftRight.RecalcLayout();
		}
	}
	// For QUAL probs: adjust left splitter height to ensure full diagram shows.
	// (Could do this on all problems)
	if (m_pDoc->m_nProblemType == PROB_QUAL 
		|| m_pDoc->m_nProblemType == PROB_QUANT) {
		CClientDC dc(this);
		int cyPixels = MulDiv(size.cy, dc.GetDeviceCaps(LOGPIXELSY), nLUsPerInch)
			          + ::GetSystemMetrics(SM_CXVSCROLL);
		// if pane currently too small, set ideal height to show all of diagram. 
		if (m_splitTopBotLeft.GetSafeHwnd() != NULL) // make sure splitter exists! 
		{
			int cyCur, cyMin;
			m_splitTopBotLeft.GetRowInfo(0, cyCur, cyMin);
			if (cyCur < cyPixels) {
				m_splitTopBotLeft.SetRowInfo(0, cyPixels, 100);
				m_splitTopBotLeft.RecalcLayout();

	/* this attempt doesn't work. returned cyHint is zero at this point (why?) 
				// This could have reduced hint pane to invisibility. If so, loop decrementing
				// top pane size until hint pane size hits minimum.

				int cyHint, cyHintMin;
				m_splitTopBotLeft.GetRowInfo(1, cyHint, cyHintMin); 
				while (cyHint < 48 && cyPixels > 48) { // about 1/2 inch minimum for each.
					cyPixels -= 10; // small increment for minimum change
					m_splitTopBotLeft.SetRowInfo(0, cyPixels, 100);
					m_splitTopBotLeft.RecalcLayout();
					m_splitTopBotLeft.GetRowInfo(1, cyHint, cyHintMin); 
				}
	*/
			} 
	
		}
	}

	
	// Adjust main menu for problem type
	if ((m_pDoc->m_nProblemType == PROB_QUAL)
		|| (m_pDoc->m_nProblemType == PROB_QUANT)){
		CDocTemplate* pTemplate = (m_pDoc)->GetDocTemplate();
		if (pTemplate != NULL)
		{
			ASSERT_KINDOF(CMultiDocTemplate, pTemplate);
			// get shared menu from doc template
			HMENU hMenu = ((CMultiDocTemplate*)pTemplate)->m_hMenuShared;
			// No variable menu for qual problems
			if (m_pDoc->m_nProblemType == PROB_QUAL)
				EnableMenuItem(hMenu, 3, MF_BYPOSITION|MF_GRAYED);
			else
				EnableMenuItem(hMenu, 3, MF_BYPOSITION|MF_ENABLED);

			// delete principle menu entirely if principle window not configured
			// !!! Note this modifies the menu loaded from resources by the doc 
			// template, so affects all future problems as well.
			if (! theApp.m_bUsePrincipleWnd)
			{
				CMenu menu;
				menu.Attach(hMenu);	// to use CMenu funcs
				//look for it by name, since may have already been deleted
				for (unsigned int i = 0; i < menu.GetMenuItemCount(); i++) {
					CString strItem;
					menu.GetMenuString(i, strItem, MF_BYPOSITION);
					if (strItem == "Principles") {
						menu.DeleteMenu(i, MF_BYPOSITION);
						break;
					}
				}
				menu.Detach();
			} else if (m_pDoc->m_nProblemType == PROB_QUAL)
				// Princ wnd configured, but qual problem: gray the menu
				EnableMenuItem(hMenu, 4, MF_BYPOSITION|MF_GRAYED);
		}
	}
	
	// Make fbd view the active view initially, so its commands are enabled
	// even if student hasn't yet clicked in it.
	// !!!! "Current" FBD view returned might not be a child of ours if second
	// problem is being opened as in author mode
	// if (theApp.GetFBDView())
	//	SetActiveView(theApp.GetFBDView());
	
	// And now activate the frame
	CMDIChildWnd::ActivateFrame(nCmdShow);
}
    
//
// Here we create the splitter window and the views in its panes
//
/*
BOOL CChildFrame::OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext) 
{
    
	// avoid splitter creation for example study: don't need equation entry
	// form.
/*
	// !! this doesn't work, since document created but not loaded at this time!
	if (pContext->m_pCurrentDoc!= NULL) {
		CFBDDoc* pDoc = (CFBDDoc*)pContext->m_pCurrentDoc;
		 
		if (pDoc->m_nProblemType == PROB_EXAMPLE) 
			
			// Framework should create a single FBDView on the document
			return CMDIChildWnd::OnCreateClient(lpcs, pContext);
			// !!! but we want an example view! 
	}
*/
/*	// find out if this is an example file
	CString strExt;
	pContext->m_pNewDocTemplate->GetDocString(strExt, CDocTemplate::filterExt);
	if (strExt == ".APX")
		return CMDIChildWnd::OnCreateClient(lpcs, pContext);


	// create first splitter with 1 row, 2 columns
	if (! m_splitLeftRight.CreateStatic(this, 1, 2)) {
		TRACE0("Failed to create splitter\n");
		return FALSE;
	}
	// SetRowInfo( int row, int cyIdeal, int cyMin )
	// SetColumnInfo( int col, int cxIdeal, int cxMin )

	m_splitLeftRight.SetColumnInfo(0, 450, 100 );
	// I don't think the following matters, since height of top row
	// determines layout of rows and columns given that splitter must
	// fit inside maximized child frame. But just in case it helps:
	m_splitLeftRight.SetColumnInfo(1, 200, 100 );
	//
	// put a FBD view in the left splitter pane (row 0, column 0)
	//
	if (! m_splitLeftRight.CreateView(0,0, RUNTIME_CLASS(CFBDView),
									CSize(450, 100), pContext)){
		TRACE0("Failed to create first view pane\n");
		return (FALSE);
	}
	//
	// put an EQ view in the right splitter pane (row 0, column 1)
	//
	if (! m_splitLeftRight.CreateView(0, 1, RUNTIME_CLASS(CEQView),
									CSize(200, 100), pContext)) {
		TRACE0("Failed to create second view pane\n");
		return (FALSE);
	}
	// else everything worked:
    	return TRUE;
}
*/
            
void CChildFrame::HideRight()
{
 	if (! m_splitLeftRight.GetSafeHwnd()) // no-op if splitter doesn't exist.
 		return; 
 
	// Splitter lays out panes left to right, top to bottom, relying on
	// size info specified with:
	//		SetRowInfo( int row, int cyIdeal, int cyMin )
	//		SetColumnInfo( int col, int cxIdeal, int cxMin )
	// 
	// To minimize the right half, we set left half's "ideal" size to huge so 
	// it's given all available space by the layout algorithm.
	// !should save current size for restoring
	m_splitLeftRight.SetColumnInfo(0, MAX_PANE_SIZE, 100);
	m_splitLeftRight.RecalcLayout();
    
}

void CChildFrame::HideHintPane()
{
	if (! m_splitTopBotLeft.GetSafeHwnd()) // no-op if splitter doesn't exist.
 		return; 

	// Set top row's ideal size to max so it's given all available space 
	// by the layout algorithm.
	m_splitTopBotLeft.SetRowInfo(0, MAX_PANE_SIZE, 100);
	m_splitTopBotLeft.RecalcLayout();
} 

// Ensure hint pane is showing. (Called from hint pane when it needs to show itself.)
void CChildFrame::ShowHintPane()
{
	// Query Hint pane for its desired height.
	CHintView* pHint = (CHintView*) theApp.FindView(RUNTIME_CLASS(CHintView));
	if (! pHint) return;
	int cyHintIdeal = pHint->GetIdealHeight();

	// Check current size of pane, since user might have sized it arbitrarily.
//	CRect rcHint;
//	pHint->GetWindowRect(rcHint);
//	if (rcHint.Height() >= cyHintIdeal)// it's big enough, no need to resize.
//		return;

	// Get total splitter pane height in pixels (estimate from frame client size?)
	// int cyTotal = ?;
	CRect rcTopBotLeft;
	m_splitTopBotLeft.GetWindowRect(rcTopBotLeft);
	// total height available is current height - little room for split bar
	int cyTotal = rcTopBotLeft.Height() - 10; // generous estimate

	// Set top pane's ideal height in pixels to total height available
	// minus what the hint pane needs 
	m_splitTopBotLeft.SetRowInfo(0, cyTotal - cyHintIdeal, 100);
	// and force layout update
	m_splitTopBotLeft.RecalcLayout();
}  

// Hide the EQ Pane by reducing its height to zero. 
// Used when upper pane switches to Hi-level Solution view Tab.
void CChildFrame::HideEQPane()
{
	if (! m_splitTopBotRight.GetSafeHwnd()) // no-op if splitter doesn't exist.
 		return; 

	// Set top row's ideal size to max so it's given all available space 
	// by the layout algorithm.
	m_splitTopBotRight.SetRowInfo(1, MAX_PANE_SIZE, 100);
	m_splitTopBotRight.RecalcLayout();
	return;
}

// Ensure EQ pane is showing. 
void CChildFrame::ShowEQPane()
{
	// Query pane for its desired height.
	CEQView* pView = (CEQView*) theApp.FindView(RUNTIME_CLASS(CEQView));
	if (! pView) return;
	int cyHintIdeal = pView->GetIdealHeight();

	// Check current size of pane, since user might have sized it arbitrarily.
	CRect rcEq;
	pView->GetWindowRect(rcEq);
	if (rcEq.Height() >= cyHintIdeal)// it's big enough, no need to resize.
		return;

	// Get total splitter pane height in pixels (estimate from frame client size?)
	// int cyTotal = ?;
	CRect rcTopBotRight;
	m_splitTopBotRight.GetWindowRect(rcTopBotRight);
	// total height available is current height - little room for split bar
	int cyTotal = rcTopBotRight.Height() - 10; // generous estimate

	// Set top pane's ideal height in pixels to total height available
	// minus what the hint pane needs 
	m_splitTopBotRight.SetRowInfo(1, cyTotal - cyHintIdeal, 100);
	// and force layout update
	m_splitTopBotRight.RecalcLayout();


}  
 
/////////////////////////////////////////////////////////////////////////////
// CChildFrame diagnostics
    
#ifdef _DEBUG
void CChildFrame::AssertValid() const
{
	CMDIChildWnd::AssertValid();
}
    
void CChildFrame::Dump(CDumpContext& dc) const
{
	CMDIChildWnd::Dump(dc);
}
    
#endif //_DEBUG
    
/////////////////////////////////////////////////////////////////////////////
// CChildFrame message handlers
    
    
    
int CChildFrame::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CMDIChildWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
    
	// Set frame to allow docking along any border.
	EnableDocking(CBRS_ALIGN_ANY);
    
/* test code puts a dialog bar for object properties on the doc frame 
  
	// Create the property bar
	if (!m_wndPropertyBar.Create(this, IDD_DIALOGBAR, CBRS_TOP, IDD_DIALOGBAR) )
	{
		TRACE0("Failed to create property bar\n");
		return -1;  // fail to create
	}
	m_wndPropertyBar.SetBarStyle(m_wndPropertyBar.GetBarStyle() |
		CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC);
	m_wndPropertyBar.EnableDocking(CBRS_ALIGN_ANY);
	
	// and dock it:
	EnableDocking(CBRS_ALIGN_ANY);
	DockControlBar(&m_wndPropertyBar);

*/
    	
	return 0;
}
    
void CChildFrame::OnWindowMinimizeright() // for testing
{
	HideRight();
	
}

// 
// Hook MFC command routing here so as to give inactive views a chance to handle them.
// This is so drawing tools stay enabled for selection while working in other views.
// Code below follows example of Prosise "Windows Wanderer" p 879. (Note *each* view 
// will also route to document and doc template, slightly wasteful).
//  
BOOL CChildFrame::OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo) 
{
	// Route to standard command targets first. (Tries active view first, then self.
	// View tries self then document, and document tries self then doc template.)
	if (CMDIChildWnd::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
		return TRUE;

/* Changed. The technique works, but:
   Sending top-level commands along route to all views complicates enabling/handling
   of commands which can be handled in several views, such as clipboard or help 
   requests. Each update and command handler for such a command would have to include 
   code to check that the view is active, and if not, cause the command/update to 
   be passed along (via ON_COMMAND_EX for commands and CCmdUI::ContinueRouting() for
   updates.) Rather then have to add this code in several places, we change in one
   place here to just forward the drawtool commands to inactive FBDView.

	// Route to inactive views second (generic code)
	CDocument* pDoc = GetActiveDocument();
	if (pDoc) {
		CView* pActiveView = GetActiveView();	// don't forward to this one
		POSITION pos = pDoc->GetFirstViewPosition();
		while (pos != NULL) {
			CView* pView = pDoc->GetNextView(pos);
			if (pView != pActiveView) {
				// Cast CView up to CCmdTarget because OnCmdMsg erroneously protected in CView
				if (((CCmdTarget*) pView)->OnCmdMsg (nID, nCode, pExtra, pHandlerInfo))
					return TRUE;
			}
		}
	} 
*/
	// Not handled by normal route including active view. If drawing tool selection 
	// command, forward to FBDView. Also for app printing cmds, handled by FBDView.
	// For historical reasons, drawtool commands are not all in one contiguous range. 
	// But it is tricky to change the numerical IDs since they occur in log files
	// (as arguments to the select-tool message).
	if ((nID >= ID_DRAWTOOL_FIRST && nID <= ID_DRAWTOOL_LAST) ||
		(nID >= ID_DRAWVECTOR_FIRST && nID <= ID_DRAWVECTOR_LAST) ||
		(nID >= ID_ZDIR_FIRSTCMD && nID <= ID_ZDIR_LASTCMD) ||
		// 3 contiguous MFC print cmd ids for CViews include ID_FILE_PRINT_DIRECT 
		(nID >= ID_FILE_PRINT && nID <= ID_FILE_PRINT_PREVIEW) )
	{
		CFBDView* pFBDView = theApp.GetFBDView();
		if (pFBDView != NULL)
			// Cast needed because OnCmdMsg erroneously protected in CView
			if (((CCmdTarget*) pFBDView)->OnCmdMsg (nID, nCode, pExtra, pHandlerInfo))
				return TRUE;
	}
	// If variable definition command, forward to Varview. Would also make sense
	// to put range handler for these in document or one of the frames.
	if (nID >= ID_VARIABLE_ADDFIRST && nID <= ID_VARIABLE_ADDLAST) {
		CVarView* pVarView = theApp.GetVarView();
		if (pVarView != NULL)
			if (((CCmdTarget*) pVarView)->OnCmdMsg (nID, nCode, pExtra, pHandlerInfo))
				return TRUE;
	}
	
	// If principle declaration command, forward to Princview. Would also make sense
	// to put range handler for these in document or one of the frames.
	if (nID == ID_PRINCIPLE_ADD) {
		CPrincView* pView = theApp.GetPrincView();
		if (pView != NULL)
			if (((CCmdTarget*) pView)->OnCmdMsg (nID, nCode, pExtra, pHandlerInfo))
				return TRUE;
	}

	// If SolveFor variable command forward to EQView. Also request for popup menu
	// and request to solve for nth item from popup menu
	if (nID == ID_SOLVEFOR || 
		nID == ID_SOLVEFOR_MENU ||
	   (nID >= ID_SOLVEFOR_FIRST && nID <= ID_SOLVEFOR_LAST)) {
		CEQView* pView = theApp.GetEQView();
		if (pView)
			if (((CCmdTarget*) pView)->OnCmdMsg (nID, nCode, pExtra, pHandlerInfo))
				return TRUE;
	}

	return FALSE;		// not handled
}
    

// Handle ZDIR button command in frame, since we own toolbar so can know where
// to popup IN/OUT menu. Actual menu commands handled by ID_SET_ZDIR in view.
void CChildFrame::OnZDirMenu() 
{
	CRect rcBtn, rcBar;
	int nIndex = m_wndProblemBar.CommandToIndex(ID_ZDIR_MENU);
	m_wndProblemBar.GetItemRect(nIndex, rcBtn);	// in pixels relative to toolbar
	m_wndProblemBar.ClientToScreen(&rcBtn);

	// Show popup menu w/variables below button
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_ZDIR));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);
	// note parent = command receiver is childframe, which should be OK.
	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_LEFTBUTTON, rcBtn.right, rcBtn.top,
		this);
}

