// FBDView.cpp : implementation of the CFBDView class
//
// Main class for the ANDES diagram editor pane
//
#include "stdafx.h"
#include <math.h>
#include <winspool.h>      // printer control functions

#include "FBD.h"
#include "history.h"
#include "MainFrm.h"
#include "Childfrm.h"
#include "FBDDoc.h"			// pulls in Drawobj.h
#include "FBDObj.h"
#include "Motion.h"
#include "FBDView.h"
#include "HelpIfc.h"
#include "PlanView.h"
#include "EQView.h"			// For printing equations
#include "HiLevelVw.h"		// for Printing plan
#include "GreekOpts.h"

// dialog box classes:
#include "SysDlg.h"
#include "VecDlg.h"
#include "LabelDlg.h"
#include "HypertxtDlg.h"
#include "VecCpDlg.h"
#include "VecAVDlg.h"
#include "AxesDlg.h"
#include "EXTxtDlg.h"
#include "AuthDlg.h"
#include "MCQDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CFBDView

IMPLEMENT_DYNCREATE(CFBDView, CBaseView)
IMPLEMENT_DYNAMIC(CChoiceBtn, CButton)

#define ID_CHOICE_BASE 10000	// start of id range for runtime choice buttons
#define ID_CHOICE_MAX  10100	// end of range reserved for runtime choice btn ids
#define ID_ANSWER_BASE 10200	// start id range for runtime answer edit boxes
#define ID_ANSWER_MAX  10250	// end of range reserved for answer edit boxes

BEGIN_MESSAGE_MAP(CFBDView, CBaseView)
	ON_WM_CONTEXTMENU()
	//{{AFX_MSG_MAP(CFBDView)
	ON_COMMAND(ID_DRAW_VECTOR, OnDrawVector)
	ON_COMMAND(ID_DRAWVECTOR_ACCELERATION, OnDrawvectorAcceleration)
	ON_COMMAND(ID_DRAWVECTOR_FORCE, OnDrawvectorForce)
	ON_COMMAND(ID_DRAWVECTOR_VELOCITY, OnDrawvectorVelocity)
	ON_COMMAND(ID_DRAWVECTOR_MOMENTUM, OnDrawvectorMomentum)
	ON_COMMAND(ID_DRAWVECTOR_IMPULSE, OnDrawvectorImpulse)
	ON_COMMAND(ID_DRAWVECTOR_UNITVECTOR, OnDrawvectorUnitVector)
	ON_COMMAND(ID_DRAWVECTOR_MAGDIPOLE, OnDrawvectorMagDipole)
	ON_COMMAND(ID_DRAWVECTOR_ELECDIPOLE, OnDrawvectorElecDipole)
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_MOUSEMOVE()
	ON_UPDATE_COMMAND_UI(ID_DRAW_VECTOR, OnUpdateDrawVector)
	ON_COMMAND(ID_LABEL, OnLabel)
	ON_UPDATE_COMMAND_UI(ID_ANGLE, OnUpdateAngle)
	ON_COMMAND(ID_EDIT_DELETE, OnEditDelete)
	ON_UPDATE_COMMAND_UI(ID_EDIT_DELETE, OnUpdateEditDelete)
	ON_COMMAND(ID_EDIT_PROPERTIES, OnEditProperties)
	ON_UPDATE_COMMAND_UI(ID_EDIT_PROPERTIES, OnUpdateEditProperties)
	ON_COMMAND(ID_DECOMPOSE_VECTOR, OnDecomposeVector)
	ON_UPDATE_COMMAND_UI(ID_DECOMPOSE_VECTOR, OnUpdateDecomposeVector)
	ON_COMMAND(ID_EDIT_DUPLICATE, OnEditDuplicate)
	ON_UPDATE_COMMAND_UI(ID_EDIT_DUPLICATE, OnUpdateEditDuplicate)
	ON_COMMAND(ID_EXAMPLE_TEXT, OnExampleText)
	ON_UPDATE_COMMAND_UI(ID_EXAMPLE_TEXT, OnUpdateExampleText)
	ON_COMMAND(ID_HELP_WHATSWRONG, OnHelpWhatswrong)
	ON_UPDATE_COMMAND_UI(ID_HELP_WHATSWRONG, OnUpdateHelpWhatswrong)
	ON_COMMAND(ID_GROUP, OnGroup)
	ON_UPDATE_COMMAND_UI(ID_GROUP, OnUpdateGroup)
	ON_COMMAND(ID_UNGROUP, OnUngroup)
	ON_UPDATE_COMMAND_UI(ID_UNGROUP, OnUpdateUngroup)
	ON_WM_LBUTTONDBLCLK()
	ON_UPDATE_COMMAND_UI(ID_EDIT_COPY, OnUpdateEditCopyCut)
	ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE, OnUpdateEditPaste)
	ON_COMMAND(ID_EDIT_COPY, OnEditCopy)
	ON_COMMAND(ID_EDIT_CUT, OnEditCut)
	ON_COMMAND(ID_EDIT_PASTE, OnEditPaste)
	ON_COMMAND(ID_CANCEL_EDIT, OnCancelEdit)
	ON_COMMAND(ID_VIEW_GRID, OnViewGrid)
	ON_UPDATE_COMMAND_UI(ID_VIEW_GRID, OnUpdateViewGrid)
	ON_COMMAND(ID_ALIGNTOGRID, OnAligntogrid)
	ON_UPDATE_COMMAND_UI(ID_ALIGNTOGRID, OnUpdateAligntogrid)
	ON_UPDATE_COMMAND_UI(ID_MOTION_BODY, OnUpdateMotionBody)
	ON_COMMAND(ID_OLE_INSERT_NEW, OnInsertObject)
	ON_UPDATE_COMMAND_UI(ID_OLE_INSERT_NEW, OnUpdateInsertObject)
	ON_WM_SETFOCUS()
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_CONTEXTMENU()
	ON_COMMAND(ID_OBJECT_MOVEBACK, OnObjectMoveBack)
	ON_COMMAND(ID_OBJECT_MOVEFORWARD, OnObjectMoveForward)
	ON_COMMAND(ID_OBJECT_MOVETOBACK, OnObjectMoveToBack)
	ON_COMMAND(ID_OBJECT_MOVETOFRONT, OnObjectMoveToFront)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVEBACK, OnUpdateSingleSelect)
	ON_UPDATE_COMMAND_UI(ID_DIAGRAM_TOGGLEGROUP, OnUpdateTogglegroup)
	ON_COMMAND(ID_DIAGRAM_TOGGLEGROUP, OnTogglegroup)
	ON_COMMAND(ID_CHOICE_ITEM, OnChoiceItem)
	ON_UPDATE_COMMAND_UI(ID_CHOICE_ITEM, OnUpdateChoiceItem)
	ON_UPDATE_COMMAND_UI(ID_DOC_AREA, OnUpdateDocArea)
	ON_WM_PAINT()
	ON_UPDATE_COMMAND_UI(ID_AUTHORPROPS, OnUpdateAuthorprops)
	ON_COMMAND(ID_AUTHORPROPS, OnAuthorprops)
	ON_WM_ERASEBKGND()
	ON_COMMAND(ID_DRAWVECTOR_COMPONENT, OnDrawvectorComponent)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_COMPONENT, OnUpdateDrawvectorComponent)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_ACCELERATION, OnUpdateDrawvectorAcceleration)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_FORCE, OnUpdateDrawvectorForce)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_VELOCITY, OnUpdateDrawvectorVelocity)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_MOMENTUM, OnUpdateDrawvectorMomentum)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_IMPULSE, OnUpdateDrawvectorImpulse)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_UNITVECTOR, OnUpdateDrawvectorUnitVector)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_MAGDIPOLE, OnUpdateDrawvectorMagDipole)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_ELECDIPOLE, OnUpdateDrawvectorElecDipole)
	ON_COMMAND(ID_EDIT_UNDO, OnEditUndo)
	ON_UPDATE_COMMAND_UI(ID_EDIT_UNDO, OnUpdateEditUndo)
	ON_COMMAND(ID_VIEW_FONT, OnViewFont)
	ON_COMMAND(ID_2DMOTION, On2dmotion)
	ON_UPDATE_COMMAND_UI(ID_2DMOTION, OnUpdate2dmotion)
	ON_COMMAND(ID_HELP_DRAWFORCE, OnHelpDrawforce)
	ON_UPDATE_COMMAND_UI(ID_HELP_DRAWFORCE, OnUpdateHelpDrawforce)
	ON_WM_TCARD()
	ON_COMMAND(ID_HELP_DRAWACCELERATION, OnHelpDrawacceleration)
	ON_UPDATE_COMMAND_UI(ID_HELP_DRAWACCELERATION, OnUpdateHelpDrawacceleration)
	ON_COMMAND(ID_HELP_DRAWBODY, OnHelpDrawbody)
	ON_UPDATE_COMMAND_UI(ID_HELP_DRAWBODY, OnUpdateHelpDrawbody)
	ON_COMMAND(ID_HELP_DRAWVELOCITY, OnHelpDrawvelocity)
	ON_UPDATE_COMMAND_UI(ID_HELP_DRAWVELOCITY, OnUpdateHelpDrawvelocity)
	ON_COMMAND(ID_HELP_DRAWMOTIONBODY, OnHelpDrawmotionbody)
	ON_COMMAND(ID_HYPERTEXT, OnHypertext)
	ON_WM_SETCURSOR()
	ON_COMMAND(ID_DRAWVECTOR_DISPLACEMENT, OnDrawvectorDisplacement)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_DISPLACEMENT, OnUpdateDrawvectorDisplacement)
	ON_COMMAND(ID_HELP_DRAWDISPLACEMENT, OnHelpDrawdisplacement)
	ON_UPDATE_COMMAND_UI(ID_HELP_DRAWDISPLACEMENT, OnUpdateHelpDrawdisplacement)
	ON_UPDATE_COMMAND_UI(ID_LABEL, OnUpdateLabel)
	ON_UPDATE_COMMAND_UI(ID_HYPERTEXT, OnUpdateHypertext)
	ON_UPDATE_COMMAND_UI(ID_POLYBEZIER, OnUpdatePolybezier)
	ON_UPDATE_COMMAND_UI(ID_DIAGRAM_COORDINATES, OnUpdateAxes)
	ON_COMMAND(ID_ANGLE, OnAngle)
	ON_UPDATE_COMMAND_UI(ID_ZDIR_MENU, OnUpdateZDirMenu)
	ON_UPDATE_COMMAND_UI(ID_GREEK, OnUpdateGreek)
	ON_COMMAND(ID_DRAWVECTOR_TORQUE, OnDrawvectorTorque)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_TORQUE, OnUpdateDrawvectorTorque)
	ON_COMMAND(ID_DRAWVECTOR_RELPOS, OnDrawvectorRelpos)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_RELPOS, OnUpdateDrawvectorRelpos)
	ON_UPDATE_COMMAND_UI(ID_HELP_DRAWMOTIONBODY, OnUpdateHelpDrawmotionbody)
	ON_COMMAND(ID_INSERTPICTURE, OnInsertpicture)
	ON_UPDATE_COMMAND_UI(ID_INSERTPICTURE, OnUpdateInsertpicture)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVEFORWARD, OnUpdateSingleSelect)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVETOBACK, OnUpdateSingleSelect)
	ON_UPDATE_COMMAND_UI(ID_EDIT_CUT, OnUpdateEditCopyCut)
	ON_UPDATE_COMMAND_UI(ID_OBJECT_MOVETOFRONT, OnUpdateSingleSelect)
	ON_COMMAND(ID_DRAWVECTOR_EFIELD, OnDrawvectorEfield)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_EFIELD, OnUpdateDrawvectorEfield)
	ON_COMMAND(ID_DRAWVECTOR_BFIELD, OnDrawvectorBfield)
	ON_UPDATE_COMMAND_UI(ID_DRAWVECTOR_BFIELD, OnUpdateDrawvectorBfield)
	//}}AFX_MSG_MAP
	// Drawing tool selection handled with a command range
	ON_COMMAND_RANGE(ID_DRAWTOOL_FIRST, ID_DRAWTOOL_LAST, OnSelectDrawTool)
	ON_UPDATE_COMMAND_UI_RANGE(ID_DRAWTOOL_FIRST, ID_DRAWTOOL_LAST, OnUpdateDrawTool)
	ON_WM_CTLCOLOR()
	// Events from run-time choice button controls:
	ON_CONTROL_RANGE(BN_CLICKED, ID_CHOICE_BASE, ID_CHOICE_MAX, OnChoiceClicked)
	// Events from run-time answer box edit controls:
	ON_CONTROL_RANGE(EN_SETFOCUS, ID_ANSWER_BASE, ID_ANSWER_MAX, OnAnswerFocus)
	ON_CONTROL_RANGE(EN_KILLFOCUS, ID_ANSWER_BASE, ID_ANSWER_MAX, OnAnswerKillFocus)
	ON_CONTROL_RANGE(EN_CHANGE, ID_ANSWER_BASE, ID_ANSWER_MAX, OnAnswerChange)
	ON_BN_CLICKED(IDOK, OnAnswerEnter)

	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CView::OnFilePrintPreview)
	// Tooltip text notification callbacks
	ON_NOTIFY_EX( TTN_NEEDTEXT, 0, OnToolTipNotify)
	// For ZDIR_INTO/OUTOF choice
	ON_COMMAND_RANGE(ID_ZDIR_FIRST, ID_ZDIR_LAST, OnSetZDir)
	ON_UPDATE_COMMAND_UI_RANGE(ID_ZDIR_FIRST, ID_ZDIR_LAST, OnUpdateSetZDir)
	// Greek letter insertion commands from the symbol menu:
	ON_COMMAND_RANGE(IDM_GREEKLETTER_FIRST, IDM_GREEKLETTER_LAST, OnInsertGreekLetter)

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CFBDView construction/destruction

CFBDView::CFBDView()
{
	m_bActive = FALSE;
	m_bEnabled = TRUE;
	// m_bHighlight = FALSE;		// Not currently used

	// Default drawing modes:
	m_drawMode = Selector;
	m_pCurrentObj = NULL;
	// default subtypes for some drawing tools:
	m_selectMode = none;
	m_vectorType = -1;		// signals unknown
	m_nZDir = ZDIR_NONE;

	// CG: This line was added by the Clipboard Assistant component
	m_nClipboardFormat = ::RegisterClipboardFormat(_T("FBD Diagram Objects"));

	// Set grid mode defaults:
	// Default is invisible, since Dr. Gertner finds it annoying. But it gets
	// turned on for qualitative problems, since used to measure
	// m_bAlignGrid = TRUE; 
	m_bAlignGrid = FALSE; // Changed Andes8.0.5 -- aligning can be annoying
	m_bShowGrid = FALSE; 

#if 0 // following light blue shade looks good but is not present in default palette
	// for 256 color mode displays. So winds up mapped by windows into invisibility (white).
	enum { GRID_COLOR = RGB(192, 255, 255) };		// a nice light-blue shade 
#else // use a color from the default palette
	// According to Kruglinski the sky blue is one of 4 non-VGA colors in Windows default
	// palette. But it seems to get mapped to a grey on some of our monitors. Prosise says
	// not to rely on its presence since may be changed by operating system, p 771.
	// enum { GRID_COLOR = RGB(166, 202, 240) };	// Windows sky blue
	// enum { GRID_COLOR = RGB(192, 192, 192) };	// VGA standard light grey. 
	enum { GRID_COLOR = RGB(0, 255, 255) };			// cyan
#endif
	m_colorGrid = GRID_COLOR;
	// Grid size computed in OnInitialUpdate

	// No active child edits:
	m_idFocusEdit = -1;
	m_bIgnoreChange = FALSE;
	m_nLastActive = -1;

	m_bOnHyperText = FALSE;

	//Default train mode
}


CFBDView::~CFBDView()
{
	// free C++ objects from our lists of runtime controls
	DestroyControls();

	//make sure winhelp not running in the background
	KillTCard();

	// free default text font we created
	m_fontText.DeleteObject();	// safe if never set
}

void CFBDView::OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView) 
{
	CBaseView::OnActivateView(bActivate, pActivateView, pDeactiveView);
	if (m_bActive != bActivate) // activation status is changing
	{
		// If becoming active, change flag now to update selection as if active. 
		// (Widens update regions for selected objs to include resize handle space).
		if (bActivate)			
			m_bActive = bActivate;

		// mark selection invalid, since it's only highlighted in active views
		if (HaveSelection())
			OnUpdate(NULL, HINT_UPDATE_SELECTION, NULL);
	
		m_bActive = bActivate;
	}
	// Ensure Greek menu hidden if lose activation
	if (! bActivate)
		theApp.GetMainFrame()->HideGreekMenu();
}


/////////////////////////////////////////////////////////////////////////////
//
// CFBDView drawing
//
/////////////////////////////////////////////////////////////////////////////

// Our document's logical coordinate system:
//
// Documents were originally designed using device coordinates on our displays 
// which used 96 pixels/logical inch. In device coordinates the  origin is at the upper 
// left corner of the display and y-axis coordinates increase *downward*. Here we set up
// a mapping mode to treat that system as a "logical" (device-independent) coordinate
// system, with the origin at the top-left corner of a logical page.
//
// OnPrepareDC uses the MM_ANISOTROPIC mapping mode to set up a scaling transformation
// between a "window" in logical coordinates of 96 by 96 logical units and a "viewport"
// in device coordinates of LOGPIXELSX by LOGPIXELSY (the device's pixels per logical 
// inch). We don't adjust the window origin, nor the sign of these extents:
//
//                                      (0,0)|
//   Logical:        ^  (96,96)     Device: -+------->
//                   |----*                  |\   | Viewport
//                   |  / |                  |  \ |
//                   |/   |                  |----* 
//             ------+------>                |   (LOGPIXELSX, LOGPIXELSY)
//              (0,0)|  Window               V
//                   |
//                   |
//
// In this mode there is no orientation intrinsic to the logical coordinate system 
// beyond what is defined by this mapping. So we might better have drawn the left-hand 
// diagram flipped upside-down around the X axis to match the device orientation.  
// Conceptually, logical y coordinates increase as you go downward on the page, just as 
// device coordinates do on the screen.
//
// For the printer DC, the LOGPIXELS will be different, so the mapping ensures that
// the graphics get printed at the appropriate size. 
// 

void CFBDView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
{
	// In case this is a print DC: before we do anything else,
	// force printer to use landscape mode if required
	if (pDC->IsPrinting() && UseLandscapeLayout()) {
			ForceLandscapePrinting(pDC, pInfo);
	}

	// In CScrollView,  this will adjust viewport origin to match 
	// current scrolled position in the document:
	CBaseView::OnPrepareDC(pDC, pInfo);

	// Now set up to map logical units to this device:
	pDC->SetMapMode(MM_ANISOTROPIC);
	pDC->SetViewportExt(pDC->GetDeviceCaps(LOGPIXELSX),
						pDC->GetDeviceCaps(LOGPIXELSY));
	pDC->SetWindowExt(nLUsPerInch, nLUsPerInch);

	// if custom default font set, select it into DC before all drawing.
	if ((HFONT) m_fontText)
		pDC->SelectObject(&m_fontText);
}

// true if our doc requires landscape layout
BOOL CFBDView::UseLandscapeLayout()
{
	// for now, assume any doc wider than 6 inches is intended to 
	// be landscape printed. Later might add flag to document struct.
	CSize size = GetDocument()->GetSize();
	return (size.cx > 6 * nLUsPerInch);
}

// For dealing with scrolling view:

void CFBDView::UpdateScrollSizes()
{
	//Adapted from DRAWCLI code to set scroll sizes using MM_TEXT mapping mode
	CClientDC dc(this);
	CSize size = GetDocument()->GetSize(); 
	// CSize size(PAGEWIDTH, PAGEHEIGHT);
	size.cx = MulDiv(size.cx, dc.GetDeviceCaps(LOGPIXELSX), nLUsPerInch);
	size.cy = MulDiv(size.cy, dc.GetDeviceCaps(LOGPIXELSY), nLUsPerInch);
	SetScrollSizes(MM_TEXT, size);
}

const int nGridPerInch = 16;	// number of grid divisions per logical inch
const int nVectorGridPerInch = 4; // coarse grid used in vector problems

void CFBDView::OnInitialUpdate() 
{
	CBaseView::OnInitialUpdate();

	// Check device pixels per logical inch (currently for info only)
	CClientDC dc(this);
	int cxDotsPerInch = dc.GetDeviceCaps(LOGPIXELSX);
	int	cyDotsPerInch = dc.GetDeviceCaps(LOGPIXELSY);
	TRACE("DEVICE xDPI = %d; yDPI = %d\n", cxDotsPerInch, cyDotsPerInch);

	// Calculate grid size in logical units  (currently a constant).
	// We use fine-grained 16ths of an inch normally, so that find gradations
	// of vector angles can easily be drawn. But we use a coarse-grained grid
	// in the vector problems so it is easy to count boxes for components.
	if (GetDocument()->m_wConcept & ID_PROB_VECTOR) {
		m_cxGrid = nLUsPerInch/nVectorGridPerInch;
		m_cyGrid = nLUsPerInch/nVectorGridPerInch;
	} else {
		m_cxGrid = nLUsPerInch/nGridPerInch;
		m_cyGrid = nLUsPerInch/nGridPerInch;
	}

#ifdef FBD_SCROLL_VIEW	// scrolling view code
	UpdateScrollSizes();
#endif FBD_SCROLL_VIEW	
	
	// Create appropriate default font for text display. (hardcoded choices!)
	// Examples use a smaller font,to work on 800 X 600 student monitors
	// This must also be used by FBDView when authoring to get layout right
	LOGFONT lfText;
	GetDocument()->GetDefaultFont(&lfText);
	m_fontText.CreateFontIndirect(&lfText);

	// For qualitative problems, always show the grid (really only for motion diagrams)
	// also show for vector arithmetic problems. 
	// AW -- no need to whow grid for Andes2 qual problems.
	if (/*GetDocument()->m_nProblemType == PROB_QUAL || */ 
		(GetDocument()->m_wConcept & ID_PROB_VECTOR)) {
		m_bShowGrid = TRUE;
		m_bAlignGrid = TRUE;
	}

	// In student mode: create any run-time controls in the problem.
	if (! theApp.m_bAuthorMode)
	{
		CreateChoiceBtns();	// buttons controls for multiple choice questions
		CreateAnswerBoxes();// edit controls for answer entry
	}

	// Show FBD object defs in tooltips:
	EnableToolTips();
}

// 
// Coordinate conversions for scrolling view:
//
void CFBDView::ClientToDoc(CPoint& point)
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.DPtoLP(&point);
}

void CFBDView::ClientToDoc(CRect& rect)	// verifies result is normalized
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

void CFBDView::DocToClient(CPoint& point)
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.LPtoDP(&point);
}

void CFBDView::DocToClient(CRect& rect)	// Note: returns normalized rect
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.LPtoDP(&rect);
	// Following from DRAWCLI, not clear if appropriate for us.
	// Guarantees result is normalized client rect, useable in GDI computations.
	// Loses orientation of doc rect if it was needed.
	rect.NormalizeRect();
}

/////////////////////////////////////////////////////////////////
//
// Multiple choice item buttons:
//
// Our representation is as follows: the document contains individual CChoice
// objects, one for each choice. These may be batched into a group with other objects, like
// text, so as to build a multiple choice question with mutually exclusive choices. We have 
// no other representation of a multiple choice question itself as an object. The group ID
// serves as the question ID. The choice  objects store the state data for each choice, 
// i.e. check state and status.
//
// At runtime the view creates a single list of child CChoiceBtn controls, one for each
// choice in the document. Each CChoiceBtn obj ontains back pointers linking them to both 
// their choice objects and to the containing group if any (not all pGroup ptrs shown here):
//
// Doc data:			m_pGroup           View's m_ChoiceBtns
//     CGroup <-----------------------+      |
//        |-CChoice <------------CChoiceBtn -|
//        |-CChoice <------------CChoiceBtn -|
//                                           |
//     CGroup<-----------------------+       |
//        |-CChoice <------------CChoiceBtn -|
//        |-CChoice <------------CChoiceBtn -|
//
// So we can easily get from button control to choice object and to its group object.
// To map from choice object to control, we search view's list with FindBtn helper.
//
// We allow ungrouped choice items and represent by check boxes, so that can construct 
// questions with multiple correct answers. !!! Not fully handled, since currently unused
//////////////////////////////////////////////////////////////////

// Test for CGroup objects containing any choice items.
// We treat these as mutually exclusive sets of choices.
// Currently doesn't recurse into subgroups, so choices must be at top-level in group
BOOL CFBDView::IsChoiceGroup(CDrawObj* pObj)
{
	if (pObj->IsKindOf(RUNTIME_CLASS(CGroup)))
	{
		CGroup* pGroup = (CGroup*) pObj;
		POSITION pos = pGroup->m_objects.GetHeadPosition();
		while (pos != NULL)
		{
			CDrawObj* pElem = pGroup->m_objects.GetNext(pos);
			if (pElem->IsKindOf(RUNTIME_CLASS(CChoice)))
				return TRUE;
		}
	}
	return FALSE;
}

// Create all the child windows for multiple choice items
void CFBDView::CreateChoiceBtns()
{
	// loop through objects to find choice items and choice groups
	POSITION pos = GetDocument()->GetObjects()->GetHeadPosition();
	while (pos != NULL) 
	{
		CDrawObj* pObj = GetDocument()->GetObjects()->GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CChoice))) // found single choice
		{	
			CreateChoiceBtn((CChoice*) pObj);
		} 
		else if (IsChoiceGroup(pObj))		// found group of choices:
		{
			CGroup* pGroup = (CGroup*) pObj;	// loop to create each choice btn
			int nChoice = 0;					// counter to mark first element
			POSITION grpPos = pGroup->m_objects.GetHeadPosition();
			while (grpPos != NULL)
			{
				CDrawObj* pElem = pGroup->m_objects.GetNext(grpPos);
				if (pElem->IsKindOf(RUNTIME_CLASS(CChoice)))
					CreateChoiceBtn((CChoice*)pElem, pGroup, ++nChoice == 1);
			}
		}
	}
}

// Create a single button control for a multiple choice item.
// Optional param specifies if this is member of group. For convenience, we also
// require caller to tell is when creating first button in a group.
void CFBDView::CreateChoiceBtn(CChoice* pChoice, CGroup* pGroup, BOOL bBeginGroup)
{
	// allocate & construct a C++ CChoiceBtn object to wrap the control
	CChoiceBtn* pBtn = new CChoiceBtn; 
	pBtn->pChoice = pChoice;
	pBtn->pGroup = pGroup;
	
	// Create the Windows button control window
	CRect posBtn = pChoice->m_position;
	DocToClient(posBtn);
	DWORD dwStyle =   WS_CHILD | WS_VISIBLE;
	
	if (pGroup != NULL)// Button is part of a group... 
	{
		dwStyle |= BS_AUTORADIOBUTTON; // so gets the radio button style,
		if (bBeginGroup)				
			dwStyle |= WS_GROUP | WS_TABSTOP; // for first in group only
	}
	else	// not part of a group: // make it check box
		dwStyle |= BS_AUTOCHECKBOX | WS_TABSTOP | WS_GROUP; // end any prev group

	// Child window Id is offset from base by  number created so far
	UINT nId = ID_CHOICE_BASE + m_ChoiceBtns.GetCount();
	pBtn->Create(pChoice->m_strName, dwStyle, posBtn, this, nId);
	pBtn->SetFont(&m_fontText);
	TRACE("Created ChoiceBtn %d: %s\n", nId, pChoice->m_strName);

	// set initial check state from document
	if (pChoice->m_bChosen) {
		pBtn->SetCheck(1);
	}

	// Save ptr on view's list of choice buttons
	m_ChoiceBtns.AddTail(pBtn);	
}

// Map doc's choice object to the CChoiceBtn object for it in this view
CChoiceBtn* CFBDView::FindBtn(CChoice* pChoice)
{
	POSITION pos = m_ChoiceBtns.GetHeadPosition();
	while (pos != NULL)
	{
		CChoiceBtn* pBtn = m_ChoiceBtns.GetNext(pos);
		if (pBtn->pChoice == pChoice)
			return pBtn;
	}
	return NULL;
}
	
BOOL CFBDView::PreCreateWindow(CREATESTRUCT& cs) 
{
	// We set CLIPCHILDREN to prevent overwriting child buttons when drawing other
	// graphics in the view, e.g. the background grid. But it makes us unable to use
	// transparent buttons to get background showing through on blank areas in button
	// rectangle -- we can't overwrite the dirty region of the button rect to repaint
	// the background there, but the buttons draw themselves transparently, so any 
	// cruft drawn in that area (from a window previously on top, say) winds up left 
	// in the button's background! For now, changed buttons to have a white background
	// which obscures any view graphics behind it. (Might also try to use a patterned brush with our 
	// grid pattern, not sure if that can work, though).
	cs.style |= WS_CLIPCHILDREN | WS_EX_CONTROLPARENT; 
	
	return CBaseView::PreCreateWindow(cs);
}

// Handle message from child control to set its drawing attributes.
// Currently only used for choice buttons, edits handle their own.
// !!! should be moved into reflected message handler in our btn class
HBRUSH CFBDView::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	// Get the default brush from base class
	HBRUSH hbr = CScrollView::OnCtlColor(pDC, pWnd, nCtlColor);

	// make sure message is from one of our choice button controls
	// NB: button faces send CTLCOLOR_STATIC
	int id = pWnd->GetDlgCtrlID();
	if (id < ID_CHOICE_BASE || id > ID_CHOICE_BASE + m_ChoiceBtns.GetCount())			
		return hbr;

	// Windows control was created from our own CChoiceBtn wrapper, so MFC should have 
	// mapped it to that object, as opposed to generating a temporary generic Cwnd.
	ASSERT(pWnd->IsKindOf(RUNTIME_CLASS(CChoiceBtn)));
	CChoiceBtn* pChoiceBtn = (CChoiceBtn*) pWnd;

	// set to paint text transparently on background
	pDC->SetBkMode(TRANSPARENT);

	// color based on object status
	if (pChoiceBtn->pChoice->m_status == statusCorrect)
		pDC->SetTextColor(RGB(0, 128, 0));	// green
	else if (pChoiceBtn->pChoice->m_status == statusError)
		pDC->SetTextColor(RGB(255, 0, 0));  // red
	else
		pDC->SetTextColor(RGB(0, 0, 0));	// black (could use SYSCOLOR for 3D face

	// Routine returns handle to background brush to use when painting control
	return ::GetSysColorBrush(m_bEnabled ? COLOR_WINDOW : COLOR_3DFACE);
 /* CBrush brNull;
	brNull.CreateStockObject(NULL_BRUSH);
	return (HBRUSH) brNull; // MFC GDI classes provide conversions to Windows handles */
}

// Handle click of choice button
// This also might be moved into reflected message handler in our button class
void CFBDView::OnChoiceClicked(UINT idBtn)
{
	TRACE("Clicked choice %d\n", idBtn);
	LogEventf(EV_CHOICE_CLICK, "%d", idBtn);     // low level, just gives ctrl id
	
	// map ctrl id to ChoiceBtn object
	if (idBtn < ID_CHOICE_BASE 
		  || idBtn >= (UINT) ID_CHOICE_BASE + m_ChoiceBtns.GetCount())
		  return;
	CWnd* pCtl = GetDlgItem(idBtn);
	// Windows control was created from our own CChoiceBtn wrapper, so MFC should have 
	// mapped it to that object (as opposed to generating a temporary generic Cwnd).
	ASSERT(pCtl->IsKindOf(RUNTIME_CLASS(CChoiceBtn)));
	CChoiceBtn* pClickedBtn = (CChoiceBtn*) pCtl;
	CGroup* pGroup = pClickedBtn->pGroup;	// the choice group object

	// Transfer choice values into document (like DDX, only not a dialog):
	// if this button part of a group, must update values for all items in group
	if (pGroup != NULL) // choice is part of a mutually exclusive group
	{
		// loop for each CChoice object in the Group:
		// while looping, figure out ordinal number of chosen button in group
		int nChosen = -1;		
		int nChoice = 0;
		POSITION pos = pGroup->m_objects.GetHeadPosition();
		while (pos != NULL)
		{
			CDrawObj* pElem = pGroup->m_objects.GetNext(pos);
			if ( pElem->IsKindOf(RUNTIME_CLASS(CChoice)) )
			{
				CChoice* pChoice = (CChoice*) pElem;
				nChoice += 1;

				// Find the button control for the CChoice data object
				CChoiceBtn* pBtn = FindBtn(pChoice);
				if (! pBtn) continue;

				// Transfer this item's current check state into document object. 
				pChoice->m_bChosen = pBtn->GetCheck();

				// Status is unknown until help system call
				pChoice->m_status = statusUnknown;
				
				if (pBtn == pClickedBtn)
					nChosen = nChoice;			// remember number of selected choice
				
				// force redraw of button w/new status
				pBtn->Invalidate(); 
			}
		}

		// mark doc as modified
		GetDocument()->SetModifiedFlag();
		// Should notify other views, but not needed yet).

		// notify help system of choice selection event
		LogEventf(EV_CHOICE_SELECT, "%s %d", pGroup->m_strId, nChosen);
		LPCTSTR pszResult = 
			HelpSystemExecf("(lookup-mc-answer %s %d)", pGroup->m_strId, nChosen);

		// Apply status result to object.
		// Problem?: Apply Status can exec mini-lesson command to show dialog.
		// Invalidating here might lead to repaint before status updated. 
		// May need to split the two operations.
		pClickedBtn->Invalidate();
		CStringList strErrs;
		CCheckedObj::ApplyStatus(pszResult, pClickedBtn->pChoice->m_status, strErrs);
	}
	else // single choice, not part of group: 
	{
		//  Transfer check state into document object
		CChoice* pChoice = pClickedBtn->pChoice;
		pChoice->m_bChosen = pClickedBtn->GetCheck();

		// Doc is now dirty:
		GetDocument()->SetModifiedFlag();
	
		// Status is unknown until help system call
		pChoice->m_status = statusUnknown;
		pChoice->Invalidate(); 
		
		// how to notify help system on this? treat via Id.
		LogEventf(EV_CHOICE_CHECK, "%s %d", pChoice->m_strId, pChoice->m_bChosen);
		LPCTSTR pszResult = 
			HelpSystemExecf("(lookup-mc-answer %s %d)", pChoice->m_strId, pChoice->m_bChosen);
			
	/* changed to actually query the help system for check status
		//  Just set status field in object ourselves
		pChoice->m_status = (pChoice->m_bCorrect == pChoice->m_bChosen) ? 
			statusCorrect : statusError; */

		// Apply status result to object.
		CStringList strErrs;
		CCheckedObj::ApplyStatus(pszResult, pChoice->m_status, strErrs);

		// Should notify other views, but not needed yet.
		pClickedBtn->Invalidate();
	}
}

//
// AnswerBoxes: Runtime equation controls
// 
// For now, an answer box region can be defined by any item whose ID begins w/the
// magic prefix "Answer". We use obj position to create control and m_strName field 
// to store result text.
//
// !!! Will be problems if obj uses m_strName, e.g. label obj. Need something for
// printing too. Label object would do, but our labels are sized to content, so author
// couldn't draw empty box. Really need a special answer box object of some sort.
static char szPrefix[] = "Answer";	
#define PREFIX_LEN (sizeof(szPrefix) - 1)

void CFBDView::CreateAnswerBoxes()
{
	POSITION pos = GetDocument()->GetObjects()->GetHeadPosition();
	while (pos != NULL) 
	{
		CDrawObj* pObj = GetDocument()->GetObjects()->GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) &&
			((CDrawRect*)pObj)->IsAnswerBox())
		{
			// Allocate a C++ CWnd-derived object for the control
			CEQEditType* pEdit = new CEQEditType;
			// Set control's id to bind it to document data object
			// Can then map control obj to doc's object via CFBDoc::Lookup(m_strId)
			pEdit->m_strId = pObj->m_strId;
			// Set it to show Equation context menu on WM_CONTEXTMENU messages
			pEdit->m_nMenuId = IDR_POPUP_EQVIEW;

			// Create the Windows control window
			CRect rcEdit = pObj->m_position;// rect comes from object position
			DocToClient(rcEdit);
			// We used to set WS_VISIBLE, but on Windows XP it didn't paint the 3D
			// client-edge effect on first showing after switch out of author mode, when
			// container window already shown. Creating hidden then showing (below) fixes this.
			DWORD dwStyle = WS_CHILD | WS_TABSTOP | WS_BORDER | ES_AUTOHSCROLL;
			// Ctl's PreCreateWindow adds WS_EX_CLIENT_EDGE for sunken edge outside dlg
			// ID is offset from range base by next index in our list =current count
			UINT nID = ID_ANSWER_BASE + m_Answers.GetCount();
			pEdit->Create(dwStyle, rcEdit, this, nID);

			// Save control obj on our list of Answer controls
			m_Answers.AddTail(pEdit);

			// Set font to default text font we created
			pEdit->SetFont(&m_fontText);
#ifdef EQ_RICHEDIT 
			// Ctl requires text be protected to block EN_CHANGE on format changes
			CHARFORMAT cfDefault;
			cfDefault.cbSize = sizeof(cfDefault);
			cfDefault.dwEffects = CFE_PROTECTED; 
			cfDefault.dwMask = CFM_PROTECTED;
			pEdit->SetDefaultCharFormat(cfDefault);

			// Set up to get change notifications, adding ENM_PROTECTED since 
			// control requires it. 
			pEdit->SetEventMask(ENM_CHANGE | ENM_PROTECTED);
#endif 	
			// Transfer initial state props from document into control:
			// Need to set flag to suppress processing of EN_CHANGE notifications 
			// sent from the control on text (and possibly format) changes.
			m_bIgnoreChange = TRUE;

			// Initialize its text from document
			pEdit->SetRichEditText(pObj->m_strName);

			// Initialize its status color from document. Can't use SetAnswerStatus
			// since that is "optimized" to do nothing if status unchanged from doc,
			// and we need to force color change for red or green status.
			TRACE("Initting status color of %s to %d\n", pEdit->m_strId, (int) pObj->m_status);
			if (pObj->m_status == statusCorrect)
				pEdit->SetTextColor(RGB(0, 128, 0));	// Dark green
			else if (pObj->m_status == statusError)
				pEdit->SetTextColor(RGB(255, 0, 0));	// Red
			else
				pEdit->SetTextColor(RGB(0, 0, 0));		// Black

			m_bIgnoreChange = FALSE;
			
			// show control window, to get entire control including 3d edge painted in all cases. 
			pEdit->ShowWindow(SW_SHOWNORMAL);
		}
	}
}

// GetAnswerEdit -- get answer box edit control by object id, NULL if not found
// (Used to map id arg to control on log playback)
CEQEditType* CFBDView::FindAnswerEdit(CString strId)
{
	// search list of answer boxes for match
	POSITION pos = m_Answers.GetHeadPosition();
	while (pos != NULL){
		CEQEditType* pEdit = m_Answers.GetNext(pos);
		if (pEdit->m_strId == strId)
			return pEdit;
	}
	return NULL;	// not found
}

// save whether answer box has keyboard focus
void CFBDView::OnAnswerFocus(UINT idCtrl)
{
	m_idFocusEdit = idCtrl;
	m_nLastActive = idCtrl;
	// test id against range to make sure its one of our equation edits
	if (idCtrl < ID_ANSWER_BASE || (int)idCtrl >= ID_ANSWER_BASE + m_Answers.GetCount())
		  return;
	
	// An answer control taking focus is a little like a selection. But we don't really
	// want to select the AnswerBox object in the document, since that leaves it liable to 
	// deletion and shows the resize handles "behind" the edit control. 
	// So we just clear selection and check for active edit controls at the right time.
	Select(NULL);
	
	// Log
	CEQEditType* pEdit = (CEQEditType*) GetDlgItem(idCtrl);
	LogEventf(EV_ANSWER_FOCUS, "%s", pEdit->m_strId);
}

void CFBDView::OnAnswerKillFocus(UINT idCtrl)
{
	m_idFocusEdit = -1;
	
	// Log
	CEQEditType* pEdit = (CEQEditType*) GetDlgItem(idCtrl);
	LogEventf(EV_ANSWER_KILLFOCUS, "%s", pEdit->m_strId);
}

// helper sets the status value of an answer box control (and associated doc item)
void CFBDView::SetAnswerStatus(CEQEditType* pEdit, Status newStatus)
{
	// update underlying object in document
	CDrawObj* pObj = GetDocument()->Lookup(pEdit->m_strId);
	ASSERT(pObj);

	// Don't set modified if status unchanged. Omit log/trace output as well.
	if (pObj->m_status != newStatus) 
	{
		TRACE("Changing status/color of %s to %d\n", pEdit->m_strId, (int) newStatus);
		pObj->m_status = newStatus;
		// Mark doc dirty on status change
		GetDocument()->SetModifiedFlag();

		// Trace status changes in log file
		LogEventf(EV_ANSWER_STATUS, "%s %d", pEdit->m_strId, (int) pObj->m_status);
	}
	
	// Always update control's text color to ensure correct. Possible text change was
	// paste of colored text (copied from equation box, e.g.) into answer box with status 
	// recorded as unknown. Still must ensure text recolored though status unchanged.

#ifdef EQ_RICHEDIT	// !!! In richedits, color change causes EN_CHANGE to be sent
	BOOL bPrevFlag = m_bIgnoreChange;
	m_bIgnoreChange = TRUE;
#endif 
	if (newStatus == statusCorrect)
		pEdit->SetTextColor(RGB(0, 128, 0));	// Dark green
	else if (newStatus == statusError)
		pEdit->SetTextColor(RGB(255, 0, 0));	// Red
	else
		pEdit->SetTextColor(RGB(0, 0, 0));		// Black

#ifdef EQ_RICHEDIT	
	m_bIgnoreChange = bPrevFlag;
#endif 
}

// Handle change in Answer box text
void CFBDView::OnAnswerChange(UINT idCtrl)
{
	if (m_bIgnoreChange)		// do nothing for changes we have caused.
		return;

	CEQEditType* pEdit = (CEQEditType*) GetDlgItem(idCtrl);
	CString strAnswer;
	pEdit->GetRichEditText(strAnswer);

	// With richedits, we get these even on formatting changes. To ignore them,
	// we update the document contents with the control's on every text change.
	// We do nothing if text has not changed from last stored in document.
	CDrawObj* pObj = GetDocument()->Lookup(pEdit->m_strId);
	ASSERT(pObj != NULL);
	if (strAnswer != pObj->m_strName)	// text has really changed
	{
		// The document data should reflect state of controls even if text is not 
		// yet submitted for checking, so update and mark it dirty now
		pObj->m_strName = strAnswer;
		GetDocument()->SetModifiedFlag();
		// Could broadcast update on change in answer. (Might do again on status change)
		// GetDocument()->UpdateAllViews(this, HINT_UPDATE_ANSWER, pEdit);

		// Log the change. 
		LogEventf(EV_ANSWER_CHANGE, "%s %s", pEdit->m_strId,  strAnswer);
	}

	// Change status to unknown until submitted again
	SetAnswerStatus(pEdit, statusUnknown);
}

// Apply status result from help system, updating status and executing any
// attached command.
void CFBDView::ApplyAnswerStatus(LPCTSTR pszResult, CEQEditType* pEdit)
{
	CString strReturn, strCommand;
	CStringList strErrs;
	CCheckedObj::SplitResult(pszResult, strReturn, strCommand);
   
   	// set status from return value
   	if (strReturn.IsEmpty())
		SetAnswerStatus(pEdit, statusUnknown);
	else if (strReturn == "T") {
		SetAnswerStatus(pEdit, statusCorrect);
		theApp.MakeSound(sndCorrectEntry);
	}
	else {
		SetAnswerStatus(pEdit, statusError);
		theApp.MakeSound(sndErrorEntry);
	}
   
	// if got a command, pass it to application's command interpreter
   	if (!strCommand.IsEmpty()) 
   		ExecuteCmd(strCommand);
}

// Handle Submit of changed text in an Answer box.
//
// Inside dialog boxes, the standard keyboard interface translates Enter key to a default 
// pushputton press, usually OK button, or to IDOK command if none. This is not
// a dialog box, So that response will be nearly the same here and in EQView, we set
// our subclassed EQEdit controls to send a BN_CLICK notification for 
// IDOK whenever the user hits the Enter key. That event is mapped to this handler.
void CFBDView::OnAnswerEnter()
{
	// Use currently active edit. Don't get from Windows focus so this works in
	// log playback mode, where focus edit is cached in member var, but not actually set.
	CEQEditType* pEdit;
	if (IsActiveEdit())
		pEdit = GetActiveEdit();
	else // old code as backup, just in case lost track of focus control somehow
	{
		// Get focus control. Should be same as in focus edit id.
		CWnd* pWndCtrl = GetFocus();
		if (pWndCtrl == NULL) 
			return;
		// ensure focus control is one of our Answer edits
		int idCtrl = pWndCtrl->GetDlgCtrlID();
		if (idCtrl < ID_ANSWER_BASE || idCtrl >= ID_ANSWER_BASE + m_Answers.GetCount())
			return;
		TRACE("FBDView::OnAnswerEnter: idFocusEdit out of synch with focus!\n");
		pEdit = (CEQEditType*) pWndCtrl;
	}

	// Fetch the new text
	CString strEq;
	pEdit->GetRichEditText(strEq);

	// Log the submit event
	TRACE("Submitted %s: %s\n", pEdit->m_strId, strEq);
	LogEventf(EV_ANSWER_SUBMIT,"%s %s", pEdit->m_strId, strEq);	// was Answer-enter 	

	// Now do this anyway to fix up text for bug getting Greek char when synching on change.

#if 1 // shouldn't be necessary anymore since we synch on changes
	// Find associated answer box item in doc and update data
	CDrawObj* pObj = GetDocument()->Lookup(pEdit->m_strId);
	ASSERT(pObj != NULL);
	pObj->m_strName = strEq;
	// Note this is point at which we consider document to be changed
	GetDocument()->SetModifiedFlag();	

	// Could Notify other views of change in answer. (Might do again on status change)
	// GetDocument()->UpdateAllViews(this, HINT_UPDATE_ANSWER, pEdit);
#endif 
	ASSERT(GetDocument()->Lookup(pEdit->m_strId)->m_strName == strEq);

	// check entry with the help system. 
	LPCTSTR pszResult = 
			HelpSystemExecf("(check-answer \"%s\" %s)", LISPSTR(strEq), pEdit->m_strId);
	
	// set status and color
	ApplyAnswerStatus(pszResult, pEdit);
	
	// if no error, advance the focus to next in tab order
	if (pszResult && strcmp(pszResult, "T") != 0) {
		CWnd* pWndCtrlNext = GetNextDlgTabItem(pEdit);
		if (pWndCtrlNext != NULL)
			pWndCtrlNext->SetFocus();
	}
}

//
// DestroyControls -- destroy all dynamically created control windows 
//                    (buttons & answers) and delete memory for C++ CWnds.
// Safe to use after Windows windows destroyed.
void CFBDView::DestroyControls()
{
	while (!m_ChoiceBtns.IsEmpty()) {
		CChoiceBtn* pBtn = m_ChoiceBtns.RemoveHead();
		if (pBtn->GetSafeHwnd())	// Windows control window exists
			pBtn->DestroyWindow();
		delete pBtn;
	}
	while (!m_Answers.IsEmpty()) {
		CEQEditType* pEdit = m_Answers.RemoveHead();
		if (pEdit->GetSafeHwnd())	// Windows control window exists
			pEdit->DestroyWindow();
		delete pEdit;
	}
}

//////////////////////////////////////////////////////////////////////////////////
//
// CFBDView Drawing and screen updating
//
//////////////////////////////////////////////////////////////////////////////////

//
// InvalObjInView -- helper invalidates region of a single object in this view only
//
// This gets called directly when in process of tracking mouse while moving or resizing 
// objects in this view, in which case other views don't get notified until move is 
// completed. Other calls come via the general OnUpdate mechanism (see below).
//
// We attempt to implement smart (i.e. minimal) invalidation by marking only the 
// bounding box of the given object as needing repainting. However, currently not 
// all our objects define their bounding boxes correctly. !!! Need to fix.
//
void CFBDView::InvalObjInView(CDrawObj* pObj) 
{
	// Some objects don't currently define good bounding boxes because of
	// thickness and labels, so just invalidate the whole view for them.
	// !!! This should be gotten by methods in object, e.g. HaveBoundingBox.
	if ( pObj->IsKindOf(RUNTIME_CLASS(CVector)) ||
		 pObj->IsKindOf(RUNTIME_CLASS(CAngle)) ||
		 pObj->IsKindOf(RUNTIME_CLASS(CAxes)) ||
		 pObj->IsKindOf(RUNTIME_CLASS(CSystem)) || 
		 pObj->IsKindOf(RUNTIME_CLASS(CRadius)) ||
		 pObj->IsKindOf(RUNTIME_CLASS(CMotionBody)) ||
		 pObj->IsKindOf(RUNTIME_CLASS(CMotionDiagram))  )
	{	
		Invalidate(FALSE);
	} 
	else // Invalidate only bounds
	{
		// following enlargement code is from DRAWCLI: 
		// if obj selected in active view, widens dirty region to include
		// resize handles and possible OLE activation border.
		// !!! depends on handle sizes in device units.
		CRect dirty = pObj->GetBoundingBox();
		DocToClient(dirty);
		// dirty.NormalizeRect(); /* handled in DocToClient
		if (m_bActive &&  IsSelected(pObj) ) {
			dirty.left -= 4;
			dirty.top -= 5;
			dirty.right += 5;
			dirty.bottom += 4;
		}
		dirty.InflateRect(1, 1); // handles COleDrawObj objects 
		InvalidateRect(dirty, FALSE);
	}

	// Diagram extent may have changed, e.g may be move/resize drag
	UpdateScrollSizes();
}

//
// OnUpdate -- process an update hint sent by a view after some change
//
void CFBDView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	// ignore updates that do not affect diagram objects
	if (! (IS_DIAGRAM_UPDATE(lHint) || 
		   lHint == HINT_AUTHOR_MODE || lHint == HINT_TUTOR_MODE) )
		return;

	switch (lHint)
	{
	default:
	case HINT_UPDATE_WINDOW:	// redraw entire diagram window
		Invalidate(FALSE);
		break;

	case HINT_UPDATE_DRAWOBJ:	// a single diagram object has changed
		InvalObjInView((CDrawObj*) pHint);
		break;

	case HINT_UPDATE_TEMPOBJ:
	{
		CChkObjList* pList = (CChkObjList*)pHint;
		if (pList->IsEmpty())
			return;
		CCheckedObj* pRealObj = pList->GetHead();
		CCheckedObj* pTempObj = pList->GetTail();
		pRealObj->m_status = pTempObj->m_status;
		if (pTempObj->IsKindOf(RUNTIME_CLASS(CVector))) {
			CVector* pRealVec = (CVector*) pRealObj;
			CVector* pTempVec = (CVector*) pTempObj;
			pRealVec->SetZDir( pTempVec->m_nZDir );
			int nDeg;
			if (!pTempVec->IsZAxisVector() &&
				sscanf( pTempVec->m_strOrientation,"%d", &nDeg) == 1)
				pRealVec->SetDirection(nDeg );
		} else if (pTempObj->IsKindOf(RUNTIME_CLASS(CAxes)))
			pRealObj->SetDirection(((CAxes*)pTempObj)->m_nDirection);

		InvalObjInView(pRealObj);
	}
	case HINT_UPDATE_SELECTION:	// an entire selection list has changed
		{
			// NULL means our own selection list.
			CDrawObjList* pList = pHint != NULL ? (CDrawObjList*)pHint : &m_Selection;
			POSITION pos = pList->GetHeadPosition();
			while (pos != NULL)
				InvalObjInView(pList->GetNext(pos));
		}
		break;

	case HINT_DELETE_SELECTION: // an entire selection is about to be deleted
		
		if (pHint != &m_Selection) // Only process if sent by *other* view than this	
		{
			CDrawObjList* pList = (CDrawObjList*) pHint;
			POSITION pos = pList->GetHeadPosition();
			while (pos != NULL)
			{
				CDrawObj* pObj = pList->GetNext(pos);

				// Mark object's region dirty
				InvalObjInView(pObj);	

				// and remove it from this view's selection list
				RemoveFromSel(pObj); 
			}
		}
		break;
	
	case HINT_UPDATE_OLE_ITEMS:	// updates all OLE items in document
		{
			CFBDDoc* pDoc = GetDocument();
			POSITION pos = pDoc->m_objects.GetHeadPosition();
			while (pos != NULL)
			{
				CDrawObj* pObj = pDoc->m_objects.GetNext(pos);
				if (pObj->IsKindOf(RUNTIME_CLASS(COleDrawObj)))
					InvalObjInView(pObj);
			}
		}
		break;
	
	case HINT_AUTHOR_MODE:		// change into/out of author mode
		if (theApp.m_bAuthorMode)	// entered author mode: destroy runtime controls
			DestroyControls();		//       so can edit author's representation
		else // changed back to student mode: 
		{
			CreateChoiceBtns();		// create runtime controls again
			CreateAnswerBoxes();
		}
		Invalidate(FALSE);			// redraw all in either case
		break;

	case HINT_TUTOR_MODE:
		// enable/disable and redraw
		EnablePane(! (DWORD) pHint);
		break;

	} // end big switch

	// Diagram extent may have changed:
	UpdateScrollSizes();
}

//
// EnablePane -- Enable or Disable the whole pane and update appearance
//
void CFBDView::EnablePane(BOOL bEnable)
{
	// remember enabled state for quick access in cmd updating; button repainting
	m_bEnabled = bEnable;
	// Disable this window and repaint
	EnableWindow(bEnable);
	Invalidate(FALSE);

	// Update all child windows
	// enable child equation controls and adjust appearance 
	COLORREF crBackground = ::GetSysColor(bEnable ? COLOR_WINDOW : COLOR_3DFACE); 
	// answer boxes 
	POSITION pos = m_Answers.GetHeadPosition();
	while (pos != NULL){
		CEQEditType* pEdit = m_Answers.GetNext(pos);
		pEdit->EnableWindow(bEnable);
		pEdit->SetBkColor(crBackground);
	}
	// repaint multiple choice buttons (they're colored by OnCtlColor);
	pos = m_ChoiceBtns.GetHeadPosition();
	while (pos != NULL) {
		CChoiceBtn* pBtn = m_ChoiceBtns.GetNext(pos);
		pBtn->Invalidate(FALSE);
	}
}

//
// OnPaint -- repaint the currently invalid region
//
// Sets up offscreen bitmap for repainting dirty region, uses OnDraw to paint it,
// then blits it onto the screen. Calls to CWnd:Invalidate should specify FALSE
// to prevent Windows from erasing the background, which is what causes flashing.
// This routine afairly generic, hence re-usable. (needs only DocToClient helper).
//
void CFBDView::OnPaint() 
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
	/* CBrush brush;
	if (!brush.CreateSolidBrush(::GetSysColor(COLOR_WINDOW)))
				return;
	brush.UnrealizeObject();
	pDrawDC->FillRect(docDirty, &brush); */
	COLORREF crBackground = ::GetSysColor(m_bEnabled ? COLOR_WINDOW : COLOR_3DFACE);
	pDrawDC->FillSolidRect(docDirty, crBackground); 

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

BOOL CFBDView::OnEraseBkgnd(CDC* pDC) 
{
	return TRUE;	// eat this to prevent Windows from erasing background ever
}

//
// OnDraw -- render image of view into a DC. Used for paint, print, print preview
//
void CFBDView::OnDraw(CDC* pDC)
{
	CFBDDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	// Draw the grid background if user option set, on display but not printer. 
	if (m_bShowGrid && !pDC->IsPrinting()) {
		DrawGrid(pDC);
	}

	// Now paint all the drawing objects from back to front
	pDoc->Draw(pDC, this);
} 

void CFBDView::DrawGrid(CDC* pDC) 
{
	/* CFBDDoc* pDoc = GetDocument(); */
	/* COLORREF oldBkColor = pDC->SetBkColor(pDoc->GetPaperColor()); */
	// !!! Get size from document
	CRect rect(0, 0, PAGEWIDTH, PAGEHEIGHT);
	if (UseLandscapeLayout())
		rect = CRect(0, 0, PAGEHEIGHT, PAGEWIDTH);
	
	// Pens we use 
	CPen penSolid;
	penSolid.CreatePen(PS_SOLID, 1, m_colorGrid);
	CPen penDot;
	penDot.CreatePen(PS_DOT, 1, m_colorGrid);
	CPen* pOldPen = pDC->SelectObject(&penDot);

	// Draw unit lines. For normal fine-grained grid, we draw line at every 
	// other grid interval so it's not too busy. For coarse-grained vector grid 
	// we draw at every division so it's easy to draw while counting boxes.
	int nFactor = (GetDocument()->m_wConcept & ID_PROB_VECTOR) ? 1 : 2;
	for (int x = rect.left ; x < rect.right; x += nFactor*m_cxGrid)
	{
		if (x != 0)
		{
			// inch lines are solid, minor lines dashed
			if (x % nLUsPerInch == 0) 
				pDC->SelectObject(&penSolid);
			else
				pDC->SelectObject(&penDot);

			pDC->MoveTo(x, rect.top);
			pDC->LineTo(x, rect.bottom);
		}
	}

	for (int y = rect.top ; y < rect.bottom; y += nFactor*m_cyGrid)
	{
		if (y != 0)
		{
			// inch lines are solid, minor lines dashed
			if (y % nLUsPerInch == 0) 
				pDC->SelectObject(&penSolid);
			else
				pDC->SelectObject(&penDot);
			pDC->MoveTo(rect.left, y);
			pDC->LineTo(rect.right, y);
		}
	}

	// And the heavy page border
	DrawBorder(pDC);

	pDC->SelectObject(pOldPen);
	/* pDC->SetBkColor(oldBkColor); */
}

// Following code attempted to draw a focus border around the client area
// in inverse mode to show active status of view, allowing repeated draw to 
// toggle the highlighting with changes in active status. But doesn't work. 
// Now just draws a page border.
void CFBDView::DrawBorder(CDC* pDC)
{
	/* CRect rcBorder;
	GetClientRect(rcBorder); */
	CRect rcBorder(0, 0, PAGEWIDTH, PAGEHEIGHT);
	if (UseLandscapeLayout())
		rcBorder = CRect(0, 0, PAGEHEIGHT, PAGEWIDTH);
	
	CPen penWideSolid;
	penWideSolid.CreatePen(PS_SOLID, 2, RGB(0,0,0));
	CPen* pOldPen = pDC->SelectObject(&penWideSolid);
	CBrush* pOldBrush = (CBrush*) pDC->SelectStockObject(NULL_BRUSH);
	/* Not working to track activity, for now just draw border */
	/* int oldROP = pDC->SetROP2(R2_NOT); */

	pDC->Rectangle(rcBorder);

	/* pDC->SetROP2(oldROP); */
	pDC->SelectObject(pOldPen);
	pDC->SelectObject(pOldBrush);
}

/////////////////////////////////////////////////////////////////////////////
// CFBDView printing
// 
// MFC provides implementation of standard printing commands in the View base class.
// These are mapped in the view as follows:
//    ON_COMMAND(ID_FILE_PRINT, CView::OnFilePrint)
//    ON_COMMAND(ID_FILE_PRINT_DIRECT, CView::OnFilePrint) // cmd line printto option? 
//    ON_COMMAND(ID_FILE_PRINT_PREVIEW, CView::OnFilePrintPreview) 
// MFC implementation is a "driver" which calls the following virtual functions
// in the view to do the rendering during the print process.
//
// In our multi-view app, the FBDView handles the printing for our documents. 
// It fetches other components of the document (equations, variables) and lays them
// all out on a single page.


// SetDeviceOrientation: set portrait or landscape mode on named device (printer)
// Returns: pointer to revised DEVMODE structure (malloc'ed, caller should free)
// This is generic Win32 code copied from "ChangeDevMode" routine in MS KB articles.
//
// Note: CWinApp maintains a cached DEVMODE for the current printer, updating it after any 
// uses of the print/page setup dialog, or on a WM_DEVCHANGE event.
// Changing printer mode through this routine will *not* update CWinApp's cached DEVMODE.
// Since subsequent uses of a print dlg are initialized from the cached DEVMODE [?], the dialog 
// display may not reflect this changed mode on opening. (Once OK'd the printer settings will
// be set to match the dialog and the cache updated as well.) That should be OK. We are using 
// this just before printing certain problems to force use of landscape mode, regardless of 
// user's chosen printer settings. The effect is that user's last explicitly selected defaults 
// remain defaults for next job.
LPDEVMODE SetDeviceOrientation(HWND hWnd, char *pDevice, short dmOrientation)
{
   HANDLE      hPrinter;
   LPDEVMODE   pDevMode;
   DWORD       dwNeeded, dwRet;

   /* Start by opening the printer */
   if (!::OpenPrinter(pDevice, &hPrinter, NULL))
       return NULL;

   /*
    * Step 1:
    * Allocate a buffer of the correct size.
    */
   dwNeeded = DocumentProperties(hWnd,
       hPrinter,       /* handle to our printer */
       pDevice,        /* Name of the printer */
       NULL,           /* Asking for size so */
       NULL,           /* these are not used. */
       0);             /* Zero returns buffer size. */
   pDevMode = (LPDEVMODE)malloc(dwNeeded);

   /*
    * Step 2:
    * Get the default DevMode for the printer and
    * modify it for our needs.
    */
   dwRet = ::DocumentProperties(hWnd,
       hPrinter,
       pDevice,
       pDevMode,       /* The address of the buffer to fill. */
       NULL,           /* Not using the input buffer. */
       DM_OUT_BUFFER); /* Have the output buffer filled. */
   if (dwRet != IDOK)
   {
       /* if failure, cleanup and return failure */
       free(pDevMode);
       ::ClosePrinter(hPrinter);
       return NULL;
   }

   /*
    * Make changes to the DevMode which are supported.
    */
	if (pDevMode->dmFields & DM_ORIENTATION)       
	{
       /* if the printer supports paper orientation, set it*/
	   ASSERT(dmOrientation == DMORIENT_LANDSCAPE || dmOrientation == DMORIENT_PORTRAIT);   
       pDevMode->dmOrientation = dmOrientation;    
	}
   /*
    * Step 3:
    * Merge the new settings with the old.
    * This gives the driver a chance to update any private
    * portions of the DevMode structure.
    */
	dwRet = ::DocumentProperties(hWnd,
       hPrinter,
       pDevice,
       pDevMode,       /* Reuse our buffer for output. */
       pDevMode,       /* Pass the driver our changes. */
       DM_IN_BUFFER |  /* Commands to Merge our changes and */
       DM_OUT_BUFFER); /* write the result. */

   /* Done with the printer */
	::ClosePrinter(hPrinter);

   if (dwRet != IDOK)
   {
       /* if failure, cleanup and return failure */
       free(pDevMode);
       return NULL;
   }

   /* return the modified DevMode structure */
   return pDevMode;

}

// Modify printer DC for given pInfo print job to force printer
// to use the landscape orientation. This is to be used in OnPrepareDC
// hook to modify the print DC MFC constructs for us just before printing. 
// No effect if not a printer DC
void CFBDView::ForceLandscapePrinting(CDC* pDC, CPrintInfo* pInfo) 
{
	if(pDC->IsPrinting())   
	{
		// We should be able to get printer name from the PrintDialog member of
		// of the MFC PrintInfo struct which describes the print job. 
		CString strPrinterName = pInfo->m_pPD->GetDeviceName();		
        LPDEVMODE lpDevMode = SetDeviceOrientation(m_hWnd, strPrinterName.GetBuffer(64),
			                                       DMORIENT_LANDSCAPE);
		if (lpDevMode) {
			pDC->ResetDC(lpDevMode);
			free(lpDevMode); 
		}
	}
}

// OnPreparePrinting: called at beginning of printing or previewing, allowing us to wrap
// customizations around call to DoPreparePrinting, (which shows print dlg on printing.)
BOOL CFBDView::OnPreparePrinting(CPrintInfo* pInfo)
{
	// Fetch statistics from help system.
	LPCTSTR pszResult = HelpSystemExecf("(get-stats score)");
	if (pszResult) m_strStats = pszResult;

	// get number of pages
	int nPages;
	if (GetDocument()->m_bIncludePlan)
		nPages = 2;	// have a plan page
	else
		nPages = 1;	// just the FBD page/variable/equation page
/*
	// if we have statistics show on second page
	if (! m_strStats.IsEmpty())
		nPages = 2;
*/
	pInfo->SetMaxPage(nPages);

	BOOL bRet = DoPreparePrinting(pInfo);

	// Must adjust *after* DoPreparePrinting to override value read from .INI file:
	pInfo->m_nNumPreviewPages = nPages;
	return bRet;
}

// render str into rect rcLine using font appropriate for status, may wrap around.
// updates rcLine to contain next line. 
void CFBDView::WRITELINE(CDC* pDC, CRect &rcLine, CString str, Status status) 
{ 
	CRect rcText;		// rect actually occupied by printed text, may be >1 line
	if (status == statusError) { 
		CClientDC dcView(this);
		OnPrepareDC(&dcView);		// to set text font, if special
		CFont* pFontScreen = dcView.GetCurrentFont();
		LOGFONT logfont;
		pFontScreen->GetLogFont(&logfont);
		// create variant font for erroneous text (red on screen)
		CFont fontError;						
		logfont.lfItalic = TRUE;				// use italics to show
		fontError.CreateFontIndirect(&logfont);

		CFont* pFontPrev = (CFont*) pDC->SelectObject(&fontError); 
		COLORREF crPrev = pDC->SetTextColor(RGB(255, 0, 0)); 
		rcText = CGreekText::DrawText(pDC, rcLine, str);
		pDC->SetTextColor(crPrev);
		pDC->SelectObject(pFontPrev); 
	} else {
		rcText = CGreekText::DrawText(pDC, rcLine, str);
	}

	// Update top, bottom of printed line rect, leave left and right
	CSize sizeTextHeight(0, rcText.Height());
	rcLine += sizeTextHeight;
}

void CFBDView::OnBeginPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// Called for one-time initialization once have print device context, e.g. fonts
}

// convert score value string to float so we can multiply by weight to show subscore.
// returns: T for success, filling in fScore.
static BOOL ScoreToNum(CString strScore, double fWeight, double& fScore)
{
	float nNum, nDenom;
	if ((sscanf(strScore, "%f/%f", &nNum, &nDenom)) == 2) { // got a fraction
		if (nDenom == 0) {
		/* no longer true
			// n/0 => 1 for positively weighted scores, 0 for points off
			// not exactly right for positive rates, but we shouldn't be
			// getting this in the future anyway.
			fScore = fWeight > 0 ? 1.0 : 0.0; */
			fScore = 0.0; // should always be 0/0 => 0
		} else {
			fScore = nNum/nDenom;
		}
	} else {
		if (strScore.Find("/") != -1) // unparseable fraction???
			return FALSE;
		if (sscanf(strScore, "%lf", &fScore) != 1)
			return FALSE;
	}
	return TRUE; 
}

extern int split(const CString& str, const CString& Sep, CStringArray& result);

// Called to print a page
void CFBDView::OnPrint(CDC* pDC, CPrintInfo* pInfo) 
{
	// Try to select a font w/similar attributes to default screen font
	CClientDC dcView(this);
	OnPrepareDC(&dcView);		// to set text font, if special
	CFont* pFontScreen = dcView.GetCurrentFont();
	LOGFONT logfont;
	pFontScreen->GetLogFont(&logfont);
	CFont fontText;							// default font for text
	fontText.CreateFontIndirect(&logfont);  // should map to similar printer font
	// create variant font for erroneous text (red on screen)
	// now created in WRITELINE
//	CFont fontError;						
//	logfont.lfItalic = TRUE;				// use italics to show
//	fontError.CreateFontIndirect(&logfont);

	CFont* pOldFont = pDC->SelectObject(&fontText);

	// Header has document name plus creator name. Note creator
	// may be unset yet, but will be set if this is saved in student mode.
	CString strHeader = GetDocument()->GetTitle();
	CString strName= GetDocument()->m_strCreator;
	if (strName.IsEmpty())
		strName = theApp.m_strUserName;

	strHeader += " -- " + strName;
	// add time of printing to header info:
	strHeader += "    " + (CTime::GetCurrentTime()).Format("%c");

/* 	// We also "code" some unlabelled statistics, currently problem time.
	CString strStats;
	strStats.Format("[%d]", HistTime() - GetDocument()->m_nStartTime); */

	if (pInfo->m_nCurPage == 1) // Printing Diagram page
	{
		pInfo->m_rectDraw.top	= 0;
		pInfo->m_rectDraw.left	= 0;
		// Print the header. Updates m_rectDraw (height only) to reflect remaining space
		PrintPageHeader(pDC, pInfo, strHeader);

		// Offset logical page window origin to reflect modified pInfo->m_rectDraw. While
		// this is in effect, logical coordinate (0,0) renders at top left of page area 
		// remaining below the header, not the original top of the printing area.
		pDC->SetWindowOrg(pInfo->m_rectDraw.left, -pInfo->m_rectDraw.top);
		// remember bottom edge in new logical coords == Height of printable rectangle.
		int yBottom = pInfo->m_rectDraw.Height();

		// And render diagram "page" onto printer page. Doc objects underlying 
		// run-time controls should know how to render themselves for printer.
		OnDraw(pDC);
		
		//  Now add defs and equations in second column on right of page
		
		// We fetch some data from EQView. (!!! Would be better to get equations 
		// from document rather than relying on EQView.)
		CEQView* pEQView = theApp.GetEQView();
		// NB: Now pEQView may be NULL in case printing qual problem.
		// Should have no equations, so it'sOK. 
		//if (pEQView == NULL)
			// return;

		// set up to print series of lines in a column
		CString strLine;
		TEXTMETRIC tm;
		pDC->GetTextMetrics(&tm);
		int nHeight = tm.tmHeight + tm.tmExternalLeading;

		// fetch logical extent of diagram objects:
		CSize extDiagram = GetDocument()->GetSize();
/* 	// frame diagram on page for debugging
		pDC->FrameRect(CRect(CPoint(0,0), extDiagram), &CBrush(RGB(0,0,0)));
*/
		// Start column a quarter inch to the right of the diagram bounding box:
		int xRightCol = pInfo->m_rectDraw.left + extDiagram.cx + 24; // remember this
		// align with top of diagram (zero in logical, mapped to print below header).
		// rcLine is the box into which to render next line:
		CRect rcLine = CRect(xRightCol, 0, pInfo->m_rectDraw.right, nHeight );
		
#define SKIPLINE()	rcLine += CSize(0, nHeight);

		// Print principles and substeps
		if (! GetDocument()->m_principles.IsEmpty()) {
			WRITELINE(pDC, rcLine, "Principles", statusCorrect);
			POSITION pos = GetDocument()->m_principles.GetHeadPosition();
			while (pos != NULL) {
				CPrincObj* pPrinc = GetDocument()->m_principles.GetNext(pos);
				CString strPrinc = pPrinc->GetDisplayString();
				CString strCheck;
				if (pPrinc->m_status == statusError)
					strCheck = " X ";	// bad princ has no check box
				else if (pPrinc->m_bChecked) // ! might be read-only parent check
					strCheck = pPrinc->m_checkStatus == statusError ? "[x]" : "[+]";
				else // unchecked
					strCheck = "[  ]";

				strLine.Format("%s %s", strCheck, strPrinc);
				WRITELINE(pDC, rcLine, strLine, pPrinc->m_status);
			
				// print substeps and checks if any
				POSITION posStep = pPrinc->m_subItems.GetHeadPosition();
				while (posStep != NULL) {
					CPrincStep* pStep = (CPrincStep*) pPrinc->m_subItems.GetNext(posStep);
					CString strStep = pStep->GetDisplayString();
					if (pStep->m_pParent->m_status == statusError) 
						strCheck = "   "; // no check box on bad princ step
						else if (pPrinc->m_bChecked)
					strCheck = pPrinc->m_checkStatus == statusError ? "[x]" : "[+]";
					else // unchecked
						strCheck = "[  ]";

					strLine.Format("      %s %s", strCheck, strStep);
					WRITELINE(pDC, rcLine, strLine, pStep->m_status);
				}
			}
			SKIPLINE();		// blank line after principles if any
		}

		// Print list of variables, including predefined time point vars
		if (GetDocument()->m_nProblemType == PROB_QUANT // only show vars for quant probs
			&& (! GetDocument()->m_Variables.IsEmpty() ||
			    ! GetDocument()->m_strTimes.IsEmpty()) ) {
			WRITELINE(pDC, rcLine, "Variables", statusCorrect);
			// predefined times:
			POSITION pos = GetDocument()->m_strTimes.GetHeadPosition();
			while (pos != NULL) {
				strLine = GetDocument()->m_strTimes.GetNext(pos);
				if (strLine.Find('=') != -1)
					WRITELINE(pDC, rcLine, strLine, statusCorrect);
			}
			// student defs
			pos = GetDocument()->m_Variables.GetHeadPosition();
			while (pos != NULL) {
				CVariable* pVar = GetDocument()->m_Variables.GetNext(pos);
				strLine.Format("%s = %s", pVar->m_strName, pVar->m_strDef);
			
				WRITELINE(pDC, rcLine, strLine, pVar->m_status);
				// add specified given value equation on next line (for room)
				if (!pVar->m_strValue.IsEmpty()) {
				  strLine.Format("    = %s", pVar->m_strValue);
				  WRITELINE(pDC, rcLine, strLine, pVar->m_status);
				}
			}
			SKIPLINE();	// blank line after variables, if any
		}


		// Print list of drawing object definitions
		BOOL bDrawnObjects = FALSE;		// set when hit first one
		POSITION pos = GetDocument()->GetObjects()->GetHeadPosition();
		while (pos != NULL) {
			CDrawObj* pObj = GetDocument()->GetObjects()->GetNext(pos);
			if (pObj->m_flag == STUDENT_OBJECT && // named student entry
				! pObj->m_strName.IsEmpty()) { 
				if (! bDrawnObjects) {
					WRITELINE(pDC, rcLine, "Diagram", statusCorrect);
					bDrawnObjects = TRUE;
				}
				CString strDef = pObj->GetPrintDef();
				strLine.Format("%s %s", pObj->m_strName, strDef);
				WRITELINE(pDC, rcLine, strLine, pObj->m_status);
			}
		}
		if (bDrawnObjects)	SKIPLINE();	// blank after object defs, if any
	
		// Print list of equations
		// Search back from last element to find max non-empty equation field
		int iLast;			// index of last non-empty equation; goes to -1 if none
		for (iLast = NEQS - 1; iLast >= 0; --iLast) { 
			if (pEQView && ! pEQView->IsBlank(iLast))
				break; // found it. 
		} 
		if (iLast >= 0) {	// have at least one equation
			WRITELINE(pDC, rcLine, "Equations", statusCorrect);
			// print numbered equations up to the last non-empty one
			for (int iEq = 0; iEq <= iLast; ++iEq) {
				strLine.Format("%2d.   %s", iEq + 1, pEQView->GetEquation(iEq));
				WRITELINE(pDC, rcLine, strLine, pEQView->GetEqStatus(iEq));
			}
			//SKIPLINE();	// blank line after equations, if any
		}
		// remember height where right equation column ends, in case we need to run into
		// this area for overflow of score.
		int yEqBottom = rcLine.bottom -nHeight;
		BOOL bOverflow = FALSE; // set if we have overflowed

		/* // Append a statistics line
		yPos += 2* nHeight;
		pDC->TextOut(0, yPos, strStats); */

		// print statistics beneath diagram column
		// start at left
		int xPos = pInfo->m_rectDraw.left;
		int yPos = extDiagram.cy + 48; // 1/2 in below bottom extent of diagram
		// yPos = yBottom - nStats * nHeight; // !! some might print over two lines
		rcLine = CRect(xPos, yPos, xRightCol, (yPos+nHeight) );
		
		// print total score line.
		if (!GetDocument()->m_strScore.IsEmpty()) // make sure we have it
		{   // score string should now hold integer between 0 & 100.
			strLine.Format("ANDES Skill Estimate: %s / 100", GetDocument()->m_strScore);
			WRITELINE(pDC, rcLine, strLine, statusCorrect);
		}

		// print score stats one to a line
		if (m_strStats.IsEmpty()) return;    // make sure we got stats
		CStringArray strStats, strFields;
		int nStats = split(m_strStats, ";", strStats);
		for (int iStat = 0; iStat < nStats; iStat++)   // for each stat
		{
			// extract fields from current stat string:
			enum {iName = 0, iWeight = 1, iScore = 2, /* future:*/iSubScore = 3};
			int nFields = split(strStats[iStat], " ", strFields);
			ASSERT(nFields == 3);
			CString strName = strFields[iName];
			CString strWeight = strFields[iWeight];
			CString strScore = strFields[iScore];
			// CString strSubScore = strFields[iSubScore];

			// parse weight string so we can test it and multiply below
			float fWeight = 0.;	
			sscanf(strWeight, "%f", &fWeight);
			if (fWeight != 0) // this stat counts for score
			{
				int nWeight = Round(fWeight*100); // adjust scale to points out of 100
				
				CString strSubScore("??  "); // calc weighted subscore. ? default if we can't calc it
				double fScore;
				if (ScoreToNum(strScore, fWeight, fScore))
					strSubScore.Format("%4.1f", nWeight*fScore);

				// format our stat line
				strName.Replace('_', ' ');
				strLine.Format("%s %s (%d): %s ", strSubScore, strName, nWeight, strScore);

				// fine point: once we are below last equation line (but haven't overflowed left column)
				// we can print all the way to right edge -- might prevent wrapping a long score line.
				// !!! Wide line could run into overflow column. Ignore, overflow unlikely now
				if (rcLine.top > yEqBottom && ! bOverflow)
					rcLine.right = pInfo->m_rectDraw.right;
				WRITELINE(pDC, rcLine, strLine, statusCorrect);
			}

			// check for overflow past bottom of print area and move to right column below equations.
			// No action if we've already made this move (should be rare)
			if ((rcLine.bottom > yBottom) && !bOverflow) 
			{
				xPos = xRightCol; // should keep track of xRightMax for score and use that? 
				yPos = yEqBottom + nHeight;
				rcLine = CRect(xPos, yPos, pInfo->m_rectDraw.right, yPos+nHeight);
				bOverflow = TRUE; // don't do this again. 
			}
		}
	}
	else if (pInfo->m_nCurPage == 2) // Printing second page
	{
		CString strLine;
		int xPos = 0;
		int yPos = 0;
		// Print the header. Updates m_rectDraw to reflect remaining space
		PrintPageHeader(pDC, pInfo, strHeader);

		// Offset logical page window origin to reflect modified pInfo->m_rectDraw
		pDC->SetWindowOrg(pInfo->m_rectDraw.left, -pInfo->m_rectDraw.top);
		TEXTMETRIC tm;
		pDC->GetTextMetrics(&tm);
		int nHeight = tm.tmHeight + tm.tmExternalLeading;
		CRect rcLine = CRect(xPos, yPos, pInfo->m_rectDraw.right, (yPos+nHeight) );

		// print remaining stats one to a line
		if (m_strStats.IsEmpty()) return;    // make sure we got stats
		CStringArray strStats, strFields;
		int nStats = split(m_strStats, ";", strStats);
		for (int iStat = 0; iStat < nStats; iStat++) {
			int nFields = split(strStats[iStat], " ", strFields);
			ASSERT(nFields == 3);
			float fWeight = 0.;	
			sscanf(strFields[1], "%f", &fWeight);	
			if (fWeight == 0) {
				strLine.Format("%s:  %s", strFields[0], strFields[2]);
				WRITELINE(pDC, rcLine, strLine, statusCorrect);
			} 
		}

/*      // once printed plan page here
		// We fetch data from EQView. 
		CHiLevelVw* pHLView = theApp.GetHiLevelVw();
		if (pHLView == NULL)
			return;
		if (! GetDocument()->m_stages.IsEmpty()) 
		{
			POSITION pos = GetDocument()->m_stages.GetHeadPosition();
			while (pos != NULL) 
			{
				CStageObj* pStage = GetDocument()->m_stages.GetNext(pos);
				for (int j=0; j< pStage->m_items.GetSize(); j++)
				{
					COutlineItem* pItem = pStage->m_items[j];
					rcLine.left = pItem->GetPosition().left;
					if (pItem->m_nItemType == ITEM_TABLE){
						for (int i=0; i<pItem->m_controls.GetSize(); i++)
						{
							CItemCtrl* pCtrl = pItem->m_controls[i];
							ASSERT(pCtrl->IsKindOf(RUNTIME_CLASS(CTableGrid)));
							CTableGrid* pGrid = (CTableGrid*)pCtrl;
							pInfo->m_rectDraw.top = rcLine.top;
							pInfo->m_rectDraw.left = rcLine.left;
							pGrid->Print(pDC, pInfo);
							rcLine.top = pInfo->m_rectDraw.bottom;
							rcLine.bottom += nHeight;
						}
					}
					else{
						strLine.Format("%s", pItem->GetPrintDef());
						WRITELINE(pDC, rcLine, strLine, statusUnknown);
					}
				}
				SKIPLINE();	// blank line after stages, if any
			}
		}
*/
	}


    // SetWindowOrg back for next page
    pDC->SetWindowOrg(0,0);
	
	pDC->SelectObject(pOldFont); 
}

void CFBDView::PrintPageHeader(CDC* pDC, CPrintInfo* pInfo, CString& strHeader)
{
	// Specify left text alignment
	pDC->SetTextAlign(TA_LEFT);

	// Print a page header 
	const yStart = 12; // 1/8 inch down in logical units
	pDC->TextOut(0, yStart, strHeader); 

	TEXTMETRIC tm;
	pDC->GetTextMetrics(&tm);
	int dy = yStart + 2* (tm.tmHeight + tm.tmExternalLeading);
	// Subtract header space from drawing rectangle
	pInfo->m_rectDraw.top += dy;
}

void CFBDView::OnEndPrinting(CDC* /*pDC*/, CPrintInfo* /*pInfo*/)
{
	// TODO: add cleanup after printing
}

/////////////////////////////////////////////////////////////////////////////
// CFBDView diagnostics

#ifdef _DEBUG
void CFBDView::AssertValid() const
{
	CView::AssertValid();
}

void CFBDView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CFBDDoc* CFBDView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return (CFBDDoc*)m_pDocument;
}
#endif //_DEBUG

///////////////////////////////////////////////////////////////////////////
// OLE Client support and commands
///////////////////////////////////////////////////////////////////////////

//
// IsSelected -- test if given object is selected. 
//
// Note MFC OLE container support requires us to override 
// CView::IsSelected(const CObject* pDocItem) to tell it if a COleClientItem
// derivative argument is selected. This is used in its automatic updating of
// the object menu items and handling of the verb commands. 
// 
BOOL CFBDView::IsSelected(const CObject* pDocItem) const
{
	CDrawObj* pDrawObj = (CDrawObj*)pDocItem;
	if (pDocItem->IsKindOf(RUNTIME_CLASS(COleDrawItem)))
		pDrawObj = ((COleDrawItem*)pDocItem)->m_pDrawObj;
	else 
		pDrawObj = (CDrawObj*) pDocItem;

	return m_Selection.Find(pDrawObj) != NULL;
}

void CFBDView::OnSetFocus(CWnd* pOldWnd)
{
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL &&
		pActiveItem->GetItemState() == COleClientItem::activeUIState)
	{
		// need to forward focus to active item if it is in the same view
		CWnd* pWnd = pActiveItem->GetInPlaceWindow();
		if (pWnd != NULL)
		{
			pWnd->SetFocus();
			return;
		}
	}

	CScrollView::OnSetFocus(pOldWnd);
}

// Helper notifies active item of changes in its position/clipping rect.
void CFBDView::UpdateActiveItem()
{
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL &&
		pActiveItem->GetItemState() == COleClientItem::activeUIState)
	{
		// this will update the item rectangles by calling
		//  OnGetPosRect & OnGetClipRect.
		pActiveItem->SetItemRects();
	}
}

void CFBDView::OnSize(UINT nType, int cx, int cy) 
{
	CBaseView::OnSize(nType, cx, cy);
	UpdateActiveItem();
}

BOOL CFBDView::OnScrollBy(CSize sizeScroll, BOOL bDoScroll) 
{
#ifndef FBD_SCROLL_VIEW // non-scrolling view code
	return CBaseView::OnScrollBy(sizeScroll, bDoScroll);
#else //  scrolling view code

	// Do the scroll
	if (! CBaseView::OnScrollBy(sizeScroll, bDoScroll))
		return FALSE;
	// update the position of any in-place active item
	if (bDoScroll) {
		UpdateActiveItem();
		UpdateWindow();
	}
	return TRUE;

#endif // Scrolling view code
}

void CFBDView::OnDestroy()
{
	CScrollView::OnDestroy();
	// deactivate the inplace active item on this view
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL && pActiveItem->GetActiveView() == this)
	{
		pActiveItem->Deactivate();
		ASSERT(GetDocument()->GetInPlaceActiveItem(this) == NULL);
	}
}

//
// OLE-related command handlers
//
void CFBDView::OnUpdateInsertObject(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(theApp.m_bAuthorMode);
	
}

// returns default position of new object. Extents will be overridden by querying
// server if possible in item's UpdateFromServerExtent.
CRect CFBDView::GetInitialPosition() 
{
	CRect rect(96, 96, 192, 192);
	// ClientToDoc(rect);
	return rect;
}

// Insert a new OLE object via the standard dialog.
void CFBDView::OnInsertObject()
{
	// Invoke the standard Insert Object dialog box to obtain information
	//  for new COleDrawItem object.
	COleInsertDialog dlg;
	if (dlg.DoModal() != IDOK)
		return;

	BeginWaitCursor();

	// First create the C++ objects
	COleDrawObj* pObj = new COleDrawObj(GetInitialPosition());
	ASSERT_VALID(pObj);
	COleDrawItem* pItem = new COleDrawItem(GetDocument(), pObj);
	ASSERT_VALID(pItem);
	pObj->m_pClientItem = pItem;

	// Now create the OLE object/item
	TRY
	{
		if (!dlg.CreateItem(pObj->m_pClientItem))
			AfxThrowMemoryException();

		// add the object to the document
		GetDocument()->Add(pObj);

		// try to get initial presentation data and update extent
		pItem->UpdateLink();
		pItem->UpdateFromServerExtent();

		// if item created from class list (not from file) then launch
		// the server to edit the item.
		if (dlg.GetSelectionType() == COleInsertDialog::createNewItem)
			pItem->DoVerb(OLEIVERB_SHOW, this);
	}
	CATCH_ALL(e)
	{
		// clean up item
		pItem->Delete();
		pObj->m_pClientItem = NULL;
		GetDocument()->Remove(pObj);
		pObj->Delete();

		AfxMessageBox(IDP_FAILED_TO_CREATE);
	}
	END_CATCH_ALL

	EndWaitCursor();
}

// The following command handler provides the standard keyboard
//  user interface to cancel an in-place editing session.
void CFBDView::OnCancelEdit()
{
	// deactivate any in-place active item on this view!
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL)
	{
		// if we found one, deactivate it
		pActiveItem->Close();
	}
	ASSERT(GetDocument()->GetInPlaceActiveItem(this) == NULL);

	// escape also brings us back into select mode
	ReleaseCapture();
	m_drawMode = Selector;
}


// Helper for paste command: 
// inserts an OLE embedded object from an OLE data object, presumably
// retrieved from clipboard. This helper could also be used to
// handle OLE drag/drop as in DRAWCLI. 
void CFBDView::PasteEmbedded(COleDataObject& dataObject, CPoint point )
{
	BeginWaitCursor();

	// paste embedded object into document
	COleDrawObj* pObj = new COleDrawObj(GetInitialPosition());
	ASSERT_VALID(pObj);
	COleDrawItem* pItem = new COleDrawItem(GetDocument(), pObj);
	ASSERT_VALID(pItem);
	pObj->m_pClientItem = pItem;

	TRY
	{
		if (!pItem->CreateFromData(&dataObject) &&
			!pItem->CreateStaticFromData(&dataObject))
		{
			AfxThrowMemoryException();      // any exception will do
		}

		// add the object to the document
		GetDocument()->Add(pObj);

		// add it to selection
		m_Selection.AddTail(pObj);	// doesn't update yet, pos may change

		// Adjust position in document to point
		ClientToDoc( point );
		pObj->MoveTo( CRect( point, pObj->m_extent ), this );

		// try to get initial presentation data and adjust extent from server
		pItem->UpdateLink();
		pItem->UpdateFromServerExtent();
	}
	CATCH_ALL(e)
	{
		// clean up item
		pItem->Delete();
		pObj->m_pClientItem = NULL;
		GetDocument()->Remove(pObj);
		pObj->Delete();

		AfxMessageBox(IDP_FAILED_TO_CREATE);
	}
	END_CATCH_ALL

	EndWaitCursor();
}

/////////////////////////////////////////////////////////////////////////////
// Support for inserting embedded picture (not a full OLE embedded item)
//////////////////////////////////////////////////////////////////////////////
void CFBDView::OnUpdateInsertpicture(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(theApp.m_bAuthorMode);
}
void CFBDView::OnInsertpicture() 
{
	// run a file dialog to get the image file
	CFileDialog dlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "All supported image types|*.bmp;*.gif;*.jpg;*.emf;*.wmf;*.ico|Bitmaps (*.bmp)|*.bmp|GIF Files (*.gif)|*.gif|JPEG Files (*.jpg)|*.jpg|Enhanced Metafiles (*.emf)|*.emf|Windows Metafiles (*.wmf)|*.wmf|Icons (*.ico)|*.ico||");
    if (dlg.DoModal() == IDCANCEL)
		return;

	// create a new CDrawPicture from file
    CString strPathName = dlg.GetPathName();
	CDrawPicture* pPic = CDrawPicture::CreateFromFile(strPathName);
	if (pPic == NULL)
		return;

	// Add it to the document and update.
	GetDocument()->Add(pPic);

	//  Make new object selected. (Invalidates it in view).
	Select(pPic);
	// Notify other views of new object
	GetDocument()->UpdateAllViews(this, HINT_UPDATE_DRAWOBJ, pPic);
}



/////////////////////////////////////////////////////////////////////////////
// CFBDView message handlers

//
// Drawing tool selection. 
//
// Most commands just have to set a mode flag. These handled by generic handlers for
// command range ID_DRAWTOOL_FIRST to ID_DRAWTOOL_LAST. 
//
// Typed vector drawing modes are represented by a common major mode plus subtype.
// So full mode is ordered pair: <m_drawMode == Vector, m_vectorType = type>
// (Dates from before we had typed vector draw tools)
//
void CFBDView::OnSelectDrawTool(UINT nID)
{
	LogEventf(EV_SELECT_TOOL,"%d", nID);
	m_drawMode = (drawMode) nID;
	if (theApp.m_bTrainMode){
		DWORD dwId;
		if (m_drawMode== System)
			dwId = ID_TBODY_TOOL;
		else if (m_drawMode == MotionBody)
			dwId = ID_TPOSITION_TOOL;
		else if (m_drawMode == MotionDiagram)
			dwId = ID_TRULER_TOOL;
		
		theApp.SendTrainer(this, dwId);
	}
}

void CFBDView::OnUpdateDrawTool(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(m_bEnabled);
	pCmdUI->SetRadio(pCmdUI->m_nID == (UINT) m_drawMode);
}

// !!! Are other handlers for commands in this range also called?
// I.e. for special purpose enabling?

// Special cases for vectors:

// untyped vector draw command (not used anymore)
void CFBDView::OnDrawVector() 
{
	m_drawMode = Vector;
	m_vectorType = -1;
}
void CFBDView::OnUpdateDrawVector(CCmdUI* pCmdUI) 
{   
	pCmdUI->Enable(m_bEnabled);
    pCmdUI->SetCheck(m_drawMode == Vector);
}

// Typed vector drawing: 
void CFBDView::OnDrawvectorAcceleration() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_ACCELERATION);
	m_drawMode = Vector; m_vectorType = VECTOR_ACCELERATION;
	m_nZDir = ZDIR_NONE; 

	if (theApp.m_bTrainMode){
		// same tool used in fbd and motion diagrams. Rather then figure out ID of next
		// card, just send trainer to next in card's browse sequence (set in helpfile).
		NextInCard();
	}
}
void CFBDView::OnUpdateDrawvectorAcceleration(CCmdUI* pCmdUI) 
{ 
  pCmdUI->SetCheck(m_drawMode == Vector && m_vectorType == VECTOR_ACCELERATION); 
  pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorComponent() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_COMPONENT);
	m_drawMode = Vector; m_vectorType = VECTOR_COMPONENT;
	m_nZDir = ZDIR_NONE; 
}
void CFBDView::OnUpdateDrawvectorComponent(CCmdUI* pCmdUI) 
{ 
	BOOL bHaveAxes = GetDocument()->IsAxesDrawn();
	// Enable just in case Axes exist. Note can draw comp then delete axes.
	pCmdUI->Enable(bHaveAxes && m_bEnabled);

	// Still also have to set button state as command is current tool
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_COMPONENT); 
}

void CFBDView::OnDrawvectorForce() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_FORCE);
	m_drawMode = Vector; m_vectorType = VECTOR_FORCE;
	m_nZDir = ZDIR_NONE; 
	theApp.SendTrainer(this, ID_TFORCE_TOOL);
}

void CFBDView::OnUpdateDrawvectorForce(CCmdUI* pCmdUI) 
{ 
  pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_FORCE); 
  pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorVelocity() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_VELOCITY);
	m_drawMode = Vector; m_vectorType = VECTOR_VELOCITY; 
	m_nZDir = ZDIR_NONE; 
	if (theApp.m_bTrainMode){
		// same tool used in fbd and motion diagrams. Rather then figure out ID of next
		// card, just send trainer to next in card's browse sequence (set in helpfile).
		NextInCard();
	}
	
}
void CFBDView::OnUpdateDrawvectorVelocity(CCmdUI* pCmdUI) 
{ 
  pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_VELOCITY); 
  pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorDisplacement() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_DISPLACEMENT);
	m_drawMode = Vector; m_vectorType = VECTOR_DISPLACEMENT;
	m_nZDir = ZDIR_NONE; 
	if (theApp.m_bTrainMode){
		// following copied from accel and velocity, but not really necessary, since 
		// we can know exactly which card is next.
		NextInCard();
	}
}

void CFBDView::OnUpdateDrawvectorDisplacement(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_DISPLACEMENT);
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorMomentum() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_MOMENTUM);
	m_drawMode = Vector; m_vectorType = VECTOR_MOMENTUM;
	m_nZDir = ZDIR_NONE; 
}
void CFBDView::OnUpdateDrawvectorMomentum(CCmdUI* pCmdUI) 
{	
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_MOMENTUM); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorImpulse() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_IMPULSE);
	m_drawMode = Vector; m_vectorType = VECTOR_IMPULSE;
	m_nZDir = ZDIR_NONE; 
}
void CFBDView::OnUpdateDrawvectorImpulse(CCmdUI* pCmdUI) 
{	
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_IMPULSE); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorUnitVector() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_UNITVECTOR);
	m_drawMode = Vector; m_vectorType = VECTOR_UNITVECTOR;
	m_nZDir = ZDIR_NONE; 
}
void CFBDView::OnUpdateDrawvectorUnitVector(CCmdUI* pCmdUI) 
{	
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_UNITVECTOR); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorMagDipole() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_MAGDIPOLE);
	m_drawMode = Vector; m_vectorType = VECTOR_MAGDIPOLE;
	m_nZDir = ZDIR_NONE; 
}
void CFBDView::OnUpdateDrawvectorMagDipole(CCmdUI* pCmdUI) 
{	
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_MAGDIPOLE); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorElecDipole() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_ELECDIPOLE);
	m_drawMode = Vector; m_vectorType = VECTOR_ELECDIPOLE;
	m_nZDir = ZDIR_NONE; 
}
void CFBDView::OnUpdateDrawvectorElecDipole(CCmdUI* pCmdUI) 
{	
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_ELECDIPOLE); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorTorque() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_TORQUE);
	m_drawMode = Vector; m_vectorType = VECTOR_TORQUE;
	m_nZDir = ZDIR_NONE; 
}

void CFBDView::OnUpdateDrawvectorTorque(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_TORQUE); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorRelpos() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_RELPOS);
	m_drawMode = Vector; m_vectorType = VECTOR_POSITION;
	m_nZDir = ZDIR_NONE; 
}

void CFBDView::OnUpdateDrawvectorRelpos(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_POSITION); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorEfield() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_EFIELD);
	m_drawMode = Vector; m_vectorType = VECTOR_EFIELD;
	m_nZDir = ZDIR_NONE; 	
}

void CFBDView::OnUpdateDrawvectorEfield(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_EFIELD); 
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnDrawvectorBfield() 
{
	LogEventf(EV_SELECT_TOOL,"%d", ID_DRAWVECTOR_BFIELD);
	m_drawMode = Vector; m_vectorType = VECTOR_BFIELD;
	m_nZDir = ZDIR_NONE; 	
}

void CFBDView::OnUpdateDrawvectorBfield(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(m_drawMode == Vector && m_vectorType == VECTOR_BFIELD); 
	pCmdUI->Enable(m_bEnabled);
}

// Custom enabling for Motion diagrams
void CFBDView::OnUpdateMotionBody(CCmdUI* pCmdUI) 
	{ pCmdUI->Enable(HaveMotionArea() && m_bEnabled);
	  pCmdUI->SetRadio(m_drawMode == MotionBody);}

// Custom enabling needed for Angle
void CFBDView::OnUpdateAngle(CCmdUI* pCmdUI) 
{	
	pCmdUI->SetRadio(m_drawMode == Angle);
	if (m_Selection.GetCount()!=2)//exactly 2 things must be selected so there is
		pCmdUI->Enable(FALSE);		//no ambiguity
	else{
		CDrawObj* pObj1 = m_Selection.GetHead();
		CDrawObj* pObj2 = m_Selection.GetTail();
		CPoint p1(ExtendStartLine(pObj1, -1));///!!!!!!!!!!!!!!1
		CPoint p2(pObj1->m_position.right, pObj1->m_position.bottom);
		if (pObj1->IsKindOf(RUNTIME_CLASS(CAxes)))
			p2 = ((CAxes*)pObj1)->GetAxisPoint();
		CPoint p3(ExtendStartLine(pObj2, -1));
		CPoint p4(pObj2->m_position.right, pObj2->m_position.bottom);
		if (pObj2->IsKindOf(RUNTIME_CLASS(CAxes)))
			p4 = ((CAxes*)pObj2)->GetAxisPoint();

		if (TwoLinesSelected(pObj1, pObj2) && Intersect(p1, p2, p3, p4))
			pCmdUI->Enable(m_bEnabled);
		else
			pCmdUI->Enable(FALSE);
	}
}

// author tool for defining areas (basically a rect tool
void CFBDView::OnUpdateDocArea(CCmdUI* pCmdUI) 
	{pCmdUI->SetRadio(m_drawMode == MotionBody);
	 pCmdUI->Enable(theApp.m_bAuthorMode); }
// Label tool is a push button, not a drawing mode; implementation below.
///////////////////////////////////////////////////////////////////////////////
//
// Handlers for text item commands -- These are not really modes but rather
// command buttons that pop up dialog boxes to collect text
//
///////////////////////////////////////////////////////////////////////////////
void CFBDView::OnLabel() 
{
	m_drawMode = Label;
	//
	// For now, just pop up a dialog to collect a line of text
	//
	CLabelDlg dlg;
	dlg.m_bVisible = TRUE;	// Text labels visible in example mode
	if (! (HFONT) m_fontText) // no font selected yet
	{
		CClientDC dc(this);
		CFont* pScrnFont = dc.GetCurrentFont();
		pScrnFont->GetLogFont(&dlg.m_logFont);
	}
	else // want font in device units, not logical, for font choice dialog
		m_fontText.GetLogFont(&dlg.m_logFont);

	if (dlg.DoModal() == IDOK) 
	{
		// Create new label object, add it at default position to diagram
		// and leave it selected. User must drag to position it
	
		// Create at default pos (logical) w/dummy extent to be fixed up below
		CLabel* pLabel = new CLabel(CPoint(10,10), dlg.m_strText, &dlg.m_logFont);
		
		// set example mode flags from dialog (obsolete?)
		pLabel->m_bMask = dlg.m_bMask;
		pLabel->m_bVisible =  (pLabel->m_bMask) ? TRUE : dlg.m_bVisible;

		// Add new text piect to document
		GetDocument()->Add(pLabel);
		// A service to authors: place new mask objects behind all other objects
		if (pLabel->m_bMask)
			GetDocument()->MoveToBack(pLabel);

		// Log new text
		LogEventf(EV_OBJ_TEXT,"%s %d %d %d %d", pLabel->m_strId, pLabel->m_position.left, 
			     pLabel->m_position.top, pLabel->m_position.bottom, pLabel->m_position.right);

		//  Make new object selected. (Invalidates it in this view).
		m_pCurrentObj = pLabel;
		Select(pLabel);
		// Notify any other views of new object
		GetDocument()->UpdateAllViews(this, HINT_UPDATE_DRAWOBJ, pLabel);
	}

	// Leave in select mode
	m_drawMode = Selector;
}

// 
// Example text command: For example item editing, author mode only:
// 
// Just like a label which the user can enter, but uses a different
// dialog to include example mode properties. (!! to be taken out
// of the user label dialog)
//
void CFBDView::OnExampleText() 
{
	//
	// For now, just pop up a dialog to collect a line of text
	//
	CEXTextDlg dlg;
	dlg.m_bVisible = FALSE;
	dlg.m_strMenu = "(None)";
	
	if (dlg.DoModal() == IDOK) 
	{
		// Create label object, add it at default position to diagram
		// and leave it selected. User must position it properly.
		// First compute extent of position rect from text size:
		CClientDC dc(this);		// for current font, mapping mode
		// CSize extent = dc.GetTextExtent(dlg.m_strText); 
		CRect pos(CPoint(10, 10), CSize(1, 1));	// default pos w/dummy extent (logical)
		(void) dc.DrawText(dlg.m_strText, pos, DT_CALCRECT); // calcs bounds into pos

		// build new example text item
		CEXText* pEXT = new CEXText(pos, dlg.m_strText);
		pEXT->m_bVisible = dlg.m_bVisible;
		pEXT->m_strMenu = dlg.m_strMenu;
		pEXT->m_strId = dlg.m_strId;

		// add to document. Generate ID only if author didn't specify one in dialog
		m_pCurrentObj = pEXT;
		GetDocument()->Add(pEXT, /* bGenID:*/ pEXT->m_strId.IsEmpty());
		Logf("EXText %s %d %d %d %d", pEXT->m_strId, pEXT->m_position.left, 
			     pEXT->m_position.top, pEXT->m_position.bottom, pEXT->m_position.right);

		// leave new object selected (invalidates in view)
		Select(pEXT);
		// Notify all other views of new object
		GetDocument()->UpdateAllViews(this, HINT_UPDATE_DRAWOBJ, pEXT);
	}
	m_drawMode = Selector;
}

// Example text tool only used in editing mode:
void CFBDView::OnUpdateExampleText(CCmdUI* pCmdUI) 
	{ pCmdUI->Enable(theApp.m_bAuthorMode); }

// Author command to add a single choice item.
// We allow them interpret groups of choice items as mutually exclusive sets
void CFBDView::OnUpdateChoiceItem(CCmdUI* pCmdUI) 
	{ pCmdUI->Enable(theApp.m_bAuthorMode);}

void CFBDView::OnChoiceItem() 
{
	// Create a new individual choice item and let author edit its properties
	CChoice* pChoice = new CChoice(CRect(100, 100, 200, 250));
	pChoice->m_strName = "<Choice>";
	// must be parented to document for post-edit invalidating to work.
	GetDocument()->Add(pChoice);
	Select(pChoice);				// Invalidates obj in view.

	if (pChoice->OnEditProperties() != IDOK) // cancelled
	{
		GetDocument()->Remove(pChoice);
		pChoice->Delete();
		return;
	}
	// If OK'd, obj invalidated by OnEditProps.
}

// Under construction: new interface to make building multiple choice questions
// a little easier for authors: Build all parts from one dialog.
void CFBDView::OnMcQuestion() 
{
#if 0	// not fully implemented, don't confuse folks
	// run multiple choice building dialog
	CMCQDlg dlg;
	if (dlg.DoModal() != IDOK) 
		return;
	// Now must build a new multiple choice question obj from dialog data
	// and add to the document. Must layout part items somewhere, either here
	// or when we display the question data.
#endif 
}

// Align to Grid mode
void CFBDView::OnAligntogrid() 
{
	m_bAlignGrid = ! m_bAlignGrid;		// Toggle command
}

void CFBDView::OnUpdateAligntogrid(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_bAlignGrid);
}

void CFBDView::OnViewGrid() 
{
	m_bShowGrid = ! m_bShowGrid;		// Toggle command
	Invalidate();
}

void CFBDView::OnUpdateViewGrid(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_bShowGrid);
	
}

//
// Mutate point to align to nearest grid point if grid alignment on
//
// Use this before creating new object or moving/resizing old one. Not when
// selecting since need to be able to select or grab objects even if they are
// off the grid.
//
void CFBDView::AlignToGrid(CPoint& point)
{
	if (! m_bAlignGrid)
		return;

	int xoff = point.x % m_cxGrid;
	int yoff = point.y % m_cyGrid;
	point.x = (xoff < m_cxGrid/2) ? point.x - xoff : point.x + m_cxGrid - xoff;
	point.y = (yoff < m_cxGrid/2) ? point.y - yoff : point.y + m_cyGrid - yoff;
}

void CFBDView::AlignToGrid(CRect& rect)
{
	// We aligning a rect to grid by aligning its top-left corner. Note this 
	// may not be top-left on screen if rect is not normalized.
	CPoint newTopLeft = rect.TopLeft();
	AlignToGrid(newTopLeft);
	CSize delta = newTopLeft - rect.TopLeft();
	rect += delta;
}

////////////////////////////////////////////////////////////////////////////////////
// Motion diagram helpers
//
// For historical reasons, MotionDiagrams are identified with the ruler object, which
// is like the master object that controls the others. The associated DocArea object 
// is only used to define the "hot" region for motion drawing cmds. The DocArea can
// either be pre-defined by author (w/ruler to be added) or created around a student-drawn
// ruler. !!! This really should be cleaned up to use just one object for both purposes.

// HaveMotionArea: true iff there is currently a motion diagram area in the
// document. Used to enable motion diagram toolbar/menu commands. 
// We don't check if it is selected or anything, just enable commands if it exists.
//
BOOL CFBDView::HaveMotionArea()
{
	CFBDDoc* pDoc = GetDocument();

	POSITION pos = pDoc->m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = pDoc->m_objects.GetPrev(pos);
		if ( pObj->IsKindOf(RUNTIME_CLASS(CDocArea)) 
			&& pObj->m_strName == "Motion Diagram" )// found a diagram box.
		{	
			return TRUE; 
		}
	}
	return FALSE;
}

// Find Motion diagram area containing given logical point, NULL if none.
CDocArea* CFBDView::InMotionArea(CPoint local)
{
	CFBDDoc* pDoc = GetDocument();

	POSITION pos = pDoc->m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = pDoc->m_objects.GetPrev(pos);
		if ( pObj->IsKindOf(RUNTIME_CLASS(CDocArea)) 
			&& pObj->m_strName == "Motion Diagram" )// found a diagram box.
		{
			if (pObj->m_position.PtInRect(local))
				return (CDocArea*) pObj; 
		}
	}
	return NULL;
}

// Find ruler object containing given logical pt, NULL if none.
// Needed because no mapping from DocArea found above to ruler obj, urgh.
CMotionDiagram* CFBDView::FindMotionRuler(CPoint local)
{
	CFBDDoc* pDoc = GetDocument();

	// For each ruler in document...
	POSITION pos = pDoc->m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = pDoc->m_objects.GetPrev(pos);
		if ( pObj->IsKindOf(RUNTIME_CLASS(CMotionDiagram)) )
		{
			//see if point is in this ruler's associated area 
			CMotionDiagram* pRuler = (CMotionDiagram*) pObj;
			if (pRuler->m_pArea &&
				pRuler->m_pArea->m_position.PtInRect(local))
				return pRuler;
		}
	}
	return NULL;
}

// Find pair of combinable motion diagrams. Currently that means
// uncombined horizontal and vertical motion diagrams for the  same object. [& completed?]
// Returns TRUE if successful, filling in reference parms with diagrams, FALSE otherwise.
BOOL CFBDView::GetComponentDiagrams(CMotionDiagram* &pmdX, CMotionDiagram* &pmdY)
{
	CFBDDoc* pDoc = GetDocument();
	// In our use, just check for exactly two diagrams, on horizontal, one vertical,
	// not already combined, and completed.
	pmdX = NULL; 
	pmdY = NULL;
	POSITION pos = pDoc->m_objects.GetTailPosition();
	while (pos != NULL) {
		CDrawObj* pObj = pDoc->m_objects.GetPrev(pos);
		if ( pObj->IsKindOf(RUNTIME_CLASS(CMotionDiagram)) )
		{
			CMotionDiagram* pRuler = (CMotionDiagram*) pObj;
			// check if uncombined [& complete?]
			if (pRuler->m_p2DParent == NULL /* && pRuler->IsComplete() */) 
			{
				// found a candidate for combination
				if (pRuler->m_position.top == pRuler->m_position.bottom) // horizontal
					pmdX = pRuler;
				if (pRuler->m_position.left == pRuler->m_position.right) // vertical
					pmdY = pRuler;
			}
		}
	}
	// !!! should verify body and interval are the same
	return (pmdX && pmdY);
}

// Update the Combined motion diagram button.
// Enabled iff have two existing uncombined motion diagrams for the same object,
//  one horizontal, one vertical, both of which are completed. 
void CFBDView::OnUpdate2dmotion(CCmdUI* pCmdUI) 
{
	CMotionDiagram* pmdX, *pmdY;	// unused 
	pCmdUI->Enable(GetComponentDiagrams(pmdX, pmdY) && m_bEnabled);
}

void CFBDView::On2dmotion() 
{
	// Create combined diagram with constructor
	CMotionDiagram* pmdX, *pmdY;	// unused 
	GetComponentDiagrams(pmdX, pmdY);
	C2DMotion *p2D = new C2DMotion(pmdX, pmdY);

	// Add to document and update
	GetDocument()->Add(p2D);
	p2D->Invalidate();
}

// Find 2D Motion diagram containing given logical point, NULL if none.
C2DMotion* CFBDView::In2DArea(CPoint local)
{
	CFBDDoc* pDoc = GetDocument();

	POSITION pos = pDoc->m_objects.GetTailPosition();
	while (pos != NULL){
		CDrawObj* pObj = pDoc->m_objects.GetPrev(pos);
		if ( pObj->IsKindOf(RUNTIME_CLASS(C2DMotion)) ) // found a diagram box.
		{
			if (pObj->m_position.PtInRect(local))
				return (C2DMotion*) pObj; 
		}
	}
	return NULL;
}

////////////////////////////////////////////////////////////////////////////////
// Mouse event handlers -- what to do depends on the drawing mode
//
// Basically implements a state machine. Major state is drawing tool,
// some tools have different sub-states as well. In particular the selector
// can be used in resize or move mode.
//
// !!! Should factor big switches out into classes for each mode.
////////////////////////////////////////////////////////////////////////////////

//
// Handle Left button press:
//
void CFBDView::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CPoint local = point;		// local = point in logical coords
	ClientToDoc(local);

	// log "low-level" mouse event. In both coordinate systems
	LogEventf(EV_LBUTTON_DOWN, "%d %d  %d %d", point.x, point.y, local.x, local.y); 

	// deactivate any in-place active item on this view!
	COleClientItem* pActiveItem = GetDocument()->GetInPlaceActiveItem(this);
	if (pActiveItem != NULL){
		pActiveItem->Close();
		ASSERT(GetDocument()->GetInPlaceActiveItem(this) == NULL);
	}
	// Kill focus of any answer box edit control (like an active item)
	if (IsActiveEdit()) {
		SetFocus();		// view grabs the focus
	}

	if ((!theApp.m_bAuthorMode) && (HyperAt(point) != NULL)){
		HCURSOR hCursor = AfxGetApp()->LoadStandardCursor(IDC_ARROW);
		::SetCursor(hCursor);
		CHyperLnk* pLnk = HyperAt(point);
		pLnk->HyperActivate(point, this);
		return;
	}

	// Dispatch to handler for current drawing mode:
	CDrawObj* pObj = NULL;
	switch (m_drawMode) 
	{
	case notDrawing: 
		return;

	case Selector: {	
		m_selectMode = none;	// until we figure out submode

		// if have selection, check if this click is a resize handle grab
		// (only allowed on single selections)
		if (SingleSelection()) {
			pObj = SelectedObj();
			m_nDragHandle = pObj->HitTest(local, this, TRUE);
			if (m_nDragHandle != 0) {
				m_selectMode = resize;
				SetCursor(pObj->GetHandleCursor(m_nDragHandle));
				LogEventf(EV_BEGIN_RESIZE, "%s |%s| %d %d %d", pObj->m_strId, OBJ_NAME(pObj), m_nDragHandle, 
					             local.x, local.y);
			}
		}

		// else see if clicked on an object; if so, select it and begin possible move
		if (m_selectMode == none){
			if ((pObj = GetDocument()->ObjectAt(local, /*ignore*/ Unselectable)) != NULL) {
				m_selectMode = move;
				SetCursor(AfxGetApp()->LoadStandardCursor(IDC_SIZEALL));
				// We track a "virtual" position when grid alignment is on
				// That is position it would be but for constraint. 
				m_posVirtual = pObj->m_position;
				// need to log mouse key modifier to see if we added to selection
				LogEventf(EV_BEGIN_MOVE, "%s |%s| %d %d %d", pObj->m_strId, OBJ_NAME(pObj), 
					local.x, local.y, (nFlags & MK_CONTROL) != 0);
				// Do the select
				if ( ( IsSelected(pObj) && (pObj->GetHit() != pObj->GetSelectedPart()) ) ||
						!IsSelected(pObj) ){// hit new selected object or part
					Select(pObj, (nFlags & MK_CONTROL) != 0);
				}
				else if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
					InvalObjInView(pObj); // in case click different part of axes
			}
		}

		// else clicked on background: clear sel unless add, begin possible net select
		if (m_selectMode == none) {
			LogEventf(EV_CLICK_BG, "%d %d %d", local.x, local.y, (nFlags & MK_CONTROL) != 0); 
			if ((nFlags & MK_CONTROL) == 0)	// not adding to selection
				Select(NULL);
			// draw net rect
			CClientDC dc(this);
			m_selectMode = netSelect;
			CRect rect(point.x, point.y, point.x, point.y);
			rect.NormalizeRect();
			dc.DrawFocusRect(rect);
		}

		m_ptLastLocal = local;
	} break;	// end select tool begin

	case Polygon: 
	case BezierCurve: 
	{
		AlignToGrid(local);
		// if no current object, create a new empty polygon, else adding pt to existing
		if (m_pCurrentObj == NULL) {
			pObj = NewDrawObj(m_drawMode, local);
			ASSERT(pObj != NULL);
			GetDocument()->Add(pObj);
			LogEventf(EV_BEGIN_DRAW, "%d %s  %d %d", m_drawMode, pObj->m_strId, local.x,  local.y);
			Select(pObj);
		} else
			pObj = m_pCurrentObj;

		// Add current point to polygon
		ASSERT(pObj && pObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)));
		CDrawPoly* pPoly = (CDrawPoly*) pObj;
		pPoly->AddPoint(local, this);
		
		// Create and add an additional point to track as mouse moves.
		// offset by one grid unit so it will not be on top of this one
		CPoint ptNext = local;
		ptNext.x += m_cxGrid; // adjacent points can't be the same!
		AlignToGrid(ptNext);
		pPoly->AddPoint(ptNext, this);
		m_ptLastLocal = ptNext;

		// and go into select/resize mode.
		m_nDragHandle = pPoly->GetHandleCount();
		m_selectMode = resize;
	} break;


	//
	// All other tools are object adders: they begin drawing a new object by
	// adding a default-sized object (usually 0-sized) and letting user resize it
	//
	default:				// BeginDraw event
		AlignToGrid(local);
		pObj = NewDrawObj(m_drawMode, local);
		if (pObj != NULL) {
			GetDocument()->Add(pObj);
			// Log here because creates a new object ID which can be referred to
			// Earlier versions didn't log sub-type for vector draw
			if (m_drawMode == Vector) 
				LogEventf(EV_BEGIN_VECTOR, "%d %s  %d %d", m_vectorType, pObj->m_strId, local.x,  local.y);
			else
				LogEventf(EV_BEGIN_DRAW, "%d %s  %d %d", m_drawMode, pObj->m_strId, local.x,  local.y);
				
			Select(pObj);	// will mark new obj dirty, this view only
		}
		m_ptLastLocal = local;
		// Move axes to background so they don't obscure vectors [desirable?]
		// Ditto for radius [desirable?]
		if ( pObj->IsKindOf(RUNTIME_CLASS(CAxes)) ||
			pObj->IsKindOf(RUNTIME_CLASS(CRadius)) )
			GetDocument()->MoveToBackStudent(pObj);
		// Also, as service to authors, move DocArea markers to background
		if ( pObj->IsKindOf(RUNTIME_CLASS(CDocArea)) )
			GetDocument()->MoveToBack(pObj);
		break;
	} // End switch on drawMode

	// Following bookkeeping common to all mouse tracking tools
	m_ptDown = m_ptPrev = point;
	m_pCurrentObj = pObj;
	SetCapture();
}

// NewDrawObj: Helper creates a new drawing object at the given point
//
// Instead of ugly big switch, we could try to use the MFC dynamic creation facility
// to create a new object from a CRuntimeClass. But: would still need to find
// class from drawtool code, and would also have to initialize, since dynamic
// creation relies on the objects parameterless constructor. And some of our
// objects need sub-types as well, e.g. types of CDrawRect.
//
CDrawObj* CFBDView::NewDrawObj(drawMode type, CPoint local)
{
	CDrawObj* pObj;
	switch (type)
	{
	case Vector: {
		if (theApp.m_bTrainMode){
			DWORD dwId ;
			if (m_vectorType == VECTOR_ACCELERATION){
				if (FindMotionRuler(local))
					dwId = ID_TMOTACC_BDOWN;
				else
					dwId = ID_TACCELERATION_BDOWN;
			}				
			else if (m_vectorType == VECTOR_VELOCITY){
				if (FindMotionRuler(local))
					dwId = ID_TMOTVEL_BDOWN;
				else
					dwId = ID_TVELOCITY_BDOWN;
			}
			else if (m_vectorType == VECTOR_DISPLACEMENT) {
				dwId = ID_TDISPLACEMENT_BDOWN;	// temporary filler
			}
			else
				dwId = ID_TFORCE_BDOWN;
			theApp.SendTrainer(this, dwId);
		}
	
		// use different class if drawn in motion diagram here.
		if (FindMotionRuler(local) || In2DArea(local) ) 
			pObj = new CMDVector(CRect(local, local));
		else if (m_nZDir) { // special ZDir vector
			const int r = CVector::ZVEC_RADIUS;
			pObj = new CVector(CRect(local.x-r, local.y-r, local.x+r, local.y+r));
			((CVector*)pObj)->m_nZDir = m_nZDir;
			// init Zdir vecs to angular quantity in RotKin problems (can change in dlg).
			if (GetDocument()->m_wConcept & ID_PROB_ROTKINEMATICS)		
				((CVector*)pObj)->m_bAngular = TRUE;
		} else // normal vector, starts out zero-length
			pObj = new CVector(CRect(local, local));
		// initialize vector type if specified by command choice (affects drawing)
		if (m_vectorType != -1) {
			ASSERT(VECTYPE_FIRST <= m_vectorType && m_vectorType <= VECTYPE_LAST);
			// if problem flag is set, Velocity tool draws Relative-Velocity vector
			if (m_vectorType == VECTOR_VELOCITY
				&& (GetDocument()->m_wConcept & ID_PROB_RELVEL)) {
				((CVector*)pObj)->m_nVectorType = VECTOR_RELVEL; 
			} else { // just set it from drawtool type, converted
				((CVector*)pObj)->m_nVectorType = (int) m_vectorType;
			}
		}
		
	}break;
	case System:{
		// check if sys drawn in motion diagram, treat as motion body instead
		CMotionDiagram* pRuler = FindMotionRuler(local);
		if (pRuler) {
			// Change drawmode in midstream to continue as if MotionBody command selected
			m_drawMode = MotionBody; 
			const int r = CMotionBody::RADIUS;
			pObj = new CMotionBody(CRect(local.x-r, local.y-r, local.x+r, local.y+r));
			break;
		}
		pObj = new CSystem(CRect(local.x-12, local.y-12, local.x+12, local.y+12));
		
	}break;

	case Radius:{
		pObj = new CRadius(CRect(local.x-40, local.y-40, local.x+40, local.y+40));
	}break;

	case Axes: {
		pObj = new CAxes(CRect(local, local));
		// Set index (subscript) on axes
		((CAxes*) pObj)->m_nIndex = GetDocument()->GetNextAxesIndex();
	}break;
	case MotionDiagram: {
		pObj = new CMotionDiagram(CRect(local, local));
		// if ruler draw started in existing motion area, link ruler to it
		CDocArea* pArea = InMotionArea(local);
		if (pArea != NULL)
			((CMotionDiagram*)pObj)->m_pArea = pArea;
		// else will create and add a new doc area around it when done.
		// currently can't do it here since CreateArea method depends on document
		theApp.SendTrainer(this, ID_TRULER_BDOWN);
	} break;
	case MotionBody: {
		// Could check position here!
		pObj = new CMotionBody(CRect(local.x-10, local.y-10, local.x+10, local.y+10));
		theApp.SendTrainer(this, ID_HELP_DRAWMOTVEL);
	}break;
	case GuideLine: {
		pObj = new CGuideLine(CRect(local, local));
	} break;
	case Rectangle: {
		pObj = new CDrawRect(CDrawRect::rectangle, CRect(local, local));
	} break;
	case RoundRect: {
		pObj = new CDrawRect(CDrawRect::roundRect, CRect(local, local));
	} break;
	case Ellipse: {
		 pObj = new CDrawRect(CDrawRect::ellipse, CRect(local, local));
	} break;
	case Line: {
		pObj = new CDrawRect(CDrawRect::line, CRect(local, local));
	} break;
	case Arc: {
		pObj = new CDrawRect(CDrawRect::arc, CRect(local, local));
	} break;
	case Arc2: {
		pObj = new CDrawRect(CDrawRect::arc2, CRect(local, local));
	} break;
	case Polygon: {
		pObj = new CDrawPoly(CRect(local, local));
	} break;
	case BezierCurve: {
		pObj = new CDrawPoly(CRect(local, local), /* bBezier= */ TRUE);
	}break;
	
	case DocArea: {
		pObj = new CDocArea(CRect(local, local));
	} break;



	default:   // TRACE("NewDrawObj: Bad case in Switch!");
		return NULL;
		break;
	} // end switch
	return pObj;
}

//
// Handle mouse move:
//
void CFBDView::OnMouseMove(UINT nFlags, CPoint point) 
{
	// See if tracking mouse in this view:
	if (GetCapture() != this)			// Mouse not captured: ordinary mouse move
	{
		// if there's a selection, change cursor if moved over resize handle
		if (m_drawMode == Selector && SingleSelection()) {
			CDrawObj* pObj = SelectedObj();
			CPoint local = point;
			ClientToDoc(local);
			int nHandle = pObj->HitTest(local, this, TRUE);
			if (nHandle != 0) 
				SetCursor(pObj->GetHandleCursor(nHandle));
		}
		if ((!theApp.m_bAuthorMode) && (HyperAt(point)!= NULL))
			m_bOnHyperText = TRUE;
		else
			m_bOnHyperText = FALSE;
		return;	//  nothing else to do in this case
	}

	// Else dragging: delegate to worker routine to track the mouse movement 
	// This was broken out for use during log playback without capturing mouse. 
	TrackMove(nFlags, point);
}

void CFBDView::TrackMove(UINT nFlags, CPoint point)
{
	// One message used to log event for *all* drag modes: move/resize/draw
	CPoint local = point;
	ClientToDoc(local);
	LogEventf(EV_MOUSE_MOVE, "%d %d", local.x, local.y);  // keep short due to frequency

	if (m_drawMode == Selector ||		// handle drag when selector active	
		m_drawMode == Polygon  ||		// resize code also used for Polygon adding
		m_drawMode == BezierCurve)
	{
		if (m_selectMode == netSelect)	// drag in netSelect submode
		{	
			// rubber band the selection rectangle. NB: uses device coords
			CClientDC dc(this);
			CRect rect(m_ptDown, m_ptPrev);
			rect.NormalizeRect();
			dc.DrawFocusRect(rect);
			rect.SetRect(m_ptDown, point);
			rect.NormalizeRect();
			dc.DrawFocusRect(rect);

			// update tracking state for this mode:
			m_ptPrev = point;
			return;				// can skip the rest
		}

		// else move or resize. Last can be from select tool or other tool delegating.
		if (m_selectMode == resize && m_nDragHandle != 0)	// Dragged resize handle
		{
			if (m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CAngle))){
				m_pCurrentObj->MoveHandleTo(m_nDragHandle, local, this);
				//don't want to align arc to grid
				//we want m_ptPrev and m_ptLastLocal  to lie on the arc
				if (m_nDragHandle == 1){
					m_ptLastLocal = m_pCurrentObj->m_position.TopLeft();
					m_ptPrev = m_ptLastLocal;
					DocToClient(m_ptPrev);
				}
				else if (m_nDragHandle == 3){
					m_ptLastLocal = m_pCurrentObj->m_position.BottomRight();
					m_ptPrev = m_ptLastLocal;
					DocToClient(m_ptPrev);
				}
				else{
					m_ptPrev = point;
					m_ptLastLocal = local;
				}
			} else{
				AlignToGrid(local);
				m_pCurrentObj->MoveHandleTo(m_nDragHandle, local, this);
			
				// delta is change from last event, in logical
				CPoint delta = (CPoint) (local - m_ptLastLocal);

				// update tracking state for this mode
				m_ptPrev = point;
				m_ptLastLocal = local;
			
			}
			if (m_drawMode == Selector) {
				SetCursor(SelectedObj()->GetHandleCursor(m_nDragHandle));
				return; // bypass generic select finish 
			}
		}
		else if (m_selectMode == move)		// Dragged object or objects
		{
			// delta is change from last event, in logical
			CPoint delta = (CPoint) (local - m_ptLastLocal);

			// if single selection, use alignment constraint with virtual object
			if (SingleSelection()) 
			{
				CDrawObj* pObj = SelectedObj();
				if (pObj->IsKindOf(RUNTIME_CLASS(CAngle)))
					return;
				else if ( pObj->IsKindOf(RUNTIME_CLASS(CVector)) || 
							pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
				{
					if ((abs(delta.x) > 1) || (abs(delta.y) > 1))
						GetDocument()->DeleteObjList(&pObj->m_Angles);
				}

				ASSERT(pObj != NULL);
				CRect newPosition;
				if (m_bAlignGrid) 
				{
					// We keep a virtual position internally, the position but for the grid,
					// so that the move process can stay in synch with mouse. I.e. mouse moves
					// the "ghost" pos the exact amount, and we apply the grid constraint to see if
					// the actual object position. 
					m_posVirtual +=  delta;
					newPosition = m_posVirtual;
					AlignToGrid(newPosition);
				} else
					newPosition = pObj->m_position + delta;

				// Do the move:

				pObj->MoveTo(newPosition, this);
			}
			else // loop to move each object in selection; grid alignment not working
			{
				if ((abs(delta.x) > 1) || (abs(delta.y) > 1))
					CheckMultSel();

				POSITION pos = m_Selection.GetHeadPosition();
				while (pos != NULL) 
				{
					CDrawObj* pObj = m_Selection.GetNext(pos);
					
					if (pObj->IsKindOf(RUNTIME_CLASS(CAngle)))
					{//if both sides are not selected
						if  (!( IsSelected(((CAngle*)pObj)->m_pAngSide1) &&
								IsSelected(((CAngle*)pObj)->m_pAngSide2) ) )
								continue;//do not move
					}
				
					ASSERT(pObj != NULL);
					CRect newPosition = pObj->m_position + delta;
					pObj->MoveTo(newPosition, this);
				}
			}
			// update state for this mode
			m_ptLastLocal = local;
			m_ptPrev = point;
			// keep move cursor while moving
			SetCursor(AfxGetApp()->LoadStandardCursor(IDC_SIZEALL));
			return;		// skip generic return.
		}

		// Generic end select tool: update generic tracking state. Sets arrow cursor
		m_ptPrev = point;
		SetCursor(AfxGetApp()->LoadStandardCursor(IDC_ARROW));
		return;
	}// End case select tool


	// Ignore drag for special tools:
	if (m_drawMode == System || m_drawMode == MotionBody || m_drawMode == Radius ||
		(m_drawMode == Vector && m_nZDir != ZDIR_NONE) ) 
		return;

	// Else default: handle drag for all other object drawing tools: 
	// Want to update defining object position rect. Essentially that is
	// what we do when we resize handle number 5 for rectangle objects.
	// Haven't actually set up to delegate to common code however.
	if ((point != m_ptPrev)){
		AlignToGrid(local);
		m_pCurrentObj->MoveHandleTo(5, local, this);
		// m_pCurrentObj->MoveTo(CRect(m_pCurrentObj.TopLeft(), local), this); 
		m_ptPrev = point;
		m_ptLastLocal = local;
		SetCursor(AfxGetApp()->LoadStandardCursor(IDC_CROSS));
	}
}

//
// Handle left button release
//
void CFBDView::OnLButtonUp(UINT nFlags, CPoint point) 
{
	//  see if tracking the mouse in this view
	if (m_drawMode == notDrawing || GetCapture() != this) 
		return;

	// ignore button up for multi-click modes
	if (m_drawMode == Polygon || m_drawMode == BezierCurve)
		return;

	// else in tracking mode:
	ReleaseCapture();
 
//	if (theApp.m_bTrainMode)
//		CloseTCardHelp();

	// Worker routine dispatches to mode-specific post-tracking actions: 
	DoPostTracking(nFlags, point);

	// exit global tracking mode:
	//changing draw mode in DoPostTracking
//	m_drawMode = Selector;
	m_selectMode = none;
	m_pCurrentObj = NULL;
}

// Little predicate used as filter for ObjectAt
BOOL CFBDView::IsVector(CDrawObj* pObj)
{
	return (pObj->IsKindOf(RUNTIME_CLASS(CVector)));
}

void CFBDView::DoPostTracking(UINT nFlags, CPoint point)
{
	CPoint local = point;
	ClientToDoc(local);
	LogEventf(EV_MOUSE_UP,"%d %d  %d %d", point.x, point.y, local.x, local.y);	

	// pop up dialogs to set/adjust object properties
	switch (m_drawMode)
	{
	case Vector: {	
		m_drawMode = Selector;
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CVector)));
		CVector* pVector = (CVector*) m_pCurrentObj;
		// Now we allow zero length vectors again, but display them differently */
		LogEventf(EV_OBJ_VECTOR, "%s  %d %d %d %d", pVector->m_strId, pVector->m_position.left, pVector->m_position.top, 
								   pVector->m_position.right, pVector->m_position.bottom);
		// Initialize direction field from drawing. User may adjust in property dialog.
		char dirstr[10];// This is a string for historical reasons -- Axes uses integer.
		sprintf(dirstr, "%d", pVector->GetDirection());
		pVector->m_strOrientation = dirstr;

		// Handle motion diagram vectors specially:
		if (pVector->IsKindOf(RUNTIME_CLASS(CMDVector))) {
			// !!! Need generic container or subdiagram class for following two cases
			CMotionDiagram* pRuler = FindMotionRuler(pVector->m_position.TopLeft());
			if (pRuler) {
				AddMotionVector(pRuler, (CMDVector*)pVector);
				return;
			}
			// if vector drawn inside a 2D combined motion box, handle specially
			C2DMotion * p2D = In2DArea(pVector->m_position.TopLeft());
			if (p2D) {
				Add2DVector(p2D, (CMDVector*)pVector);
				return;
			}
		}
		
		// Query user for initial properties. First ensure we know vector type:
/*		if ((m_vectorType == Unknown)					// drawtool wasn't typed
		     && (pVector->OnEditVectorType() != IDOK) ) // user cancelled type dlg
			{	// treat as cancelling object add	
				DeleteSelection();
				return;
			}*/
		// Have type, now set other props: 
		if (! theApp.m_bAuthorMode){// Let authors draw unlabelled arrows w/o props
			if (! pVector->OnEditProperties() )// Triggers object check via Check* routine 
				DeleteSelection();					// cancelled, undo draw.	
		}
	} break;

	case GuideLine: {	
		m_drawMode = Selector;
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CGuideLine)));
		CGuideLine* pLine = (CGuideLine*) m_pCurrentObj;
		// Initialize direction field from drawing. User may adjust in property dialog.
		pLine->m_strOrientation.Format("%d", pLine->GetDirection() % 180);
		if (! pLine->OnEditProperties() )// Triggers object check via Check* routine 
				DeleteSelection();					// cancelled, undo draw.
	} break;

/*	case Angle: {				
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CAngle)));
		CAngle* pAngle = (CAngle*) m_pCurrentObj;
		// Now we allow zero length vectors again, but display them differently */
/*		Logf("Angle  %d %d %d %d", pAngle->m_position.left, pAngle->m_position.top, 
								   pAngle->m_position.right, pAngle->m_position.bottom);
		if(!pAngle->OnEditProperties())
			DeleteSelection();
	} break;*/

	case Axes: {				// pop up dialog to get object info	
		m_drawMode = Selector;
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CAxes)));
		CAxes* pAxes = (CAxes*) m_pCurrentObj;
		int delta1, delta2;
		delta1 = pAxes->m_position.left - pAxes->m_position.right;
		delta2 = pAxes->m_position.top - pAxes->m_position.bottom;
		if ((abs(delta1)<=5) && (abs(delta2)<=5))//Deletes mouse clicks
		{										//0 length/width objects
			DeleteSelection();
			return;
		}
		LogEventf(EV_OBJ_AXES, "%s  %d %d %d %d", pAxes->m_strId, pAxes->m_position.left, pAxes->m_position.top, 
						         pAxes->m_position.right, pAxes->m_position.bottom);

		//  fill in initial direction from drawing
		pAxes->m_nDirection = pAxes->GetDirection();

		// Let user edit properties. This will trigger CheckObject();
		if (! pAxes->OnEditProperties())
			DeleteSelection();
	} break;

	case System: {				// pop up dialog to get object info
		m_drawMode = Selector;
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CSystem)));
		CSystem* pSystem = (CSystem*) m_pCurrentObj;
		LogEventf(EV_OBJ_SYSTEM, "%s  %d %d %d %d", pSystem->m_strId, pSystem->m_position.left, pSystem->m_position.top, 
				pSystem->m_position.right, pSystem->m_position.bottom);
		// Let user edit properties. This will trigger CheckObject();
		if (! pSystem->OnEditProperties())
			DeleteSelection();
	}break;

	case Radius: {				// pop up dialog to get object info
		m_drawMode = Selector;
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CRadius)));
		CRadius* pRadius = (CRadius*) m_pCurrentObj;
		LogEventf(EV_OBJ_RADIUS, "%s  %d %d %d %d", pRadius->m_strId, pRadius->m_position.left, pRadius->m_position.top, 
				pRadius->m_position.right, pRadius->m_position.bottom);
		// Let user edit properties. This will trigger CheckObject();
		if (! pRadius->OnEditProperties())
			DeleteSelection();
	}break;

	case MotionDiagram: {
		m_drawMode = Selector;
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CMotionDiagram)));
		CMotionDiagram* pRuler = (CMotionDiagram*) m_pCurrentObj;
		Logf("Ruler %s  %d %d %d %d", pRuler->m_strId, pRuler->m_position.left, pRuler->m_position.top, 
				pRuler->m_position.right, pRuler->m_position.bottom);
		// user finished adding a motion diagram ruler: ensure it has
		// a surrounding doc area attached to it, creating and adding one if necessary
		if (pRuler->m_pArea == NULL)
			pRuler->CreateArea();
		if (! m_pCurrentObj->OnEditProperties())
			DeleteSelection();
	}break;

	case MotionBody: {
		m_drawMode = Selector;
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CMotionBody)));
		CMotionBody* pBody = (CMotionBody*) m_pCurrentObj;

		// Check that point is in initialized motion diagram, delete it if not.
		CMotionDiagram* pDiagram = FindMotionRuler(local);
		if (pDiagram == NULL) {
			DeleteSelection();
			break;
		}
		// !!Check that position is farther in direction of those already drawn.
		// Check that there are at most 4 time point entries
		if (pDiagram->m_entries.GetCount() >= 4) {
			DeleteSelection();
			MessageBeep(MB_ICONHAND);
			// !! Give message
			break;	
		}
		// Add link in to current motion diagram. Parents it to diagram and fills 
		// in label by default. Note entry is also in document list.
		pDiagram->AddBody(pBody);
		
		// Draw new obj (nec in this case because no dialog was popped up)
		pBody->Invalidate();	
		// finished add: update other views with new object (this one is up to date)
		GetDocument()->UpdateAllViews(this,  HINT_UPDATE_DRAWOBJ, pBody);
		// Check object with help system
		pBody->CheckObject();
		pBody->Invalidate(); // in case color changed.
	}break;

	case Selector:
		//  if finished move or resize: update all other views (this one up to date) 
		if ( (m_selectMode == resize || m_selectMode == move) 
			 && point != m_ptDown )	// 
			GetDocument()->UpdateAllViews(this,  HINT_UPDATE_DRAWOBJ, m_pCurrentObj);
		
		// Check if need to notify help system of change !!! should use method in object 

		// if just finished resize of vector or axes. allow user to adjust possibly
		// changed direction with dialog. Remember that editing props triggers 
		// check of changed position. But even if editing cancelled, we still notify 
		// help system of move, since that is not undone.
		if (m_selectMode == resize && point != m_ptDown)	// Finished RESIZE
		{
			if (m_pCurrentObj == NULL) {
				TRACE("PostTracking/resize: No current object!\n");
				break;
			}
			
			if ( m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CVector)) ) // Vector::OnResize
			{		
				CVector* pVector = (CVector*) m_pCurrentObj;
				// update direction field from drawing (urgh, string-valued).
				char szDir[10]; sprintf(szDir, "%d", pVector->GetDirection());
				pVector->m_strOrientation = szDir;
				// !!! Could check if orientation actually changed from prev value

				// skip the dialog for vectors in motion diagrams, since not exact. 
				if (pVector->IsKindOf(RUNTIME_CLASS(CMDVector))) {
					pVector->NotifyChange(pVector->m_strName);	
					return;
				}
				if (! pVector->OnEditProperties())				// if OK'd, does notify
					pVector->NotifyChange(pVector->m_strName);	// must check even if cancelled dialog
			}
			else if ( m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CAxes)) ) // Axes::OnResize
			{
				CAxes* pAxes = (CAxes*) m_pCurrentObj;
				pAxes->m_nDirection = pAxes->GetDirection();
				if (! pAxes->OnEditProperties())				// if OK'd, notify
					pAxes->NotifyChange(pAxes->m_strName);		// must check even if cancelled dialog
			}
			else if ( m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CGuideLine)) )
			{
				CGuideLine* pLine = (CGuideLine*) m_pCurrentObj;
				// update direction field from drawing (urgh, string-valued).
				char szDir[10]; sprintf(szDir, "%d", pLine->GetDirection() % 180);
				pLine->m_strOrientation = szDir;
				if (! pLine->OnEditProperties())				// if OK'd, does notify
					pLine->NotifyChange(pLine->m_strName);	// changed even if cancelled
			}
			if (m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CMotionDiagram)) )//	MotionRuler::OnResize
				((CCheckedObj*)m_pCurrentObj)->NotifyChange(m_pCurrentObj->m_strName);
		} // end post-resize

		if (m_selectMode == move && point != m_ptDown)  // Finished MOVE
		{
			if (m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CMotionBody)))	// MotionBody	
				((CCheckedObj*)m_pCurrentObj)->NotifyChange(m_pCurrentObj->m_strName);
			// Vector, Axis, System: move doesn't change semantics. ignore
			// What if move motion vector? (e.g. off body) For now, ignore.
		} // end post-move
		
		// if finished net select, erase focus rect and do selection
		if (m_selectMode == netSelect) 
		{
			CClientDC dc(this);
			CRect rect(m_ptDown, m_ptPrev);	// NB: need device coords
			rect.NormalizeRect();
			dc.DrawFocusRect(rect);

			SelectWithinRect(rect, TRUE);
		}
		break;	// end case Select

	// Else finished adding new object: check OK and log object id and position
	default: 
	{
		m_drawMode = Selector;
		// Check for degenerate 0-length/width objects, e.g. if user clicked.
		CDrawObj* pObj = m_pCurrentObj;
		int delta1, delta2;
		delta1 = pObj->m_position.left - pObj->m_position.right;
		delta2 = pObj->m_position.top - pObj->m_position.bottom;
		if ((abs(delta1)<=5) && (abs(delta2)<=5))
		{										
			DeleteSelection();
			MessageBeep(MB_ICONASTERISK);
			return;
		}
		LogEventf(EV_OBJ_OTHER, "%d %s  %d %d %d %d", m_drawMode, pObj->m_strId, 
					pObj->m_position.left, pObj->m_position.top, 
					pObj->m_position.right, pObj->m_position.bottom);

		// finished add: update other views with new object (this one is up to date)
		GetDocument()->UpdateAllViews(this,  HINT_UPDATE_DRAWOBJ, pObj);
	} break;

	} // end switch 
}// end DoPostTracking

// Handle adding of vector to motion diagram
void CFBDView::AddMotionVector(CMotionDiagram* pRuler, CMDVector* pVector)
{
	// first make sure we know the type of the vector. (Now obsolete)
/*	if ( (m_vectorType == Unknown)					// drawtool wasn't typed
		&& (pVector->OnEditVectorType() != IDOK) )  // user cancelled type choice
		{
			// treat as cancelling object add.
			DeleteSelection();
			return;
		}*/

	// Following are used in AddVector call, set to initial defaults.
	CMotionDiagram* pAddDiagram = pRuler;		// diagram to add to
	CMotionBody* pBody = NULL;					// body to attach to

	// See if vector tail over a motion body.(Must ignore vector just drawn there)
	CDrawObj* pObj = GetDocument()->ObjectAt(pVector->m_position.TopLeft(),
												/* ignore if */ &IsVector);
	BOOL bOnBody = pObj != NULL && pObj->IsKindOf(RUNTIME_CLASS(CMotionBody));
	if ( bOnBody ) 
	{
		pBody = (CMotionBody*) pObj;
		// Possible conflict: body could be owned by different diagram than the
		// one found by endpoint hit-test. For now, body's diagram takes precedence.
		pAddDiagram = pBody->m_pDiagram;

		if (pVector->m_nVectorType == VECTOR_VELOCITY)
			theApp.SendTrainer(this, ID_HELP_DRAWMOTACC);
		else if (pVector->m_nVectorType == VECTOR_ACCELERATION)
			theApp.SendTrainer(this, ID_ENDMOTIONDGRM);	
	} 
	else if (pVector->m_nVectorType == VECTOR_VELOCITY) // velocity not drawn on body
	{
		// Cancel drawing with message
		DeleteSelection();
		theApp.DoWarningMessage(
"In Motion Diagrams, velocity vectors must be drawn with tail on a body");
			return;
	} 
	
	// AddVector links in vectors to diagram either with or without time point. 
	// Ambiguous accelerations (without link to time point) default to WholePeriod. 
	// We will disambiguate with dialog box below. Note we *must* link vector
	// to parent motion diagram *before* dialog so that dialog can use it.
	pAddDiagram->AddVector(pVector, pBody);

	// For ambiguous accelerations: show dialog to set properties, i.e. time point
	// Don't use OnEditProperties since that notifies help system (must wait for further check)
	if (m_vectorType == VECTOR_ACCELERATION && ! bOnBody) 
	{
		// Have object allocate and init a property dialogue and pop it up
		CDialog* pDlg = pVector->GetPropertyDlg();
		ASSERT(pDlg != NULL);
		int nResult = pDlg->DoModal();	
		delete pDlg;
		if (nResult != IDOK) {	// user cancelled dialog -- remove object
			DeleteSelection();
			return;
		}
	}
		
	// Now sure we know its time point, so can check if this is legal add
	// !!! Note object is already in diagram, we are seeing if should take it out!
	if (! pAddDiagram->CanAddVector(pVector) ){
		DeleteSelection();
		theApp.DoWarningMessage("Vectors of a given type must be added in time point order");
		return; 
	}
			
	// Check new oject with help system
	pVector->CheckObject();
	// redraw in case status changed
	pVector->Invalidate();
}

// handle drawing of vector inside a 2D motion diagram.
// Must be velocity drawn on body that is resultant of existing velocities
void CFBDView::Add2DVector(C2DMotion* p2D, CMDVector* pVector)
{
	// Make sure it's a velocity
	if (pVector->m_nVectorType != VECTOR_VELOCITY) {
		DeleteSelection();
		theApp.DoWarningMessage("Can only add velocities to combined motion diagrams");
		return;
	}

	// Make sure vector tail over a motion body.(Must ignore vector just drawn there)
	CMotionBody* pBody;					
	if (! (pBody = p2D->BodyAt(pVector->m_position.TopLeft()))) {
		DeleteSelection();
		theApp.DoWarningMessage(
"In Motion Diagrams, velocity vectors must be drawn with tail on a body");
			return;
	} 

	// Make sure both components drawn on component diagrams

	// position vec as X component on diagram
	if (p2D->m_pmdX->GetVectorAt(pBody->m_nNumber, VECTOR_VELOCITY) == NULL ||
		p2D->m_pmdY->GetVectorAt(pBody->m_nNumber, VECTOR_VELOCITY) == NULL ) {
		DeleteSelection();
		theApp.DoWarningMessage(
"Both component velocities must be drawn on 1D diagrams before resultants are drawn");
			return;
	}
		
	// Add to diagram's list
	p2D->AddVector(pVector, pBody);

	// Check new oject with help system
	pVector->CheckObject();
	// redraw in case status changed
	pVector->Invalidate(); 
}

//
// Handle double click
//
void CFBDView::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
	// point unused, so no need to convert
	if (m_drawMode == Selector) 
	{
		if (SingleSelection() // note first click of dbl should have selected obj
			&& SelectedObj()->IsKindOf(RUNTIME_CLASS(COleDrawObj)) )
			// Open it
			((COleDrawObj*) SelectedObj())->OnOpen(this);
			// !!! OLE objects in problem are not selectable by students, but if we
			// put media clips in problem, they need a way to *play* them.
		else
			OnEditProperties();	// will check if command is valid, i.e. there is selection
		return;
	} 

	if (m_drawMode == Polygon || 
		m_drawMode == BezierCurve) // end polygon adding
	{
		ReleaseCapture();
		ASSERT(m_pCurrentObj->IsKindOf(RUNTIME_CLASS(CDrawPoly)));
		CDrawPoly* pPoly= (CDrawPoly*) m_pCurrentObj;
		int nPoints = pPoly->m_nPoints;
		// Nuke the last point if it's the same as the next to last or if it is
		// the adjustable one we added one grid point away.
		if (nPoints > 2 &&
			(pPoly->m_points[nPoints - 1] == pPoly->m_points[nPoints - 2] ||
			pPoly->m_points[nPoints - 1].x - m_cxGrid == pPoly->m_points[nPoints - 2].x &&
			pPoly->m_points[nPoints - 1].y == pPoly->m_points[nPoints - 2].y))
		{
			
			pPoly->m_nPoints -= 1;
			InvalObjInView(pPoly);
		}

		m_pCurrentObj = NULL;
		m_drawMode = Selector;
		m_selectMode = none;

		// finished add: update other views with new object (this one is up to date)
		GetDocument()->UpdateAllViews(this,  HINT_UPDATE_DRAWOBJ, pPoly);
	}
}

// Tooltip support:
//
// With tooltips enabled, CWnds filter every mouse message, relaying data to MFC's 
// shared tooltip control. The filter calls following virtual function to obtain info
// about the currently hit "tool" (child window or rectangular region). 
// We override to provide data for tooltips that treat FBD objects as "tools".
// Cf. sample at http://ourworld.compuserve.com/homepages/MRConway/tooltip.htm
//
CDrawObj* CFBDView::s_pTipObj = NULL;			// static saves obj on hits for text callback

int CFBDView::OnToolHitTest( CPoint point, TOOLINFO* pTI ) const
{
	// MFC declared function const, making type of "this" ptr const within it. Have to
	// cast away constness of "this", since didn't declare all methods used as const.
	CFBDView* pThis = (CFBDView*) this;	
	
	// Find object at point, saving into static s_pTipObj. 
	CPoint local = point;					// local = point in logical coords
	pThis->ClientToDoc(local);
	s_pTipObj = pThis->GetDocument()->ObjectAt(local, /* ignore:*/ Unselectable);
	if (s_pTipObj == NULL)		// no hit
		return -1;
	
	// We must fill in TOOLINFO to return data about hit "tool" region.
	pTI->hwnd = m_hWnd;						// handle of containing wnd
	CRect rcObj = s_pTipObj->GetBoundingBox();
	pThis->DocToClient(rcObj);
	pTI->rect = rcObj;						// tool bounding rect, client coords
	//
	// Set id sent with TTN_NEEDTEXT callback, normally a cmd id or child ctl hwnd. 
	// We use a distinguished id, so can tell it's not a standard cmd in OnToolTipNotify,
	// see below. Note id must be unique among all views using custom tooltips.
	//
#	define FBDVIEW_TOOLID ((UINT) -2)		// id for FBDVIEW drawobj tool callbacks
	pTI->uId = FBDVIEW_TOOLID; 
	pTI->uFlags &= ~TTF_IDISHWND;			// id is not an hwnd, in case it matters
	pTI->uFlags |= TTF_ALWAYSTIP;			// show tips even when inactive (from sample)
	pTI->lpszText = LPSTR_TEXTCALLBACK;		// retrieve text via TTN_NEEDTEXT callback 

	// must return cookie MFC can use to determine when hit "tool" changes
	return int(s_pTipObj);					// just use object pointer value
}

// Handle TTN_NEEDTEXT notification callback from tooltip ctl to supply text for tip
// Because notifications are routed like commands, view may receive this msg
// for all tools in app, including, e.g., tools in mainframe toolbar. For that reason
// handler is mapped by a NOTIFY_EX entry, which passes msg up if not consumed.
// 
BOOL CFBDView::OnToolTipNotify( UINT id, NMHDR * pNMHDR, LRESULT * pResult )
{
	TOOLTIPTEXT *pTTT = (TOOLTIPTEXT *)pNMHDR;
	// Ensure this is a request for a CFBDView "tool" by checking that ID in request 
	// is the same as the distinguished one we supplied in OnToolHitTest
    if (pNMHDR->idFrom == FBDVIEW_TOOLID)
	{
		// get hit obj from saved static
		if (!s_pTipObj)	return FALSE;	// shouldn't happen
				
		// use print def to show object properties in tooltip
		// in author mode, append object id to tip
		if (theApp.m_bAuthorMode)
			sprintf(pTTT->szText, "%s ID=%s", s_pTipObj->GetPrintDef(), 
											  s_pTipObj->m_strId);
		else
			strcpy(pTTT->szText, s_pTipObj->GetPrintDef());	
		
		return(TRUE);
	}
	
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////
// Selection management
/////////////////////////////////////////////////////////////////////////

// 
// Select: Select given object. bAdd indicates whether adding to selection list 
// Accepts NULL pObj to clear selection.
// 
void CFBDView::Select(CDrawObj* pObj, BOOL bAdd)
{
	if (! bAdd)	 // not adding to selection list: clear current selection
	{	
		// Might want trace message if clearing non-empty selection
		// if (!m_Selection.IsEmpty()) Logf("ClearSel");
		OnUpdate(NULL, HINT_UPDATE_SELECTION , NULL);
		m_Selection.RemoveAll();
	}

	// Do no more if just called to clear selection
	if (pObj == NULL)	
		return; 

	// Make sure the object PART that was hit is the PART that is currently selected.
	// Where it matters (axes), hit part saved in object on last HitTest call (dicey).
	if (IsSelected(pObj) && (pObj->GetHit() == pObj->GetSelectedPart())) 
		return;		// no change to selection state

	pObj->SetSelectedPart(pObj->GetHit());	

	// only add if the object is not already selected (might just be change in part).
	if (! IsSelected(pObj) )
		m_Selection.AddTail(pObj);

	// object now needs to be redrawn (this view only)
	InvalObjInView(pObj);

	// Trace changes in selection to log file. (!!! part changes not logged)
	LogEventf(EV_SELECT, "%s |%s| %s",  pObj->m_strId, OBJ_NAME(pObj), bAdd ? "+" : "" );
}

// filter function for use in ignoring unselectable teacher objects
BOOL CFBDView::Unselectable (CDrawObj* pObj)
{
	if ( (pObj->m_flag == TEACHER_OBJECT) && ! theApp.m_bAuthorMode )
		return TRUE;
	return FALSE;
}

// SelectWithinRect -- does net selection of all objects intersecting rect
// rect in *device* coords
void CFBDView::SelectWithinRect(CRect rect, BOOL bAdd)
{
	if (!bAdd)
		Select(NULL);

	ClientToDoc(rect);

	CDrawObjList& ObList = GetDocument()->m_objects;
	POSITION posObj = ObList.GetHeadPosition();
	while (posObj != NULL)
	{
		CDrawObj* pObj = ObList.GetNext(posObj);
		if (!Unselectable(pObj) && pObj->Intersects(rect))
			Select(pObj, TRUE);
	}
}

//
// RemoveFromSel: worker routine to delete an object from our selection list
//
// This routine is provided as a callback to be fired on removal of object 
// from document by CFBDDoc::Remove to ensure views don't hold dangling references. 
//
// Note this does *not* invalidate any region to trigger a repaint, it just
// does the work of removing the object from our selection list. See DeleteSelection
// for the object deletion procedure we inherited from the DRAWCLI sample.
//
void CFBDView::RemoveFromSel(CDrawObj* pObj)
{
	POSITION pos = m_Selection.Find(pObj);
	if (pos != NULL)
		m_Selection.RemoveAt(pos);

	// Following needed since the deletion protocol sends no update hint 
	// just *after* obj's removal from document, other than this callback
	UpdateScrollSizes();
}

// Deselect -- Command to remove object from selection
// 
// "RemoveFromSel" is an internal worker routine that just does
// list manipulation; it is used to update selection on object deletions.
// Deselect handles an editing command that may have come from the user  
// For now, difference is that Deselect updates object region (and logs).
//
void CFBDView::Deselect(CDrawObj* pObj)
{
	POSITION pos = m_Selection.Find(pObj);
	if (pos != NULL) {
		InvalObjInView(pObj);
		m_Selection.RemoveAt(pos);
		LogEventf(EV_DESELECT, "%s |%s|", pObj->m_strId, OBJ_NAME(pObj));
	}
}

// General function to enable just in case single selection
void CFBDView::OnUpdateSingleSelect(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(SingleSelection() && m_bEnabled);
}

/////////////////////////////////////////////////////////////////////////
// Editing operations and context-menu handler
/////////////////////////////////////////////////////////////////////////

// 
// DeleteSelection -- Delete currently selected objects from document.
//
// Other FBDViews can have selection lists referencing objects in the document,
// and other views might reference the objects in other ways, Therefore we need some
// method of notifying them on object deletions so they aren't left holding
// dangling references after the object goes away.
//
// Following somewhat involved procedure comes from the DRAWCLI sample, which allowed 
// for multiple diagram views. (We only allow students one FBDView currently, but 
// we have other views that need notification, and also want to code for the general
// case.) Looks like it could be simplified, but don't want to mess with it.
//
// First, DRAWCLI sends a special update hint to all other views *before* the selected 
// objects are removed from the document. The other views can delete the listed objects 
// from their selection lists and mark the relevant regions dirty. This hint is
// only supposed to be processed by views other than the sender.
// Second, we mark the selection as dirty in our view so it will be repainted when
// we are done. 
// Finally, we run through the selection and remove each object in turn from the document. 
// 
// Furthermore, the document's Remove routine has been instrumented so that each Remove 
// call triggers a callback to the RemoveFromSel method in all attached FBDView's 
// It appears this is only effective in this view, since other views should have already 
// removed object from their selections in the hint processing.
//
void CFBDView::DeleteSelection() // does work of deleting selected objects
{
	if (m_Selection.IsEmpty()) return;

	// Notify all *other* views of list of objects that are going away.
	GetDocument()->UpdateAllViews(NULL,  HINT_DELETE_SELECTION , &m_Selection);

	// Call our method to mark selected object regions as dirty in our view:
	OnUpdate(NULL, HINT_UPDATE_SELECTION, NULL);

	// Loop to unlink each selected object from the document and delete it

	// Note: items may be removed from selection list in the course of the loop by 
	// the RemoveFromSel callback hooked to CFBDDoc::Remove. Removal of the deleted 
	// object in the course of a GetHeadPosition/GetNext loop (as in original version)
	// is acceptable, since "pos" always points beyond current item to the next item 
	// in the selection list. However, the deletion of some of our objects (those that
	// function as containers, like motion diagrams) calls destructors that trigger 
	// removal and deletion of *other* dependent objects (like MD entries). In this
	// case, the RemoveFromSel callback can delete objects further down the selection 
	// list, including the very next one referenced by pos, causing crash later.
	// For that reason, we now carefully remove object from selection list *before* 
	// deleting it. (now RemoveFromSel callback not relied on in our view
	while (!m_Selection.IsEmpty())
	{
		// remove first object from our selection list
		CDrawObj* pObj = m_Selection.RemoveHead();

		// Unlink obj from document's list
		// Triggers RemoveFromSel callback to all FBDViews to update selection lists
		GetDocument()->Remove(pObj);

		// trace msg in log tracks individual deletions
		LogEventf(EV_DELETED, "%s |%s|", pObj->m_strId, OBJ_NAME(pObj)); 

		// for checked objects,update help system:  
		if (pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)))
			((CCheckedObj*)pObj)->NotifyDelete();
	
		// Destroy object and free all its storage. For contained objects as in 
		// MotionDiagrams, this or destructor will notify containing parent. Deletion 
		// of container objects may automatically remove/delete dependent contained objects. 
		// Note no helpsys notification for automatically deleted dependents.
		pObj->Delete();	
	}

#if 0	// OLD CODE
	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = m_Selection.GetNext(pos);

		// Unlink obj from document list
		// This will trigger RemoveFromSel in all FBDViews to update selection lists
		GetDocument()->Remove(pObj);

		// trace msg in log tracks individual deletions
		LogEventf(EV_DELETED, "%s |%s|", pObj->m_strId, OBJ_NAME(pObj)); 

		// for checked objects,update help system:  
		if (pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)))
			((CCheckedObj*)pObj)->NotifyDelete();
	
		// Destroy object and free all its storage. For objects contained in MotionDiagrams,
		// this or destructor will update parent.
		pObj->Delete();	
	}

	// clear selection list in this view 
	// This looks redundant given the RemoveFromSel callback; I assume this is 
	// defensive programming just to make sure -- AW.
	m_Selection.RemoveAll();
#endif // OLD CODE
}

void CFBDView::OnEditDelete()	// user command, separate for logging
{
	// If focus is in a child edit, just delegate command to the control
	// The change will be logged, so no need to log user command.
	if (IsActiveEdit()) {
		GetActiveEdit()->Clear();
		return;
	}
	// else apply to current selection:
	LogEventf(EV_DELETE, "");				// log user command
	DeleteSelection();
}

void CFBDView::OnUpdateEditDelete(CCmdUI* pCmdUI) 
{
	// Can only delete motion diagram entries in certain order.
	// Here check that all selected objects are currently deletable
	// !!! should be moved into method in MotionDiagram or use	
	// !!!  CanDelete method on objects
	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = m_Selection.GetNext(pos);
		if ( pObj->IsKindOf(RUNTIME_CLASS(CMotionBody)) 
			&& ((CMotionBody*)pObj)->m_pDiagram) // hit body contained in MotionDiagram
		{ 
			CMotionBody* pBody = (CMotionBody*) pObj;
			// deletable iff body number == count of entries  (last body added) 
			if (pBody->m_nNumber != pBody->m_pDiagram->m_entries.GetCount()) {
				pCmdUI->Enable(FALSE);
				return;
			}
		}
		if (pObj->IsKindOf(RUNTIME_CLASS(CMDVector)) 
			&& ((CMDVector*)pObj)->m_pDiagram)	// hit vector contained in 1D MotionDiagram
		{
			CMDVector* pVec = (CMDVector*) pObj;
			if (! pVec->m_pDiagram->CanDelete(pVec) ) {
				pCmdUI->Enable(FALSE);
				return;
			}
		}
	}
	// else just enable if have selection.
	OnUpdateEditCopyCut(pCmdUI);
}

//
// Clipboard command handlers.
//
void CFBDView::OnUpdateEditCopyCut(CCmdUI* pCmdUI)
{
	if (IsActiveEdit()) {	// see if edit control has selection
		int nStart, nEnd;
		// CRichEditCtrl uses long& parms in GetSel method. But should understand CEdit msg
		// sent by CEdit::GetSel (an inline wrapper). so use that for both.
		((CEdit*)GetActiveEdit())->GetSel(nStart, nEnd);
		pCmdUI->Enable(nStart != nEnd && m_bEnabled);
	}
	else
		pCmdUI->Enable(HaveSelection() && m_bEnabled);
}

// Stream selection into a global memory block as text for clipboard export.
// Writes each text object as a line adding CRLF; ignores non-text objects.
// Returns global memory handle, NULL if failed or no text objects available.
HGLOBAL CFBDView::GetTextData()
{
	BOOL bWroteText = FALSE;	// TRUE if found any text to write
	
	// Use a global memory CFile to stream data as text file
	CSharedFile memFile;
	// do for each object on selection list:
	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj  = m_Selection.GetNext(pos);
		// If this is a text object: (!!! might just test if m_strName is non-empty)
		if ( pObj->IsKindOf (RUNTIME_CLASS(CLabel)) )
		{
			CString strLine = pObj->m_strName + "\r\n";
			// !!! must catch exception for write failure
			memFile.Write(strLine, strLine.GetLength());
			bWroteText = TRUE;
		}
	}
	
	if (bWroteText)
		return memFile.Detach();
	else
		return NULL;	// memFile destructor should free all resources
}

// Get picture of diagram as a Windows metafile
HENHMETAFILE CFBDView::GetMetaFileData()
{
	// use client dc as reference dev and "attribute" DC (for text extent queries, eg.)
	CClientDC dcClient(this);
	
/* more reliable to let Windows derive bounds from extent of GDI drawing operations
	// Calculate size of drawing in HIMETRIC (NB total drawing includes non-selected).
	CSize sizeDrawing = GetDocument()->GetSize();
	OnPrepareDC(&dcClient);	// set mapping mode for dc extent conversion func
	dcClient.LPtoHIMETRIC(&sizeDrawing);
	CRect rectDrawing(CPoint(0, 0), sizeDrawing); 
*/
	// create a metafile DC to draw on
	CMetaFileDC dcMeta;
	dcMeta.CreateEnhanced(&dcClient, NULL, /*rectDrawing*/ NULL, "Andes\0Diagram\0\0");
	dcMeta.SetAttribDC(dcClient.m_hAttribDC);		// doesn't attach to DC object

	// !!! Currently, drawn using MM_TEXT map mode, which is only OK because LUs 
	// usually match device resolution (96 dpi) on our systems. But using
	// OnPrepareDC led to problems. 

#if 0 
	// draw each selected obj on the metafile dc
	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj  = m_Selection.GetNext(pos);
		pObj->Draw(&dcMeta);
		// just ignore select state when drawing to metafile
	}
#else // always render whole diagram into metafile, regardless of selection
	OnDraw(&dcMeta);
#endif 

	// close and return it
	dcMeta.ReleaseAttribDC();
	return dcMeta.CloseEnhanced();
}

void CFBDView::OnEditCopy()
{
	// If focus is in a child edit, just delegate command to the control
	if (IsActiveEdit()) {
		GetActiveEdit()->Copy();
		return;
	}

	// else copy selection to clipboard in our export formats
	// Logf("Copy");			// log user command
	
	// Wrap an archive around a global-memory CFile.
	CSharedFile	memFile;
	CArchive ar(&memFile, CArchive::store|CArchive::bNoFlushOnDelete);
	ar.m_pDocument = GetDocument();	// set back ptr in archive

	// serialize selection to memory file
	m_Selection.Serialize(ar);
	ar.Flush();

	// Build OLE data object with our formats and set to the clipboard
	COleDataSource* pDataSource = NULL;
	TRY
	{
		pDataSource = new COleDataSource;

		// put data in our custom format (in a global mem block)
		HGLOBAL hData = memFile.Detach();
		pDataSource->CacheGlobalData(m_nClipboardFormat, hData);

		// see if can export text only (CRLF separated lines)
		HGLOBAL hTextData = GetTextData();
		if (hTextData != NULL)
			pDataSource->CacheGlobalData(CF_TEXT, hTextData);

#ifdef _DEBUG	// risky code for exporting diagram as metafile
		// see if can export drawing as metafile, to copy picture
		// !!! This seems to cause a crash when data object is released
		// during next clipboard operation by any program.
		HENHMETAFILE hMetaFile = GetMetaFileData();
		if (hMetaFile != NULL) {
			STGMEDIUM stg;		// tmp to pass hMetaFile to pDataSource
			stg.tymed = TYMED_ENHMF;
			stg.hEnhMetaFile = hMetaFile;
			pDataSource->CacheData(CF_ENHMETAFILE, &stg);
		}
#endif 
		// if selected one OLE item, have it export its native data as well.
		if (SingleSelection() && 
			SelectedObj()->IsKindOf(RUNTIME_CLASS(COleDrawObj)) ) 
		{
			COleDrawObj* pDrawObj = (COleDrawObj*)SelectedObj();
			pDrawObj->m_pClientItem->GetClipboardData(pDataSource, FALSE);
		}

		pDataSource->SetClipboard();
	}
	CATCH_ALL(e) {
		delete pDataSource;
		THROW_LAST();
	}
	END_CATCH_ALL
}

void CFBDView::OnEditCut()
{
	// Implement as macro-op composed of copy and delete:
	// Logf("Cut");	// component ops will log. Note this means our logs lose 
	// distinction between user choosing "Cut" vs. Copy + Delete, but that seems OK.
	OnEditCopy();
	OnEditDelete();
}

//
// Paste command -- handles pasting in our custom format or 
//                  pasting of OLE object to embed (authors only)
//
void CFBDView::OnUpdateEditPaste(CCmdUI* pCmdUI)
{
	// If focus is in a child edit, let it enable command.
	if (IsActiveEdit()) {
		pCmdUI->Enable(::IsClipboardFormatAvailable(CF_TEXT) && m_bEnabled);
		return;
	}
	// else see if can paste into current document

	// determine if private or standard OLE formats are on the clipboard
	COleDataObject dataObject;
	BOOL bEnable = dataObject.AttachClipboard() &&
		( dataObject.IsDataAvailable(m_nClipboardFormat) ||
		 (theApp.m_bAuthorMode && COleClientItem::CanCreateFromData(&dataObject)) );

	// enable command based on availability
	pCmdUI->Enable(bEnable && m_bEnabled);
}

void CFBDView::OnEditPaste()
{
	// If focus is in a child edit, just delegate command to the control
	if (IsActiveEdit()) {
		GetActiveEdit()->Paste();
		return;
	}

	// Fetch OLE data object on clipboard.
	COleDataObject dataObject;
	dataObject.AttachClipboard();

	if (dataObject.IsDataAvailable(m_nClipboardFormat)) // Paste our drawing objects
	{
		PasteNative(dataObject);
	}
	else // Paste an OLE embedded object
	{
		// clear current selection (invalidates it in view)
		Select(NULL);
		// Do the paste: adds to document at position and makes it selected
		PasteEmbedded(dataObject, GetInitialPosition().TopLeft() );
		// invalidate new pasted stuff now in selection
		GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_SELECTION, &m_Selection);
	}
}

//
// Paste a set of drawing objects in our own native format
//
// Semantics of pasting may be a little unclear. It creates a set of new
// objects with new ids; however the new object properties (including
// teacher/student mode flag) are the same as the objects on the clipboard,
// which holds an object file. Unclear if students have much need for it,
// it is functionally the same as duplicate, perhaps with a more familiar interface.
// Could also extend to cut and paste objects between panes, e.g. equations.
//
void CFBDView::PasteNative(COleDataObject& dataObject)
{
	// Get file referring to clipboard data
	CFile* pFile = dataObject.GetFileData(m_nClipboardFormat);
	if (pFile == NULL)
		return;

	// wrap an archive around the file
	CArchive ar(pFile, CArchive::load);
	ar.m_pDocument = GetDocument();	// set back ptr in archive

	// Transfer objects from file into selection
	// Clear current selection
	Select(NULL);

	// Serialize objects from clipboard into selection list
	m_Selection.Serialize(ar);

	// Now done with file and its archive wrapper
	ar.Close();
	delete pFile;

	// Loop to add each new object into document
	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pNewObj = m_Selection.GetNext(pos);
		AlignToGrid(pNewObj->m_position);
		GetDocument()->Add(pNewObj);
		
		// Notify help system about new object and its id.
		if ( pNewObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)) )
			((CCheckedObj*) pNewObj)->CheckObject();
	
	}
	// Tell all views to update for new objects (no hint!)
	GetDocument()->UpdateAllViews(NULL);
}

// Edit Undo: not implemented by us, but can forward to active edit control
void CFBDView::OnUpdateEditUndo(CCmdUI* pCmdUI) 
{
	if (IsActiveEdit())
		pCmdUI->Enable(GetActiveEdit()->CanUndo() && m_bEnabled);
	else
		pCmdUI->Enable(FALSE);
}

void CFBDView::OnEditUndo()
{
	// If focus is in a child edit, just delegate command to the control
	if (IsActiveEdit())
		GetActiveEdit()->Undo();
}

//
// Duplicating current selected object
//
void CFBDView::OnEditDuplicate() 
{
	if ( m_Selection.IsEmpty()) return;
	
	// loop to create and add clone of each obj in Selection, saving into CloneList
	CDrawObjList CloneList;	
	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = m_Selection.GetNext(pos);

		CDrawObj* pClone =  GetDocument()->AddClone(pObj);	// adds to document, marks dirty & invals
		if (pClone == NULL) 
			continue;

		AlignToGrid(pClone->m_position);
		CloneList.AddTail(pClone);

		// Log the operation and the new object
		LogEventf(EV_DUPLICATE, "%s |%s|", pObj->m_strId, OBJ_NAME(pObj) );
		LogEventf(EV_OBJECT, "%s  %d %d %d %d", pClone->m_strId, pClone->m_position.left,
			  pClone->m_position.top,	pClone->m_position.bottom, pClone->m_position.top);
		
		// If needed, notify help system about new object and its id.
		if (pClone->IsKindOf(RUNTIME_CLASS(CCheckedObj)) )
			((CCheckedObj*) pClone)->CheckObject();
	}

	// Now clear selection and send in the clones
	if (! CloneList.IsEmpty()) 
	{
		Select(NULL);
		POSITION pos = CloneList.GetHeadPosition();
		while (pos != NULL) {
			CDrawObj* pClone = CloneList.GetNext(pos);
			Select(pClone, TRUE);		
		}
	}
}

void CFBDView::OnUpdateEditDuplicate(CCmdUI* pCmdUI) 
{
	if (!HaveSelection()) {
		pCmdUI->Enable(FALSE);
		return;
	}
	// we enable if at least one object in the current selection supports duplication.
	// the duplication will do nothing on other objects. 
	// !!! a little strange, could require that all objects in selection be clonable
	BOOL HaveClonable = FALSE;
	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL && ! HaveClonable) {
		CDrawObj* pObj = m_Selection.GetNext(pos);
		if (pObj->CanDuplicate()) 
			HaveClonable = TRUE;
	}

	pCmdUI->Enable(HaveClonable && m_bEnabled); 
}

// 
// Commmands for changing order of objects. (Work should be done in document?)
//
void CFBDView::OnObjectMoveBack()
{
	CFBDDoc* pDoc = GetDocument();
	CDrawObj* pObj = m_Selection.GetHead();

	CDrawObjList* pObjects = pDoc->GetObjects();
	POSITION pos = pObjects->Find(pObj);
	ASSERT(pos != NULL);
	if (pos != pObjects->GetHeadPosition())
	{
		POSITION posPrev = pos;
		pObjects->GetPrev(posPrev);
		pObjects->RemoveAt(pos);
		pObjects->InsertBefore(posPrev, pObj);

		pObj->Invalidate();
		pDoc->SetModifiedFlag();
	}
}

void CFBDView::OnObjectMoveForward()
{
	CFBDDoc* pDoc = GetDocument();
	CDrawObj* pObj = m_Selection.GetHead();
	CDrawObjList* pObjects = pDoc->GetObjects();
	POSITION pos = pObjects->Find(pObj);
	ASSERT(pos != NULL);
	if (pos != pObjects->GetTailPosition())
	{
		POSITION posNext = pos;
		pObjects->GetNext(posNext);
		pObjects->RemoveAt(pos);
		pObjects->InsertAfter(posNext, pObj);

		pObj->Invalidate();
		pDoc->SetModifiedFlag();
	}
}

void CFBDView::OnObjectMoveToBack()
{
	if (!SingleSelection() )
		return;
	GetDocument()->MoveToBack(SelectedObj());
}

void CFBDView::OnObjectMoveToFront()
{
	if (!SingleSelection() ) 
		return;
	GetDocument()->MoveToFront(SelectedObj());
}

//
// Grouping commands:
//
void CFBDView::OnGroup() 
{
	if (m_Selection.GetCount() < 2) return;
	CFBDDoc* pDoc = GetDocument();

	// Allocate new group object
	CGroup* pGroup = new CGroup();

	// Need to preserve back to front order of objects in document when grouping. 
	// This can be different from order in selection list (=order in which selected).
	// So we run through all objects in document in back to front order and check if 
	// they are in the current selection. For each we find, we unlink it from document
	// list and append it to object list hanging off the new group. 
	POSITION pos = pDoc->m_objects.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = pDoc->m_objects.GetNext(pos);
		if (IsSelected(pObj) )
		{
			// At a selected object: unlink from document
			// Will trigger callback to RemoveFromSel to update selection
			pDoc->Remove(pObj);
		
			// And append to group
			pGroup->AddObj(pObj);
		}
	}

	// add the new group object to document selected.
	pDoc->Add(pGroup);
	Select(pGroup);
}

void CFBDView::OnUpdateGroup(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_Selection.GetCount() >= 2 && m_bEnabled);
}

void CFBDView::OnUngroup() 
{
	if (! SingleSelection()) return;
	CDrawObj* pObj = SelectedObj();
	if (! pObj->IsKindOf(RUNTIME_CLASS(CGroup)) ) return;
	CGroup* pGroup = (CGroup*) pObj;

	// Clear selection 
	Select(NULL);

	// unlink group object from document
	CFBDDoc* pDoc = GetDocument();
	pDoc->Remove(pGroup);		

	// loop to remove member objects from group list and insert into document
	while (! pGroup->m_objects.IsEmpty()) 
	{
		CDrawObj* pObj = pGroup->m_objects.RemoveHead();
		pDoc->Add(pObj, FALSE);	// object already has doc-unique id
		// Add each object to selection 
		Select(pObj, TRUE);
	}

	// Free up group now empty of members.
	pGroup->Delete();
}

void CFBDView::OnUpdateUngroup(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(SingleSelection() && 
		           SelectedObj()->IsKindOf(RUNTIME_CLASS(CGroup)) && m_bEnabled);
	
}

// Toggle group: one command groups or ungroups depending on selection
void CFBDView::OnTogglegroup() 
{
	if ( SingleSelection() && SelectedObj()->IsKindOf(RUNTIME_CLASS(CGroup)) ){
		OnUngroup();
	} 
	else  if (m_Selection.GetCount() >= 2)
		OnGroup();
}

void CFBDView::OnUpdateTogglegroup(CCmdUI* pCmdUI) 
{
	if ( SingleSelection() && SelectedObj()->IsKindOf(RUNTIME_CLASS(CGroup)) ){
		pCmdUI->SetText("Ungroup");
		pCmdUI->Enable(m_bEnabled);
	} 
	else  {
		pCmdUI->SetText("Group\tCtrl+G");
		pCmdUI->Enable(m_Selection.GetCount() >= 2 && m_bEnabled);
	}
}

//////////////////////////////////////////////////////////////////////////////////////////
// Commands for Editing object properties via dialogs
//////////////////////////////////////////////////////////////////////////////////////////
void CFBDView::OnEditProperties() 
{

	if (m_Selection.IsEmpty() && ((CFBDApp*)AfxGetApp())->m_bAuthorMode ){
		// Author can edit document properties if no selection
		GetDocument()->OnEditProperties();
	}
	else if (SingleSelection()) {
		LogEventf(EV_EDIT_PROPS,"%s", SelectedObj()->m_strId);
		if (! SelectedObj()->OnEditProperties() && SelectedObj()->IsKindOf(RUNTIME_CLASS(CCheckedObj)) )
		{//need to checkobject on cancel cause help system thinks object is whatever its last InDialog check was
			CCheckedObj* pObj = (CCheckedObj*)SelectedObj();
			pObj->CheckObject();
			GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_DRAWOBJ, pObj);
		}
	}
}

void CFBDView::OnUpdateEditProperties(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( // need editable selection or authormode (for doc props)
		((SingleSelection()  &&  SelectedObj()->CanEditProperties())
		  || (m_Selection.IsEmpty() && ((CFBDApp*)AfxGetApp())->m_bAuthorMode))
		&& m_bEnabled
		);
}

// Special command for authors so they can set special properties, such as ID
// Should be changed to add sheet to a property page, but currently our property
// dialogs are not pages.
void CFBDView::OnUpdateAuthorprops(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(SingleSelection() && theApp.m_bAuthorMode);
}

void CFBDView::OnAuthorprops() 
{
	CDrawObj* pObj = SelectedObj();
	CPropertySheet sheet;
	CAuthorDlg dlg(pObj);
	sheet.AddPage(&dlg);

	if (sheet.DoModal() == IDOK) {
		/* nothing to do, dialog transfers props */
	}
}

///////////////////////////////////////////////////////////////////////////////////////
// Context menu command: show menu giving most common object commands:
///////////////////////////////////////////////////////////////////////////////////////

// The WM_CONTEXTMENU msg is generated by Windows from WM_RIGHTBUTTONUP if that message 
// is not handled. The Component Gallery wizard also generated a PreTranslateMsg
// hook for the View to interpret the "standard" keyboard alias Shift-F10 as well as 
// the ContextMenu key on certain special Windows95 keyboards.
//
// Like other graphic editors such as PowerPoint, we here treat the Right-button click 
// as selecting the object to which the context menu applies. !!! That could better be
// done when the mouse goes *down*. That would also allow us to handle case when 
// WM_CONTEXTMENU is generated from keyboard with an arbitary point in upper left, which
// should apply to selection if any.
// 
void CFBDView::OnContextMenu(CWnd*, CPoint point)
{
	CPoint local = point;	// point for this message comes in screen coords
	ScreenToClient(&local);
	ClientToDoc(local);

	// Make sure click selected an object, ignoring Unselectable problem objects
	// (Would also like to "activate" (set focus) an Answer Box, which is unselectable since
	// doc object is Problem obj. Now handled in control by grabbing focus on RClick. 
	CDrawObj* pObj;
	if ((pObj = GetDocument()->ObjectAt(local, /* ignore:*/ Unselectable)) != NULL)
	{
		if ( ( IsSelected(pObj) && (pObj->GetHit() != pObj->GetSelectedPart()) ) ||
				!IsSelected(pObj) )//if we are not clicking on the selected object or selected part
			Select(pObj, FALSE);//try to select the one we are over
		// didn't log right-click selection change here -- will be replayed by FBD_MENU event

		// following should now be unnecessary given Unselectable filter above -- AW
		if ( !IsSelected(pObj))//make sure an object is selected
			return;		        //i.e. not over an unselectable teacher object
		
		LogEventf(EV_FBD_MENU, "%d %d  %d %d", point.x, point.y, local.x, local.y);

		CMenu menu;
		VERIFY(menu.LoadMenu(IDR_POPUP_FBDVIEW));
		CMenu* pPopup = menu.GetSubMenu(0);
		ASSERT(pPopup != NULL);

		// Modify menu to reflect object-specific commands
		if (theApp.m_bAuthorMode) // add author specific commands
		{
			pPopup->AppendMenu(MF_STRING, ID_AUTHORPROPS, "Author Properties");
		}

		CWnd* pWndPopupOwner = this;
		while (pWndPopupOwner->GetStyle() & WS_CHILD)
			pWndPopupOwner = pWndPopupOwner->GetParent();

		pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
			pWndPopupOwner);
	}
	else // we are not clicking on an object
	{
		Select(NULL);//we deselect any selected object
		return;	
	}
}

BOOL CFBDView::PreTranslateMessage(MSG* pMsg)
{
/* 	
	// Allow dialog keyboard interface to tab among/click our own child controls 
	if (IsDialogMessage(pMsg))
		return TRUE;
   No good -- if we use it, our keyboard accelerators don't work. 
   The MFC main loop applies PreTranslateMsg "hooks" starting with current child
   window, i.e. the view and working up to parent until one returns true. This is how
   keyboard accelerator translation is enabled.
   But IsDialogMsg here winds up forwarding all keypresses to the focus control 
   (and buttons get the kbd focus to handle space-bar clicks!). This pre-empts the 
   translation of our accelerators which is normally done in the mainframe's 
   PreTranslateMessage hook. Note that this translation doesn't happen in in our base 
   class, so we can't just call the base class method first. 
*/
#if 0
	// Following code is from MFC CFormView::PreTranslateMessage. It gives each 
	// parent frame a chance to process accelerators before calling IsDialogMessage.
	// (CWnd:PreTranslateInput calls IsDialogMessage for Mouse and Keyboard events only.)

	// Still seems not to work perfectly: Tab does not advance from richedit 
	// answer boxes, unclear why. (It works in EQView -- perhaps need to handle DLGCODE msgs?)
	// Also standard interface means moving with arrows among radio buttons within a 
	// choice group causes them to be clicked, but they seem to lose focus during the
	// DDE processing (no problem w/o helpsys).

   // allow tooltip messages to be filtered
	if (CView::PreTranslateMessage(pMsg))
		return TRUE;

	// don't translate dialog messages when in Shift+F1 help mode
	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	if (pFrameWnd != NULL && pFrameWnd->m_bHelpMode)
		return FALSE;

	// since 'IsDialogMessage' will eat frame window accelerators,
	//   we call all frame windows' PreTranslateMessage first
	pFrameWnd = GetParentFrame();   // start with first parent frame
	while (pFrameWnd != NULL)
	{
		// allow owner & frames to translate before IsDialogMessage does
		if (pFrameWnd->PreTranslateMessage(pMsg))
			return TRUE;

		// try parent frames until there are no parent frames
		pFrameWnd = pFrameWnd->GetParentFrame();
	}

	// filter both messages to dialog and from children
	/* return PreTranslateInput(pMsg); */
	if (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST) {
		BOOL bFiltered = ::IsDialogMessage(m_hWnd, pMsg);
		TRACE("FBDView key msg %x wParam=%d: IsDialogMsg=%d\n", pMsg->message, pMsg->wParam, bFiltered);
		if (bFiltered)
			return TRUE;
	}		
#endif 
	// CG: This block was added by the Pop-up Menu component
	{
		// Shift+F10: show pop-up menu.
		if ((((pMsg->message == WM_KEYDOWN || pMsg->message == WM_SYSKEYDOWN) && // If we hit a key and
			(pMsg->wParam == VK_F10) && (GetKeyState(VK_SHIFT) & ~1)) != 0) ||	// it's Shift+F10 OR
			(pMsg->message == WM_CONTEXTMENU))									// Natural keyboard key
		{
			CRect rect;
			GetClientRect(rect);
			ClientToScreen(rect);

			CPoint point = rect.TopLeft();
			point.Offset(5, 5);
			OnContextMenu(NULL, point);

			return TRUE;
		}
	}
	

	return CBaseView::PreTranslateMessage(pMsg);
}

//////////////////////////////////////////////////////////////////////////////////////////
// Special commands for vectors:
//////////////////////////////////////////////////////////////////////////////////////////
void CFBDView::OnDecomposeVector() 
{
	ASSERT(SingleSelection() && 
		   SelectedObj()->IsKindOf(RUNTIME_CLASS(CVector))); 
	CVector* pVector = (CVector*) SelectedObj();

	// Command is toggles display of mark 
	pVector->m_bDecomposed = ! pVector->m_bDecomposed;
	pVector->Invalidate();

	LogEventf(EV_MARK_VECTOR, "%s |%s| %d", pVector->m_strId, OBJ_NAME(pVector), 
											pVector->m_bDecomposed);
	
	// !!! verify that all components have been drawn?
}

void CFBDView::OnUpdateDecomposeVector(CCmdUI* pCmdUI) 
{
	// Enabled for non-component vectors
	if (SingleSelection() && SelectedObj()->IsKindOf(RUNTIME_CLASS(CVector)) ) 
	{
		CVector* pVec = (CVector*) SelectedObj();
		pCmdUI->Enable(pVec->m_nVectorType != VECTOR_COMPONENT
			           && !pVec->IsKindOf(RUNTIME_CLASS(CMDVector)) // not for MotionVectors
					   && m_bEnabled);
		// Set text to indicate toggle
		pCmdUI->SetText(pVec->m_bDecomposed ? "Mark unresolved" : "Mark resolved");
	}
	else
		pCmdUI->Enable(FALSE);
}


/////////////////////////////////////////////////////////////////////////////////////////
// Commands for getting help
/////////////////////////////////////////////////////////////////////////////////////////
void CFBDView::OnHelpWhatswrong() 
{
	// Apply to either selection or active Answer edit control
	CDrawObj* pObj = NULL;
	if (IsActiveEdit()) 
		pObj = GetDocument()->Lookup(GetActiveEdit()->m_strId);
	else if (SingleSelection())
		pObj = SelectedObj();
	ASSERT(pObj != NULL);

	LogEventf(EV_FBD_WHATSWRONG, "%s", pObj->m_strId);

	LPCTSTR pszResult;
	// silly special case -- workbench checks resultants drawn on 2D diagrams
	if ( pObj->IsKindOf(RUNTIME_CLASS(CMDVector)) && ((CMDVector*)pObj)->m_p2DDiagram ) {
		pszResult = "\"The resultant as drawn is not equal to the vector sum of the components\"";
	} else {
		 pszResult = HelpSystemExecf("(Why-wrong-object |%s| %s) ", 
									STR2ARG(pObj->m_strName), STR2ARG(pObj->m_strId) );
	}	

	// Display result in hint dialog, which knows how to parse it.
	// Ask frame to show result in hint window
	theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong);
}

void CFBDView::OnUpdateHelpWhatswrong(CCmdUI* pCmdUI) 
{
	// Enable if either: selection is in error, or focussed on AnswerBox w/statusError
	CDrawObj* pObj = NULL;
	if (IsActiveEdit()) 
		pObj = GetDocument()->Lookup(GetActiveEdit()->m_strId);
	else if (SingleSelection())
		pObj = SelectedObj();

	pCmdUI->Enable((theApp.m_wHelpFlags & fWhatsWrong) // make sure enabled
		           && pObj && pObj->m_status == statusError && m_bEnabled);
}

/*
void CFBDView::OnPhysReviewEqn() 
{
	AfxGetApp()->WinHelp(ID_PHYS_REVIEW_EQN, HELP_CONTEXT);
}

void CFBDView::OnUpdatePhysReviewEqn(CCmdUI* pCmdUI) 
{
	// TODO: Add your command update UI handler code here
	
}
*/
///////////////////////////////////////////////////////////////////////////////////////////
//
// Dispatcher for replaying events from a log file 
//
///////////////////////////////////////////////////////////////////////////////////////////

// We don't want to capture the mouse when replaying from a log since viewer can
// move mouse and mess up drawing. We rather want to create the same semantic effects on
// the workbench state programmatically. !!! Probably need to disable mouse input while
// replaying.
//
// Ideally we would consistently factor all the UI event-handling code above into an
// upper layer ("controller") which responds to input events and figures out what document
// operators to apply, plus a lower layer of operator routines to actually execute the
// operation. Then we could use the same operator routines either from a log file or 
// in response to user input. To some extent our command handlers are support that. 
// The complexity comes from the fact that several of our commands initiate complex 
// sequences of interactive operations, e.g. the process of adding a new vector, say,
// involves: selecting a drawing tool, starting the draw mode (builds the new object), 
// dragging the object to the desired size, finishing the draw, going into a
// dialog mode to edit its properties and then accepting the new object. The only operator
// on the document state is to add a new object with certain properties, but we want to
// be able to replay the details -- esp the move into the dialog -- as well.
//
// Log entries include trace information that is redundant given the main operators. 
// It could be used as a consistency check but is not currently processed.
//
BOOL CFBDView::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{
	LPCTSTR pszRest = pszArgs;	// remainder of arg string when parsing
	char szObjID[80];			// buffer to scan object id
	char szArg[255];			// buffer to scan arbitrary argument
	CDrawObj* pObj = NULL;
	int lx, ly, nHandle;		// lx, ly = pt coords in logical coords
	CPoint ptClient;			// = point in playback client area coords
	CPoint local;
	drawMode tool;				// drawtool code 
	int subtype;
	int nCtlID;					// integer child control id
	CEQEditType* pEdit;			// Answer edit box
	CWnd* pCtl = NULL;			// child control
	BOOL bFlag;
	int nStatus;
		
	switch(nEvent)
	{
	case EV_SELECT_TOOL:
		if (sscanf(pszArgs, "%d", &tool) != 1)
			return FALSE;
		// Select-tool. Wasn't originally logged, tool choice was deferred
		// till begin-draw or begin-vector event. But now we have it -- main
		// effect is to show toolbar button press before drawing begins.
		// Following handles old logs which used different integer id range
		if (tool >= ID_OLD_DRAWVECTOR_FIRST && tool <= ID_OLD_DRAWVECTOR_LAST) {
			m_drawMode = Vector;
			m_vectorType = (tool-ID_OLD_DRAWVECTOR_FIRST); 
		}
	    if (tool >= ID_DRAWVECTOR_FIRST && tool <= ID_DRAWVECTOR_LAST) {
			m_drawMode = Vector;
			m_vectorType = (tool-ID_DRAWVECTOR_FIRST); 
		} else
			m_drawMode = tool;	// should check its within drawtool range!
		break;

	case EV_LBUTTON_DOWN:// Pressed left mouse button: init tracking state vars
		// LogEventf(nEvent, "%d %d  %d %d", point, x, point.y, local.x, local.y); 
		if (sscanf(pszArgs, "%*d %*d %d %d", &lx, &ly) != 2) return FALSE;
		// fetch logical pt down and begin a tracking mode:
		m_ptLastLocal = CPoint(lx, ly);
		// NB: Device coords may be different on playback!
		m_ptDown = m_ptLastLocal; 
		DocToClient(m_ptDown); 
		m_ptPrev = m_ptDown;
		// !! might try to scroll into view.
	
		// No more to do, because what to do depends on current drawing tool. 
		// We  wait for next high-level event to see what it is:
		// either draw-obj/resize/move/click-bg. We recreate effect here rather
		// then call OnLButtonDown so as not to capture the mouse
		//	
		break;
	
	case EV_BEGIN_DRAW:	// begin drawing object (drawing tool)
		// "Draw-obj %d %s %d %d %d %d", m_drawMode, pObj->m_strId, point.x,  point.y);
		if (sscanf(pszArgs, "%d %s %d %d  %d %d", &tool, szObjID, &lx, &ly) != 4)
			return FALSE;
		m_drawMode = tool;
		pObj = NewDrawObj(m_drawMode, CPoint(lx, ly));
		if (pObj != NULL) {
			GetDocument()->Add(pObj);
			// Should get same ID on playback, if deterministic. But id counters
			// might be changed invisibly in problem file if edited (add and delete, eg.).
			// Could log id counters; or create mapping; but for now try to force log id.
			if (pObj->m_strId != szObjID) {
				if (GetDocument()->Lookup(szObjID) == NULL) // log id is unused
					pObj->m_strId = szObjID; // so try to proceed this way (risky?)
				else {
					TRACE("Begin-draw -- ID mismatch, log %s (in use); playback %s\n",
								szObjID, pObj->m_strId);
					return FALSE;	// !!! no way to alert user to reason for failure
				}
			}
			Select(pObj);
			if ( pObj->IsKindOf(RUNTIME_CLASS(CAxes)) ||
				pObj->IsKindOf(RUNTIME_CLASS(CRadius)) ) {
				// For backwards compatility with logs through version 7.0.4 Needed 
				// since layering can affect which object is hit by a mouse click:
				if (g_nLogAndesVersion <= MakeVersionInt(7,0,4)) // assumes we are playing log 
					GetDocument()->MoveToBack(pObj);
				else
					GetDocument()->MoveToBackStudent(pObj);
			}
			if ( pObj->IsKindOf(RUNTIME_CLASS(CDocArea)) )
				GetDocument()->MoveToBack(pObj);
		}
		m_pCurrentObj = pObj;
		break;

	case EV_BEGIN_VECTOR:
		// "Begin-vector %d %s %d %d %d %d", m_vectorType, pObj->m_strId, point.x,  point.y);
		if (sscanf(pszArgs, "%d %s %d %d  %d %d", &subtype, szObjID, &lx, &ly) != 4)
			return FALSE;
		m_drawMode = Vector;
		m_vectorType = subtype;
		pObj = NewDrawObj(m_drawMode, CPoint(lx, ly));
		if (pObj != NULL) {
			GetDocument()->Add(pObj);
			// Should get same ID on playback, if deterministic. But id counters
			// might be changed invisibly in problem file if edited (add and delete, eg.).
			// Could log id counters; or create mapping; but for now try to force log id.
			if (pObj->m_strId != szObjID) {
				if (GetDocument()->Lookup(szObjID) == NULL) // log id is unused
					pObj->m_strId = szObjID; // so try to proceed this way (risky?)
				else {
					TRACE("Begin-draw -- ID mismatch, log %s (in use); playback %s\n",
								szObjID, pObj->m_strId);
					return FALSE;	// !!! no way to alert user to reason for failure
				}
			}
			Select(pObj);
		}
		m_pCurrentObj = pObj;
		break;
	
	case EV_BEGIN_RESIZE:		// begin resizing object (select tool mode)
		/* LogEventf(nEvent, "%s |%s| %d %d %d", pObj->m_strId, OBJ_NAME(pObj), m_nDragHandle, 
					             local.x, local.y); */
		if (sscanf(pszArgs, "%s %*s %d %d %d", szObjID,  &nHandle, &lx, &ly) != 4)
			return FALSE;
		// !!! Here and elsewhere, we parse bracketed labels as a scanf string
		// (and ignore, since they are unneeded), This will fail if the
		// string contains white space. Don't know how to fix this using scanf.
		// Tried using "|[^|]|" in the format string, but this fails to scan
		// a field if label is empty -- and scanf requires non-empty fields!.
		// Should write our own tokenizer, but not needed for now: these are now either
		// IDs or checked student labels, both non-empty and no spaces.
		m_drawMode = Selector;
		m_selectMode = resize;
		m_nDragHandle = nHandle;
		// Begin a tracking mode:
		m_pCurrentObj = SelectedObj();
		break;

	case EV_BEGIN_MOVE:	// select hit obj and begin possible drag (may go nowhere)
		// %s |%s| %d %d %d", pObj->m_strId, OBJ_NAME(pObj), local.x, local.y, bCtlFlag)
		if (sscanf(pszArgs, "%s %*s %d %d %d", szObjID, &lx, &ly, &bFlag) != 4) 
			return FALSE;
		m_drawMode = Selector;
		m_selectMode = move;
		pObj = GetDocument()->Lookup(szObjID);
		if (! pObj) // !!! not found by id, try by location
			 pObj = GetDocument()->ObjectAt(CPoint(lx, ly), Unselectable);
		ASSERT(pObj != NULL);
		m_posVirtual = pObj->m_position;
		// Do the select
		if ( ( IsSelected(pObj) && (pObj->GetHit() != pObj->GetSelectedPart()) ) ||
			!IsSelected(pObj) ){ // clicked new selected object or selected part
			Select(pObj, bFlag);
		} else if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
			InvalObjInView(pObj);// in case click different part of axes
		// Begin a tracking mode:
		m_pCurrentObj = pObj;
		break;

	case EV_CLICK_BG:	// start net select, clearing sel unless add
		// "Click-bg %d %d %d", local.x, local.y, bControlFlag); 
		if (sscanf(pszArgs, "%d %d %d", &lx, &ly, &bFlag) != 3)
			return FALSE;
		if (!bFlag)	// not adding to selection: clear it.
			Select(NULL);
		m_selectMode = netSelect;
		// draw initial net select rect
		ptClient = CPoint(lx, ly);
		DocToClient(ptClient);
		{
			CClientDC dc(this);
			CRect rcFocus(ptClient.x, ptClient.y, ptClient.x, ptClient.y);
			rcFocus.NormalizeRect();
			dc.DrawFocusRect(rcFocus);
		}
		break;

	case EV_MOUSE_MOVE:	// Move mouse. Note only logged if tracking while dragging
		// special optimization: in FF playback, ignore intermediate moves in drags.
		// and just update tracking position once on final mouse up.
		// !!! EXCEPTION -- now that we restrict positions for axes, we need intermediate 
		// moves to take effect, because final move may be to illegal position so ignored.
		if (LogPlayerFF() && m_drawMode != Axes)
			return TRUE;

		/* LogEventf(nEvent, "%d %d", point.x, point.y);   */
		if (sscanf(pszArgs, "%d %d", &lx, &ly) != 2)
			return FALSE;
		// following will be suppressed in FF playback. Will get final
		// object position from mouse up event.
		ptClient = CPoint(lx, ly);
		DocToClient(ptClient);	
#if 0	// in demo mode, track drag with the pointer if it is showing as well
		if (theApp.m_bDemoMode && theApp.GetMainFrame()->PtrIsVisible()) {
			CPoint ptScreen = ptClient;
			ClientToScreen(&ptScreen);
			// !!don't want to animate; +slow, flickery if msg hides/shows on each move
			theApp.GetMainFrame()->MovePointerTo(ptScreen.x, ptScreen.y);
		}
#endif 0
		// recreate the move in current client area
		TrackMove(0, ptClient);
		UpdateWindow();
		break;

	case EV_MOUSE_UP:	// Release mouse button after tracking.
		// LogEventf(nEvent, "%d %d  %d %d", point, x, point.y, local.x, local.y); 
		if (sscanf(pszArgs, "%*d %*d %d %d", &lx, &ly) != 2) return FALSE;
		ptClient = CPoint(lx, ly);
		DocToClient(ptClient);
		// we do a TrackMove here in case FF mode skipped last mouse move
		TrackMove(0, ptClient);
		// and recreate the release in current client coords.
		DoPostTracking(0, ptClient);
		// End the tracking mode:
		m_drawMode = Selector;
		m_selectMode = none;
		m_pCurrentObj = NULL;
		break;

	// To recreate initial entries in the FBDView:
	case EV_FBD_ENTRY:
		// scan first type arg and create new object of right type
		if (sscanf(pszArgs, "%s", szArg) != 1) return FALSE;	
		if ((pObj = CreateNew(szArg)) == NULL) return FALSE;
		// set obj's properties from the string (includes id, status).
		// (Pass whole string with typename since vectors can use it to determine subtype)
		if (! pObj->SetFromLogStr(pszRest)) return FALSE;
		// add to document and update. Skip Doc->Add since it sets modified flag
		GetDocument()->m_objects.AddTail(pObj);
		pObj->m_pDocument = GetDocument();
		pObj->Invalidate();
		break;

	case EV_ANSWER_ENTRY:
		// Parse id, status, text from arguments
		if (sscanf(pszArgs, "%s", szObjID) != 1) return FALSE;	
		pszRest += strlen(szObjID) + 1;
		if ((pObj = GetDocument()->Lookup(szObjID)) == NULL) return FALSE;
		sscanf(pszRest, "%s", szArg); pszRest += strlen(szArg) + 1;
		if (sscanf(szArg, "%d", &nStatus) != 1) return FALSE;
		ASSERT(0 <= nStatus && nStatus <= 2);
		// update document from arg string
		pObj->m_status = (Status) nStatus;
		pObj->m_strName = pszRest;
		// Sync the control with the document (urgh no sub or update hint to do this)
		if ((pEdit = FindAnswerEdit(pObj->m_strId)) == NULL) return TRUE; // skip if not found
		m_bIgnoreChange = TRUE;
		pEdit->SetWindowText(pObj->m_strName);
		TRACE("Setting status color of %s to %d\n", pEdit->m_strId, (int) pObj->m_status);
		if (pObj->m_status == statusCorrect)
			pEdit->SetTextColor(RGB(0, 128, 0));	// Dark green
		else if (pObj->m_status == statusError)
			pEdit->SetTextColor(RGB(255, 0, 0));	// Red
		else
			pEdit->SetTextColor(RGB(0, 0, 0));		// Black
		m_bIgnoreChange = FALSE;	
		break;

	//
	// Nothing to do for following, handled in mouse-up.
	//
	case EV_OBJ_VECTOR:	// Finished vector drawing
	case EV_OBJ_AXES:	// Finished axes drawing
	case EV_OBJ_SYSTEM:	// Finished system drawing
	case EV_OBJ_RADIUS: // Finished Radius drawing
	case EV_OBJ_OTHER:	// Finished other graphic object drawing
	case EV_OBJ_TEXT:	// Finished entering text
		break;

	case EV_OBJ_ANGLE: // No mouse click for angle drawing
		OnAngle();
		break;

	case EV_SET_ZDIR:	// arg is ZDir code
		if (sscanf(pszArgs, "%d", &nCtlID) != 1) return FALSE;
		OnSetZDir(nCtlID + ID_ZDIR_FIRST);	// map back to command
		break;

	// Summary of property editing:
	case EV_PROPS_VECTOR:// records new vector properties after editing
	case EV_PROPS_FORCE: // records new force properties after editing
	case EV_PROPS_COMPO: // records new component properties after editing
	case EV_PROPS_AXES:	 // records new axes properties after editing
	case EV_PROPS_SYSTEM:// records new system properties after editing
	case EV_PROPS_RADIUS:// records new radius properties after editing
	case EV_PROPS_VARIABLE:// records new variable properties after editing
		break;
	//
	// Changes in selection: although traced in detail for checking purposes, 
	// these should all be by-products of other actions which we already reproduce.
	// (Grep "Select" to verify). So we shouldn't need to
	// do anything special to reproduce them on playback. Could try to verify
	// that current selection is OK.
	//
	case EV_REMOVE_SEL:	// Traces deselection of old obj inside Select call
	case EV_SELECT:		// Traces Select calls, possibly to NULL to clear
	case EV_DESELECT:	// Traces Deselect calls -- currently unused!
		break;

	case EV_FBD_MENU: // "%d %d  %d %d", point.x, point.y, local.x, local.y);
		// can't replay menu w/o entering modal loop.
		// but must reproduce right-button selection change if any
		if (sscanf(pszArgs, "%*d %*d %d %d", &lx, &ly) != 2) return FALSE;
		local = CPoint(lx, ly);
		// Should be in common routine like SelectAt(local)
		if ((pObj = GetDocument()->ObjectAt(local, /* ignore:*/ Unselectable)) != NULL)
		{
			if ( ( IsSelected(pObj) && (pObj->GetHit() != pObj->GetSelectedPart()) ) ||
				!IsSelected(pObj) )	//new selected object or selected part
			Select(pObj, FALSE);
		}
		break;
	//
	// Following are simple (parameter-less) commands. They operate on current 
	// selection; hence require it to be valid at playback time
	//
	case EV_DELETE:		// Deleted object
		// "Delete %x |%s|", OBJ_ID(pObj), OBJ_NAME(pObj));
		OnEditDelete();
		break;
	case EV_EDIT_PROPS:
		OnEditProperties();
		break;

	case EV_DUPLICATE:	// Duplicate object
		// "Dup %x |%s|", OBJ_ID(m_pSelection), OBJ_NAME(m_pSelection) );
		// !!! This is wrong for multiple selections. In that case EV_DUPLICATE is 
		// logged for each duplicated object. So it would have to be replayed by
		// DUPing one object, not duping selection
		OnEditDuplicate();
		break;
	case EV_OBJECT:		// Records object created after duplicate
		// record object ID in symbol table?
		break;
	case EV_MARK_VECTOR:// Toggled mark on vector
		// "Mark %x |%s| %d", OBJ_ID(pVector), OBJ_NAME(pVector), pVector->m_bDecomposed);
		OnDecomposeVector();
		break;
	case EV_FBD_WHATSWRONG:
		OnHelpWhatswrong();
		break;
	
	case EV_FBD_TCARD_MSG:	// "%u %d", idAction, (int) dwActionData);
	{ 
		UINT idAction; DWORD dwActionData;
		if (sscanf(pszArgs, "%u %d", &idAction, &dwActionData) != 2)
			return FALSE;
		OnTCard(idAction, dwActionData);
	} break;
	
	// Answer box events
	case EV_ANSWER_FOCUS: // LogEventf(EV_ANSWER_FOCUS, "%s", pEdit->m_strId);
		// only arg is answer object id. We need to find child edit control
		if ((pEdit = FindAnswerEdit(pszArgs)) == NULL)
			return FALSE;
		// recreate user action on state
		// pEdit->SetFocus(); // does this do anything if window disabled?
		// recreate response
		OnAnswerFocus(pEdit->GetDlgCtrlID());
		break;

	case EV_ANSWER_KILLFOCUS:
		if ((pEdit = FindAnswerEdit(pszArgs)) == NULL)
			return FALSE;
		// nothing to do to recreate action?
		OnAnswerKillFocus(pEdit->GetDlgCtrlID());
		break;

	case EV_ANSWER_CHANGE: // LogEventf(EV_ANSWER_CHANGE, "%s %s", pEdit->m_strId,  strAnswer); 
		// String which follows first arg may be empty or contain white space
		// Terminates at end of arg string
		pszRest = pszArgs;
		sscanf(pszArgs, "%s", szObjID);		
		pszRest += strlen(szObjID) + 1;
		if ((pEdit = FindAnswerEdit(szObjID)) == NULL)
			return FALSE;
		pEdit->SetWindowText(pszRest);	// should trigger EN_CHANGE notification
		break;

	case EV_ANSWER_SUBMIT: //LogEventf(EV_ANSWER_SUBMIT,"%s %s", pEdit->m_strId, strEq);
		sscanf(pszArgs, "%s", szObjID);		
		if ((pEdit = FindAnswerEdit(szObjID)) == NULL)
			return FALSE;
		// ignore contents arg, just submit current contents
		OnAnswerEnter();
		break;

	case EV_ANSWER_STATUS: // Trace message on status change, ignored.
		break;
	// Multiple choice question events
	case EV_CHOICE_CLICK:		// arg is integer child id
		if (sscanf(pszArgs, "%d", &nCtlID) != 1)
			return FALSE;
		pCtl = GetDlgItem(nCtlID);
		if (!pCtl || ! pCtl->IsKindOf(RUNTIME_CLASS(CButton)) ) 
			return FALSE;
		// Following simulates user click, complete with notification msg back
		// to parent to fire its handler in the dialog
		pCtl->SendMessage(BM_CLICK);
		break;

	//
	// On popup of dialog boxes for editing properties (really initdialog). 
	// This is just trace info, dialog popup must be initiated by some other handler,
	// e.g for editproperties command or double-click, or in course of handling drawop.
	//
	case EV_DLG_TEXT:	// Popped up text label dialog
		OnLabel();
		break;
	}
	return TRUE;
}

// Create new drawobj instance of right class given type name
CDrawObj* CFBDView::CreateNew(LPCSTR pszTypeName)
{
	CString strType = pszTypeName;
	// need to invert mapping from classes to drawobj type names. 
	// Could search MFC's runtime class list, but for now just use a switch.
	
	// strip Ang- prefix off vector type names
	if (_strnicmp(strType, "Ang-", 4) == 0) 
		strType = strType.Mid(4);

	if (strType == "Body") return new CSystem();
	// For vectors, type name comes from CVector::GetTypeName. !!!Centralize table 
	else if (strType == "Force" || strType == "Velocity" || strType == "Acceleration" 
		    || strType == "Component" || strType == "Displacement" || strType == "Momentum"
			|| strType == "Position" || strType == "Torque" || strType == "Relative-Vel"
			|| strType == "Impulse" || strType == "E-field" || strType == "B-field"
			|| strType == "Unit-Vector" || strType == "Mag-dipole" || strType == "Elec-dipole") 
		return new CVector();
	else if (strType == "Axes") return new CAxes();
	else if (strType == "Angle") return new CAngle();
	else if (strType == "Radius") return new CRadius();
	else if (strType == "MotionDiagram") return new CMotionDiagram();
	else if (strType == "MotionBody") return new CMotionBody();
	else if (strType == "MDVector") return new CMDVector();
	else if (strType == "2DMotion") return new C2DMotion();
	else return NULL;
}

void CFBDView::PointToObject(LPCTSTR pszObjID)
{
	CRect rcObj;
	// first see if this is logical page point spec of form x,y  
	// if so, put x,y into rect's bottom right.
	int xPage, yPage;
	if (sscanf(pszObjID, "%d,%d", &xPage, &yPage) == 2)
		rcObj = CRect(0, 0, xPage, yPage); // code below points at bottom right
	else 
	{
		// get diagram object by its ID
		CFBDDoc* pDoc = GetDocument(); 
		ASSERT(pDoc != NULL);
		CDrawObj* pObj = pDoc->Lookup(pszObjID); 
		if (! pObj) 
			return;
		rcObj = pObj->m_position;
	}
	
	// get from page to screen coords
	DocToClient(rcObj);
	ClientToScreen(rcObj);

	// and now move the pointer, use bottom right since it points up left
	theApp.GetMainFrame()->MovePointerTo(rcObj.right, rcObj.bottom);
}



BOOL CFBDView::TwoLinesSelected(CDrawObj* pObj1, CDrawObj* pObj2)
{
	if ( ((pObj1->IsKindOf(RUNTIME_CLASS(CVector)))||(pObj1->IsKindOf(RUNTIME_CLASS(CAxes))))
		&& ((pObj2->IsKindOf(RUNTIME_CLASS(CVector)))||(pObj2->IsKindOf(RUNTIME_CLASS(CAxes)))) )
		return TRUE;

	else 
		return FALSE;
}

CPoint CFBDView::IntersectPoint(CDrawObj* pObj1, CDrawObj* pObj2, int nAxis)
{
	CRect rect1 = pObj1->m_position;
	TRACE("Axis is %d\n", nAxis);
	if (pObj1->IsKindOf(RUNTIME_CLASS(CAxes))){
		CPoint point;
		if (nAxis == -1)
			point = ((CAxes*)pObj1)->GetAxisPoint();
		else
			point = ((CAxes*)pObj1)->GetAxisPoint(nAxis);
		rect1.BottomRight() = point;
	}

	CPoint point1 = ExtendStartLine(pObj1, nAxis);
//	double x1ofOne = rect1.left; 
	double x1ofOne = point1.x; 
	double x2ofOne = rect1.right;
	double y1ofOne = point1.y; 
//	double y1ofOne = rect1.top; 
	double y2ofOne = rect1.bottom;
	double deltaXofOne =  (x2ofOne-x1ofOne);
	double deltaYofOne = (y2ofOne-y1ofOne);
	double slopeOfOne = (deltaYofOne/deltaXofOne);
	double bOfOne = (y1ofOne)- (slopeOfOne*x1ofOne);//y=mx+b (b=intercept)
	
	CRect rect2 = pObj2->m_position;
	if (pObj2->IsKindOf(RUNTIME_CLASS(CAxes))){
		CPoint point;
		if (nAxis == -1)
			point = ((CAxes*)pObj2)->GetAxisPoint();
		else
			point = ((CAxes*)pObj2)->GetAxisPoint(nAxis);
		rect2.BottomRight() = point;
	}
	CPoint point2 = ExtendStartLine(pObj2, nAxis);
	//double x1ofTwo = rect2.left;  
	double x1ofTwo = point2.x;  
	double x2ofTwo = rect2.right;
	double deltaXofTwo = (double) (x2ofTwo-x1ofTwo);
//	double y1ofTwo = rect2.top; 
	double y1ofTwo = point2.y; 
	double y2ofTwo = rect2.bottom;
	double deltaYofTwo = (double) (y2ofTwo-y1ofTwo);
	double slopeOfTwo = (deltaYofTwo/deltaXofTwo);
	double bOfTwo = ((double)y1ofTwo)-(slopeOfTwo*x1ofTwo);//y=mx+b (b=intercept)

	int xIntercept;
	int yIntercept;
	if (slopeOfOne==slopeOfTwo)//lines are parallel (they don't intersect)
		return (0, 0);//Problem with this, lines facing opposite directions
	else if (deltaXofOne==0.0)//line 1 is vertical
	{
		xIntercept = rect1.left;
		if (deltaYofTwo==0.0)//AND line 2 is horizontal
			yIntercept = rect2.top;
		else//line 2 NOT horizontal
			yIntercept = (int)((slopeOfTwo*(double)xIntercept)+bOfTwo);
		//check to see if y intercept lies on line segment 1
		if ((yIntercept<__min(y1ofOne, y2ofOne)) || (yIntercept> __max(y1ofOne,y2ofOne)))
			return (0,0);
	}
	else if (deltaXofTwo==0.0)//line 2 is vertical
	{
		xIntercept = rect2.left;
		if (deltaYofOne==0.0)//AND line 1 is horizontal
			yIntercept = rect1.top;
		else//line 1 NOT horizontal
			yIntercept = (int)((slopeOfOne*(double)xIntercept)+bOfOne);
		//check to see if y intercept lies on line segment 2
		if ((yIntercept<__min(y1ofTwo, y2ofTwo)) || (yIntercept> __max(y1ofTwo,y2ofTwo)))
			return (0,0);
	}
	else if (deltaYofOne==0.0)//line 1 is horizontal (line 2 not vertical)
	{
		yIntercept = rect1.top;
		xIntercept = (int)(((double)yIntercept-bOfTwo)/slopeOfTwo);
	}
	else if (deltaYofTwo==0.0)//line 2 is horizontal (line 1 not vertical)
	{
		yIntercept = rect2.top;
		xIntercept = (int)(((double)yIntercept-bOfOne)/slopeOfOne);
	}
	else
	{//Neither line 1 nor line 2 vertical or horizontal
		xIntercept = (int)((bOfTwo-bOfOne)/(slopeOfOne-slopeOfTwo));
		yIntercept = (int)((slopeOfOne*(double)xIntercept)+bOfOne);
	}
	CPoint intersectPoint(xIntercept, yIntercept);//point where lines will intersect
	
	//check to see if intersection point (x intercept) lies on line segment 1
	if ((xIntercept>= __min(x1ofOne, x2ofOne)) && (xIntercept <= __max(x1ofOne, x2ofOne)))	
	{//check to see if intersection point (x intercept) lies on line segment 2
		if ((xIntercept>= __min(x1ofTwo, x2ofTwo)) && (xIntercept <= __max(x1ofTwo, x2ofTwo)))
			return intersectPoint;
		else 
			return (0,0);
	}
	else 
		return (0,0);
}

CRect CFBDView::GetAnglePosition(/*CPoint point*/)
{
	CPoint startPoint, endPoint;
	CDrawObj* pSide1;
	CDrawObj* pSide2;
	CDrawObj* pObj1 = m_Selection.GetHead();
	CDrawObj* pObj2 = m_Selection.GetTail();
	CPoint m_intercept = IntersectPoint(pObj1 , pObj2, -1);
	POINT ptArray[3];
	startPoint = ExtendEndLine(pObj1, -1);
	endPoint = ExtendEndLine(pObj2, -1);
	
	ptArray[0]= m_intercept;
	ptArray[1]= startPoint;
	ptArray[2]= endPoint;
	
/*	CRgn tempRegion;
	tempRegion.CreatePolygonRgn((LPPOINT)&ptArray, 3, ALTERNATE);
	if (tempRegion.PtInRegion(point)){
		if (CCW(ptArray[0], ptArray[1], ptArray[2])<0){
			RemoveFromSel(pObj1);//rearrange so that head of selection list is 
			m_Selection.AddTail(pObj1);//the vector which contains the start point 
		}								//of the angle
		
	}
	else{
		if (CCW(ptArray[0], ptArray[1], ptArray[2])>0){
			RemoveFromSel(pObj1);//rearrange so that head of selection list is 
			m_Selection.AddTail(pObj1);//the vector which contains the start point 
		}								//of the angle
	}*/
	pSide1 = m_Selection.GetHead();
	pSide2 = m_Selection.GetTail();
//	tempRegion.DeleteObject();
	int radius1 = GetRadius(m_intercept, startPoint);
	int radius2 = GetRadius(m_intercept, endPoint);
	int radius = ((radius1 <= radius2) ? radius1 : radius2);
	if  (radius > 55)
		radius = 55;
	startPoint= GetPointOnLine(pSide1, m_intercept, radius, -1);
	endPoint= GetPointOnLine(pSide2, m_intercept, radius, -1);
	return CRect(startPoint, endPoint);
}

//CCW (CounterClockwise)
//Determines, given three points, if when travelling from the first to
//the second to the third, we travel in a counterclockwise direction.
//returns 1 if travel in CCW direction, -1 if not
BOOL CFBDView::CCW(CPoint p0, CPoint p1, CPoint p2)
{
	double dx1, dx2;
	double dy1, dy2;
	dx1 = p1.x - p0.x;
	dx2 = p2.x - p0.x;
	dy1 = p0.y - p1.y;
	dy2 = p0.y - p2.y;
	return (((dx1 * dy2) > (dy1 * dx2)) ? 1 : -1);
}

//Determines if two line segments intersect
BOOL CFBDView::Intersect(CPoint p1, CPoint p2, CPoint p3, CPoint p4)
{
	return ( ( (CCW(p1, p2, p3) * CCW(p1, p2, p4)) <= 0) &&
					( (CCW(p3, p4, p1) * CCW(p3, p4, p2)) <= 0) );
}

int CFBDView::GetRadius(CPoint intercept, CPoint point)
{
	double deltax = (point.x - intercept.x);
	double deltay = (point.y - intercept.y);
	int radius = (int) sqrt((deltax*deltax)+(deltay*deltay));
	return radius;
}

CPoint CFBDView::GetPointOnLine(CDrawObj* pLine, CPoint intercept, int radius, int nAxis)
{
	CPoint ptFrom = pLine->m_position.TopLeft();
   	CPoint ptTo = pLine->m_position.BottomRight();

	int dx = ptTo.x - ptFrom.x;
   	int dy = ptTo.y - ptFrom.y;
   	
   	// atan2 returns angle in radians in range -pi to pi.
   	// this is direction in device coordinates in which positive y
   	// is downward, hence a clockwise angle measure
   	double radsFromZero = atan2(dy,  dx);
    
	const double pi = 3.1415926535;	// Not predefined somewhere??
	if (pLine->IsKindOf(RUNTIME_CLASS(CAxes)))
	{
		if (nAxis == -1)
			radsFromZero = radsFromZero + ((((CAxes*)pLine)->m_nAxis-POSX) * (-pi/2));
		else
			radsFromZero = radsFromZero + ((nAxis-POSX) * (-pi/2));
	}

	double realx = radius*(cos(radsFromZero));
	double realy = radius*(sin(radsFromZero));
	int x = Round(realx);
	int y = Round(realy);
	x = x + intercept.x;
	y = y + intercept.y;

	return CPoint(x, y);
}



CPoint CFBDView::ExtendStartLine(CDrawObj* pLine, int nAxis)
{
	double angFromZero;
	double const pi = 3.1415926535;
	angFromZero = ((double)(pLine->GetDirection()));///!!!!!!!!!!!!!!!!!
	if (pLine->IsKindOf(RUNTIME_CLASS(CAxes))){
		if (nAxis == -1)
			angFromZero = angFromZero + ((((CAxes*)pLine)->m_nAxis-POSX) * 90);
		else
			angFromZero = angFromZero + ((nAxis-POSX) * 90);
	}
	double radsFromZero = angFromZero*((2.0*pi)/360.0);
	double dy = 20 * (sin (-radsFromZero));
	double dx = 20 * (cos (radsFromZero));
	int	x = pLine->m_position.left;
	int	y = pLine->m_position.top;
	x = x - (int)dx;
	y = y - (int)dy;
	
	return CPoint(x, y);
}

CPoint CFBDView::ExtendEndLine(CDrawObj* pLine, int nAxis)
{
	double angFromZero;
	double const pi = 3.1415926535;
	CRect rect = pLine->m_position;
	angFromZero = ((double)(pLine->GetDirection()));///!!!!!!!!!!!!!!!!!
	if (pLine->IsKindOf(RUNTIME_CLASS(CAxes))){
		CPoint point;
		if (nAxis == -1){
			angFromZero = angFromZero + ((((CAxes*)pLine)->m_nAxis-POSX) * 90);
			point = ((CAxes*)pLine)->GetAxisPoint();
		}
		else{
			angFromZero = angFromZero + ((nAxis-POSX) * 90);
			point = ((CAxes*)pLine)->GetAxisPoint(nAxis);
		}
		rect.BottomRight() = point;
	}
	double radsFromZero = angFromZero*((2.0*pi)/360.0);
	double dy = 1000 * (sin (-radsFromZero));
	double dx = 1000 * (cos (radsFromZero));
	
	int	x = rect.right;
	int	y = rect.bottom;
	x = x + (int)dx;
	y = y + (int)dy;
	
	return CPoint(x, y);
}

void CFBDView::CheckMultSel()
{
	CDrawObjList delAngList;

	POSITION pos = m_Selection.GetHeadPosition();
	while (pos != NULL) 
	{
		CDrawObj* pObj = m_Selection.GetNext(pos);
		if (! pObj->IsKindOf(RUNTIME_CLASS(CVector)) && !pObj->IsKindOf(RUNTIME_CLASS(CAxes)) )
			continue;

		if (!pObj->m_Angles.IsEmpty())
		{
			POSITION posAng = pObj->m_Angles.GetHeadPosition();
			while (posAng != NULL) 
			{
				CDrawObj* pAng = pObj->m_Angles.GetNext(posAng);
				if ( IsSelected(pAng) && 
					IsSelected(((CAngle*)pAng)->m_pAngSide1) &&
						IsSelected(((CAngle*)pAng)->m_pAngSide2) )
					continue;
				else
					delAngList.AddTail(pAng);
			}
		}
	}
	GetDocument()->DeleteObjList(&delAngList);			

}

// Set display font. On example edit view menu. Mainly for testing by authors
void CFBDView::OnViewFont() 
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
	// RecalcTextExtents();
	Invalidate();
}

/* 
 * Training Card menu commands: 
 */

void CFBDView::OnHelpDrawforce()
{
	theApp.CallTCardHelp(this, ID_HELP_DRAWFORCE);
}

void CFBDView::OnUpdateHelpDrawforce(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnHelpDrawacceleration() 
{
	theApp.CallTCardHelp(this, ID_HELP_DRAWACCELERATION);
}

void CFBDView::OnUpdateHelpDrawacceleration(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnHelpDrawbody() 
{
	theApp.CallTCardHelp(this, ID_HELP_DRAWBODY);
}

void CFBDView::OnUpdateHelpDrawbody(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnHelpDrawvelocity() 
{
	theApp.CallTCardHelp(this, ID_HELP_DRAWVELOCITY);
}

void CFBDView::OnUpdateHelpDrawvelocity(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_bEnabled);	
}

void CFBDView::OnHelpDrawdisplacement() 
{
	theApp.CallTCardHelp(this, ID_HELP_DRAWDISPLACEMENT);
}

void CFBDView::OnUpdateHelpDrawdisplacement(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnHelpDrawmotionbody() 
{
	theApp.CallTCardHelp(this, ID_HELP_DRAWMOTIONBODY);
}

void CFBDView::OnUpdateHelpDrawmotionbody(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_bEnabled);
	
}

/* 
 * training card implementation support (some could be moved to App):
 */

// start tcard help with given ID, using this window to receive messages
void CFBDView::ShowTCard(int idTCard) 
{
	theApp.CallTCardHelp(this, idTCard);
}

// Handle notification msg sent from Winhelp to us on user action in card:
void CFBDView::OnTCard(UINT idAction, DWORD dwActionData) 
{
	// Log it so can replay this sequence:
	LogEventf(EV_FBD_TCARD_MSG, "%u %d", idAction, (int) dwActionData);

	if (idAction == HELP_TCARD_DATA) // clicked an authorable button
	{
		// Our tcards only send this on custom continue buttons in initial prompt cards 
		// (topics HID_HELP_DRAWFORCE, etc.) which ask user if should delete last entry 
		// and begin a training sequence (topics HID_TRAINFORCE, etc).
		// Action data codes the training sequence to begin.

		// Lookup last student entry, if any. 
		CCheckedObj* pLastEntry = NULL;
		CDrawObj* pLastObj = GetDocument()->GetObjects()->GetTail();
		if (pLastObj && pLastObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)) 
			&& pLastObj->m_flag == STUDENT_OBJECT)
			pLastEntry = (CCheckedObj*) pLastObj;

		BOOL bMatchLast = FALSE; // true if TCard type matches type of last entry
		DWORD dwId;
		if (dwActionData == 21) {
			dwId = ID_TRAINFORCE;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CVector))
				         && ((CVector*)pLastEntry)->m_nVectorType == VECTOR_FORCE;
		} else if (dwActionData == 22) {
		    dwId = ID_TRAINVELOCITY;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CVector))
				         && ((CVector*)pLastEntry)->m_nVectorType == VECTOR_VELOCITY;
		} else if (dwActionData == 23) {
			dwId = ID_TRAINACCELERATION;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CVector))
				         && ((CVector*)pLastEntry)->m_nVectorType == VECTOR_ACCELERATION;
		} else if (dwActionData == 24) {
			dwId = ID_TRAINBODY;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CSystem));
		} else if (dwActionData == 25) {
			dwId = ID_TRAINRULER;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CMotionDiagram));
		} else if (dwActionData == 26) {
			dwId = ID_TRAINPOSITION;
				bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CMotionBody));
		} else if (dwActionData == 27) {
			dwId = ID_TRAINMOTVEL;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CMDVector))
				         && ((CVector*)pLastEntry)->m_nVectorType == VECTOR_VELOCITY;
		} else if (dwActionData == 28) {
			dwId = ID_TRAINMOTACC;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CMDVector))
				         && ((CVector*)pLastEntry)->m_nVectorType == VECTOR_ACCELERATION;
		} else if (dwActionData == 29) {
			dwId = ID_TRAINDISPLACEMENT;
			bMatchLast = pLastEntry && pLastEntry->IsKindOf(RUNTIME_CLASS(CVector))
				         && ((CVector*)pLastEntry)->m_nVectorType == VECTOR_DISPLACEMENT;
		}

		// delete last entry if it matches type and is bad.
		if (bMatchLast && pLastEntry->m_status == statusError){
			// ?? ensure selected ?
			GetDocument()->UpdateAllViews(NULL,  HINT_DELETE_SELECTION , &m_Selection);
			GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_WINDOW, NULL);
			GetDocument()->Remove(pLastObj); // triggers RemoveFromSel callback
			//Notify help system
			pLastEntry->NotifyDelete();
			pLastEntry->Delete();
		}

		// now enter the specified training card sequence
		theApp.m_bTrainMode = dwId;
		theApp.CallTCardHelp(this, dwId);
	}
	else if (idAction == IDCLOSE){	// user closed the training card
		theApp.m_bTrainMode = FALSE;
		KillTCard();
	}
}

// utility: send command to advance to next card in author-defined browse sequence:
void CFBDView::NextInCard()
{
	theApp.CallTCardHelp(this, (DWORD)"Next()", HELP_COMMAND);
}

// tell TCard help to close:
void CFBDView::CloseTCardHelp()
{
	theApp.CallTCardHelp(this, 0, HELP_QUIT);
	//why doesn't help quit when called after tcard completion??????
}

// work around documented bug in Winhelp training card implementation
void CFBDView::KillTCard()
{
	DWORD dwId, dwExit;
	HWND hWnd = ::FindWindow((LPCSTR)"MS_TCARDHELP", (LPCTSTR)0);
	if (hWnd > 0){
		GetWindowThreadProcessId(hWnd, &dwId);
		HANDLE hProc = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, dwId);
		GetExitCodeProcess(hProc, &dwExit);
		TerminateProcess(hProc, dwExit);
	}
}


//
// Support for clickable hypertext links in problem pane (e.g. in problem statement):
//

void CFBDView::OnHypertext() 
{
	m_drawMode = HyperText;
	//
	// For now, just pop up a dialog to collect a line of text
	//
	CHypertxtDlg dlg;
	if (! (HFONT) m_fontText) // no font selected yet
	{
		CClientDC dc(this);
		CFont* pScrnFont = dc.GetCurrentFont();
		pScrnFont->GetLogFont(&dlg.m_logFont);
	}
	else
		m_fontText.GetLogFont(&dlg.m_logFont);
	if (dlg.DoModal() == IDOK) 
	{
		// Create label object, add it at default position to diagram
		// and leave it selected. User must position
		CClientDC dc(this);		// for current font, mapping mode
		// CSize extent = dc.GetTextExtent(dlg.m_strText); 	
		CRect pos(CPoint(10, 10), CSize(1, 1) );// default pos (logical) w/dummy extent
		(void) dc.DrawText(dlg.m_strText, pos, DT_CALCRECT); // updates into pos
		// make hypertext object and put in document
		CHyperLnk* pLnk = new CHyperLnk(pos, dlg.m_strText);
		pLnk->m_nType = dlg.m_nHyperType;
		pLnk->m_font.CreateFontIndirect(&dlg.m_logFont);
		if (pLnk->m_nType == 0)
			pLnk->m_strLink = dlg.m_strLink;
		else 
			pLnk->m_strLink = dlg.m_strDef;

		m_pCurrentObj = pLnk;
		GetDocument()->Add(pLnk);
	
		// Log new hypertext
		LogEventf(EV_OBJ_TEXT, "%s %d %d %d %d", pLnk->m_strId, pLnk->m_position.left, 
			     pLnk->m_position.top, pLnk->m_position.bottom, pLnk->m_position.right);

		//  Make new object selected. (Invalidates it in view).
		Select(pLnk);
		// Notify other views of new object
		GetDocument()->UpdateAllViews(this, HINT_UPDATE_DRAWOBJ, pLnk);
	}

	m_drawMode = Selector;
	
}

CHyperLnk* CFBDView::HyperAt(CPoint point)
{
	CFBDDoc* pDoc = GetDocument();
	POSITION pos = pDoc->m_objects.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = pDoc->m_objects.GetNext(pos);
		if (pObj->HitTest(point, this, FALSE)){
			if (pObj->IsKindOf(RUNTIME_CLASS(CHyperLnk))){
				CHyperLnk* pLnk = (CHyperLnk*)pObj;
				return pLnk;
			}
		}
	}
	return NULL;
}

BOOL CFBDView::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
	if (m_drawMode != notDrawing){
//	This is where I need to put in the pretty cursors
		HCURSOR hCursor = GetDrawtoolCursor(m_drawMode);
		::SetCursor(hCursor);	
		return TRUE;
	}
	else if (m_bOnHyperText){
		HCURSOR hCursor = AfxGetApp()->LoadCursor(IDC_LINK);
		::SetCursor(hCursor);	
		return TRUE;
	}
	return CBaseView::OnSetCursor(pWnd, nHitTest, message);
}


HCURSOR CFBDView::GetDrawtoolCursor(drawMode tool)
{
	UINT idCursor;
	switch(tool){
	case Vector: 
		if (m_vectorType == VECTOR_ACCELERATION) {
			if (m_nZDir)  idCursor = m_nZDir == ZDIR_INTO ? IDC_ZACCEL_INTO : IDC_ZACCEL_OUTOF;
			else idCursor = IDC_DRAWACCELERATION;	
		} else if (m_vectorType == VECTOR_VELOCITY) {
			if (m_nZDir) idCursor = m_nZDir == ZDIR_INTO ? IDC_ZVEL_INTO : IDC_ZVEL_OUTOF;
			else idCursor = IDC_DRAWVELOCITY; 
		} else if (m_vectorType == VECTOR_FORCE) {
			idCursor = IDC_DRAWFORCE; 
		} else if (m_vectorType == VECTOR_DISPLACEMENT) {
			if (m_nZDir)  idCursor = m_nZDir == ZDIR_INTO ? IDC_ZDISP_INTO : IDC_ZDISP_OUTOF;
			else idCursor = IDC_DRAWDISPLACEMENT;
		}else if (m_vectorType == VECTOR_MOMENTUM) {
			if (m_nZDir)  idCursor = m_nZDir == ZDIR_INTO ? IDC_ZMOM_INTO : IDC_ZMOM_OUTOF;
			else idCursor = IDC_DRAWMOMENTUM;
		} else if (m_vectorType == VECTOR_TORQUE) {
			if (m_nZDir)  idCursor = m_nZDir == ZDIR_INTO ? IDC_ZTOR_INTO : IDC_ZTOR_OUTOF;
			else idCursor = IDC_DRAWTORQUE;
		} else if (m_vectorType == VECTOR_COMPONENT) {
			idCursor = IDC_DRAWCOMPONENT; 
		} else if (m_vectorType == VECTOR_POSITION) {
			idCursor = IDC_DRAWRELPOS;
		} else if (m_vectorType == VECTOR_EFIELD) {
			idCursor = IDC_DRAWEFIELD;
		} else if (m_vectorType == VECTOR_BFIELD) {
			idCursor = IDC_DRAWBFIELD;
		} else {
			TRACE("GetDrawtoolCursor:: Unknown vector type %d\n", m_vectorType);
			return AfxGetApp()->LoadStandardCursor(IDC_ARROW);
		}
		break;
	case Axes: idCursor  = IDC_DRAWAXES; break;
	case System: idCursor  = IDC_DRAWBODY; break;
	case Angle: idCursor  = IDC_DRAWANGLE; break;
	case Radius: idCursor  = IDC_DRAWRADIUS; break;
	default: return AfxGetApp()->LoadStandardCursor(IDC_ARROW); 
	}
	return AfxGetApp()->LoadCursor(idCursor); 
}



void CFBDView::OnUpdateLabel(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(theApp.m_bAuthorMode);
	
}

void CFBDView::OnUpdateHypertext(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(theApp.m_bAuthorMode);
	
}

void CFBDView::OnUpdatePolybezier(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(theApp.m_bAuthorMode);
	
}

void CFBDView::OnUpdateAxes(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(m_drawMode == Axes);
	if (m_drawMode != Axes) // now need multiple axes, perhaps in linmom probs only, though
		pCmdUI->Enable(/* !GetDocument()->IsAxesDrawn() &&*/ m_bEnabled);
	else
		pCmdUI->Enable(m_bEnabled);
}

void CFBDView::OnAngle() 
{
	// For angle between vectors, now always want the smallest of two possible angles. 
	// Code below and in GetAnglePos is based on CCW angle from first arg to second, 
	// so ensure the selection elements are ordered for smallest angle when CCW defined.
	CDrawObj* pObj1 = m_Selection.GetHead();
	CDrawObj* pObj2 = m_Selection.GetTail();
	if (pObj1->IsKindOf(RUNTIME_CLASS(CVector)) && 
		pObj2->IsKindOf(RUNTIME_CLASS(CVector))) {
		int nDir1 = pObj1->GetDirection();
		int nDir2 = pObj2->GetDirection();
		int nCCW_1to2 = (nDir1 > nDir2) ? 360 - abs(nDir1 - nDir2) : abs(nDir1 - nDir2);
		int nCCW_2to1 = (nDir2 > nDir1) ? 360 - abs(nDir2 - nDir1) : abs(nDir2 - nDir1);
		if (nCCW_2to1 < nCCW_1to2) {
			m_Selection.RemoveHead();	// deletes pObj1, leaving pObj2 at head
			m_Selection.AddTail(pObj1);	// put pObj1 at tail.
		}
	}

	CDrawObj* pObj = new CAngle(CRect(GetAnglePosition()));
	CAngle* pAng = (CAngle*)pObj;
	
	pAng->m_pAngSide1 =  m_Selection.GetHead();
	pAng->m_pAngSide2 =  m_Selection.GetTail();
	pAng->m_pAngSide1->m_Angles.AddTail(pObj);
	pAng->m_pAngSide2->m_Angles.AddTail(pObj);

	CPoint point1 = ((CAngle*)pObj)->m_pAngSide1->m_position.BottomRight();
	if (pAng->m_pAngSide1->IsKindOf(RUNTIME_CLASS(CAxes))){
		CAxes* pAxes =  ((CAxes*)pAng->m_pAngSide1);
		point1 = pAxes->GetAxisPoint();
		pAng->m_nAxis = pAxes->m_nAxis;
	}
	CPoint point2 = pAng->m_pAngSide2->m_position.BottomRight();
	if (pAng->m_pAngSide2->IsKindOf(RUNTIME_CLASS(CAxes))){
		CAxes* pAxes =  ((CAxes*)pAng->m_pAngSide2);
		point2 = pAxes->GetAxisPoint();
		pAng->m_nAxis = pAxes->m_nAxis;
	}
	pAng->m_intercept = IntersectPoint(((CAngle*)pObj)->m_pAngSide1, ((CAngle*)pObj)->m_pAngSide2, ((CAngle*)pObj)->m_nAxis);

	int radius1 = GetRadius(pAng->m_intercept, point1);
	int radius2 = GetRadius(pAng->m_intercept, point2);
	if (radius1 >= radius2)
		pAng->m_maxRadius = radius2;
	else
		pAng->m_maxRadius = radius1;
	if (pAng->m_maxRadius > 55 )
		pAng->m_radius = 55;
	else
		pAng->m_radius = pAng->m_maxRadius;

	pAng->SetAngleMag();
	
	if (pObj != NULL) {
		GetDocument()->Add(pObj);
		Select(pObj);
	}
	m_pCurrentObj = pObj;
	
	CString strAxis;
	if (pAng->m_nAxis == -1)
		strAxis = "NIL";
	else 
		strAxis = axes[pAng->m_nAxis-2].strHelp;

	LogEventf(EV_OBJ_ANGLE, "%s %s %s %s %d", pAng->m_strId, pAng->m_pAngSide1->m_strName, 
				pAng->m_pAngSide2->m_strName, strAxis, pAng->m_degAng);

	if(!pAng->OnEditProperties())
		DeleteSelection();
}


// For Z-axis vectors used in rotational motion problems:

// enable/disable the Set Z Dir menu invoking button
void CFBDView::OnUpdateZDirMenu(CCmdUI* pCmdUI) 
{
	// only used for certain problems. And of course requires that view is enabled
	if (! (GetDocument()->UseZAxis()) || ! m_bEnabled) {
		pCmdUI->Enable(FALSE);
		return;
	}

	// used in two cases: if drawing a new vector OR 
	//					  if current selection is vector (to change it)
	// Note selected case requires this to be active view, other case doesn't,
	// since can select draw tools while inactive (will activate on mouse down in view)
	// also only certain vector types allowed.
	if (m_drawMode == Vector && m_vectorType != VECTOR_FORCE
		&& m_vectorType != VECTOR_COMPONENT) {
		pCmdUI->Enable(m_bEnabled);
	
	}
	else if (m_bActive && SingleSelection() && 
		SelectedObj()->IsKindOf(RUNTIME_CLASS(CVector))) {
		CVector* pVec = (CVector*) SelectedObj();
		pCmdUI->Enable(pVec->m_nVectorType != VECTOR_FORCE && 
			           pVec->m_nVectorType != VECTOR_COMPONENT &&
					   m_bEnabled);
	}
	else pCmdUI->Enable(FALSE);
}

// For setting zdir: used both to change current selection or set mode for
// next vector drawing
void CFBDView::OnSetZDir(UINT nID)
{
	// map command to direction code
	int nZDir = nID - ID_ZDIR_FIRST;  // command order must be same as ids
	
	// log command
	LogEventf(EV_SET_ZDIR, "%d", nZDir);
	
	// if drawing new vector, set flag to be used when drawing
	if (m_drawMode == Vector) {		
		m_nZDir = nZDir;
	}
	// else if have selected vector, set it's ZDir property
	else if (SingleSelection() && SelectedObj()->IsKindOf(RUNTIME_CLASS(CVector))) {
		CVector* pVec = (CVector*)SelectedObj();
		if (pVec->m_nZDir != nZDir) { // do nothing if no change
			// status is unknown until rechecked
			pVec->m_status = statusUnknown;
			// update Zdir (will invalidate).
			pVec->SetZDir(nZDir);
			// Re-check it with help system after change
			pVec->CheckObject();
		}
	}
}

// Update commands on Z-dir menu
void CFBDView::OnUpdateSetZDir(CCmdUI* pCmdUI)
{
	// translate command to direction code
	int nZDir = pCmdUI->m_nID - ID_ZDIR_FIRST; // // command order same as ids
	
	// see if have selected vector visible
	BOOL bSel = m_bActive  &&
		        SingleSelection() && SelectedObj()->IsKindOf(RUNTIME_CLASS(CVector));
				
	// If in select mode, check property of selected vector
	if (bSel && m_drawMode == Selector) {
		pCmdUI->SetCheck( ((CVector*)SelectedObj())->m_nZDir == nZDir);
	}
	// If in drawing mode, check setting for current drawing
	else if (m_drawMode == Vector)
		pCmdUI->SetCheck(m_nZDir == nZDir);

	// menu cmds always enabled; toolbar button enabling controls menu availability
	pCmdUI->Enable(TRUE);
}

void CFBDView::OnInsertGreekLetter(UINT nID)
{
	const char* alpha = "abgdezhqiklmnxoprstufcyw";//greek translation
	int pos = nID - IDM_GREEKLETTER_FIRST;
	CString strLetter = alpha[pos];
	// insert upper-case letter if SHIFT pressed or CAPSLOCK on
	if (   (::GetKeyState(VK_SHIFT)   & 0x8000)	 // high bit of (promoted) 16-bit result => pressed
		|| (::GetKeyState(VK_CAPITAL) & 0x0001)) // low bit of result => toggle key is "on". 
		strLetter.MakeUpper();

	
	if (m_nLastActive == -1) return;
	CEQEditType* pEdit = (CEQEditType*) GetDlgItem(m_nLastActive);
	ASSERT(pEdit);

	// Insert the greek letter in focus edit.
	// Triggers OnChange, which will update doc and notify other views as
	// for any user change.
	// pEdit->ReplaceSel(strLetter);		
	// pEdit->SetCharSymbol();
	pEdit->InsertGreekText(strLetter);
	pEdit->SetFocus();
}

void CFBDView::OnUpdateGreek(CCmdUI* pCmdUI) 
{
	BOOL bVisible = theApp.GetMainFrame()->GreekMenuIsVisible();
	pCmdUI->SetRadio(bVisible);
	pCmdUI->Enable((bVisible || IsActiveEdit()) && m_bEnabled);
}




