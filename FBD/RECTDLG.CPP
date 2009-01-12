// rectdlg.cpp : implementation file
//
// $Id: RECTDLG.CPP,v 1.2 2005/04/11 18:53:54 anders Exp $
//


#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "rectdlg.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

// define PREVIEW_DOC for test code to show whole doc page in preview wnd
/*
#define PREVIEW_DOC 1
*/
/////////////////////////////////////////////////////////////////////////////
// CRectDlg dialog

CRectDlg::CRectDlg(CDrawRect* pObj /*=NULL*/, CWnd* pParent /*=NULL*/)
	: CPropertyPage(CRectDlg::IDD)
{
	m_bInitialized = FALSE;	// Flag set after controls initialized in OnInitDialog
	m_pObjPreview = NULL;
	
	m_pObj= pObj;
	if (pObj != NULL)		// have object: init data members from its props 
	{
		m_bNoFill = !m_pObj->m_bBrush;
		m_penSize = m_pObj->m_bPen ? m_pObj->m_logpen.lopnWidth.x : 0;
		if (m_pObj->m_logpen.lopnStyle == PS_INSIDEFRAME)
			m_nPenStyle = PEN_SOLID;
		else if (m_pObj->m_logpen.lopnStyle == PS_DOT)
			m_nPenStyle = PEN_DOTTED;
		else if (m_pObj->m_logpen.lopnStyle == PS_DASH)
			m_nPenStyle = PEN_DASHED;
		m_colorFill = m_pObj->m_logbrush.lbColor;
		m_colorPen = m_pObj->m_logpen.lopnColor;

		return;
	}
	// else fall through to initialize to defaults.
	//{{AFX_DATA_INIT(CRectDlg)
	m_bNoFill = FALSE;
	m_penSize = 0;
	m_nPenStyle = -1;
	//}}AFX_DATA_INIT
}
// transfer data member values directly to given obj
void CRectDlg::UpdateObj(CDrawRect* pObj)	
{
	if (pObj == NULL) return;
	
	pObj->m_bBrush = !m_bNoFill;
	pObj->m_bPen = m_penSize > 0;
	if (pObj->m_bPen)
	{
		pObj->m_logpen.lopnWidth.x = m_penSize;
		pObj->m_logpen.lopnWidth.y = m_penSize;
		if (m_nPenStyle == PEN_DASHED) 
			pObj->m_logpen.lopnStyle = PS_DASH;
		else if (m_nPenStyle == PEN_DOTTED) 
			pObj->m_logpen.lopnStyle = PS_DOT;
		else
			pObj->m_logpen.lopnStyle = PS_INSIDEFRAME;
	}
	pObj->m_logbrush.lbColor = m_colorFill;
	pObj->m_logpen.lopnColor = m_colorPen;
	
	// obj is dirty after property change
	pObj->Invalidate();
}

void CRectDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CRectDlg)
	DDX_Control(pDX, IDC_PREVIEW_STATIC, m_stcPreview);
	DDX_Control(pDX, IDC_SPIN1, m_SpinCtrl);
	DDX_Check(pDX, IDC_NOFILL, m_bNoFill);
	DDX_Text(pDX, IDC_WEIGHT, m_penSize);
	DDV_MinMaxUInt(pDX, m_penSize, 0, 100);
	DDX_Radio(pDX, IDC_PEN_STYLE, m_nPenStyle);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CRectDlg, CDialog)
	//{{AFX_MSG_MAP(CRectDlg)
	ON_BN_CLICKED(IDC_FILL_COLOR, OnFillColor)
	ON_BN_CLICKED(IDC_LINE_COLOR, OnLineColor)
	ON_BN_CLICKED(IDC_NOFILL, OnNofill)
	ON_EN_CHANGE(IDC_WEIGHT, OnChangeWeight)
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
	ON_CONTROL_RANGE(BN_CLICKED, IDC_PEN_STYLE, IDC_PEN_DASHED, OnPenStyle)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRectDlg message handlers

BOOL CRectDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

	// Create temp copy of object to use when displaying preview image and
	// set preview control to display its image. (Temp cleaned up on close)
	if (m_pObj) 
	{
#ifndef PREVIEW_DOC
		m_pObjPreview = (CDrawRect*) m_pObj->Clone();

		
		// set preview window to display this clone
		m_stcPreview.SetObj(m_pObjPreview);

#else // PREVIEW_DOC -- test code for previewing whole document
		m_stcPreview.SetDoc(m_pObj->m_pDocument);
#endif // PREVIEW_DOC
	}

	// Init line weight spinner coupled to line weight edit:
	// m_bInitialized flag is used in OnPropChange handler to suppress processing 
	// EN_CHANGE notifications sent from weight edit during creation and
	// programmatic initialization due to spinner initialization:
	m_SpinCtrl.SetRange(0, 100);
	m_SpinCtrl.SetBase(10);
	m_SpinCtrl.SetPos(1);

	// Set default line style:
	GetDlgItem(IDC_WEIGHT)->EnableWindow(m_nPenStyle == PEN_SOLID); 

	m_bInitialized = TRUE;

	return TRUE;  
}

void CRectDlg::OnDestroy() 
{
	// free temp object if created
	if (m_pObjPreview)
		m_pObjPreview->Delete();
	
	CDialog::OnDestroy();
}

// update after change in graphics property value
void CRectDlg::OnPropChange()
{
	// ignore change notifications if fired before programmatic initialization complete
	if (! m_bInitialized) 
		return;

	SetModified(TRUE);	// mark sheet as modified
	UpdatePreview();	// and redraw preview image
}

// Update preview window (via temp obj) on change in currently selected properties
void CRectDlg::UpdatePreview()
{
	// Transfer current prop values from controls into temp drawobj: 
	// First, must sync our data members with values in controls
	UpdateData(TRUE); 
	// Then transfer values from data members into preview object
	UpdateObj(m_pObjPreview);	// safe if NULL
	
	// Force preview window to redraw
	m_stcPreview.Invalidate();
}

// OnOK -- Called for either OK or APPLY button push:
// UpdateData should have been called by framework before this, so member 
// vars reflect current state of controls
void CRectDlg::OnOK()
{
	// Transfer values into underlying object
	UpdateObj(m_pObj);
	// Can't cancel changes anymore
	CancelToClose();	// good thought, but doesn't work due to bug in Windows
	// Base class OnOK will clear modified flag on page
	CPropertyPage::OnOK();
}

// OnApply -- handle apply button push
BOOL CRectDlg::OnApply()
{
	// Base class calls OnOk, which updates underlying obj in doc
	BOOL bSuccess = CPropertyPage::OnApply();
#ifdef PREVIEW_DOC
	if (bSuccess)			// must update preview contents when doc changes
		UpdatePreview();
#endif
	return bSuccess;
}


void CRectDlg::OnFillColor() 
{
	CColorDialog dlg(m_colorFill);
	if (dlg.DoModal() != IDOK)
		return;
	m_colorFill = dlg.GetColor();

	OnPropChange();
}

void CRectDlg::OnLineColor() 
{
	CColorDialog dlg(m_colorPen);
	if (dlg.DoModal() != IDOK)
		return;
	m_colorPen = dlg.GetColor();

	OnPropChange();
}

// OnPenStyle -- Handle click of button in the pen style radio group. 
// Arg is btn control id. not enum value.
// Solid radio button is named IDC_PEN_STYLE since first control id in group 
// represents whole group value for value DDX.
void CRectDlg::OnPenStyle(UINT nId) 
{
	// Pen style other than solid only works with width of one
	if (nId != IDC_PEN_STYLE) {
		m_SpinCtrl.SetPos(1);
	}
	
	// Enable weight edit control iff clicked solid button 
	GetDlgItem(IDC_WEIGHT)->EnableWindow(nId == IDC_PEN_STYLE);

	OnPropChange();
}

// To maintain modified flag for apply processing:
void CRectDlg::OnNofill() 
{
	OnPropChange();
}

void CRectDlg::OnChangeWeight() 
{
	OnPropChange();
}


/////////////////////////////////////////////////////////////////////////////
// CPreviewStc -- subclasses CStatic to show preview image of a CDrawObj
//                or, alternatively, of a whole FBDDoc's graphics.
// 
// !!! If all graphics objects were children of a single root graphics object, say
// a group representing the whole page, then could use the same code for both cases.
//
CPreviewStc::CPreviewStc()
{
	m_pObj = NULL;
	m_pDoc = NULL;
}

// Set the object to display in preview window. 
// Our pointer references read-only drawobj owned by someone else
void CPreviewStc::SetObj(const CDrawObj * pObj)
{
	m_pObj = pObj;
	// UpdateSize();	// change to use MM_ISOTROPIC instead
	// Invalidate();
}

// alternate use is to display a whole document's graphics objects.
void CPreviewStc::SetDoc(const CFBDDoc * pDoc)
{
	m_pDoc = pDoc;
	// UpdateSize(); // change to use MM_ISOTROPIC instead
}

void CPreviewStc::UpdateSize()
{
	// update window's size to ensure correct aspect ratio for object
	// for now, we just set preview wnd to represent whole page. 
	CRect rcWnd;
	GetWindowRect(rcWnd);
	// Adjust width in terms of current height, i.e. as layed out on dialog 
	const double fWidthToHeight = double(PAGEWIDTH) / PAGEHEIGHT;
	SetWindowPos(NULL, 0, 0, int(rcWnd.Height() * fWidthToHeight), rcWnd.Height(), 
		         SWP_NOMOVE | SWP_NOZORDER);
}

BEGIN_MESSAGE_MAP(CPreviewStc, CStatic)
	//{{AFX_MSG_MAP(CPreviewStc)
	ON_WM_PAINT()
	ON_WM_ERASEBKGND()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPreviewStc message handlers

// use solid white background
BOOL CPreviewStc::OnEraseBkgnd(CDC* pDC) 
{
	CRect rcClient;
	GetClientRect(rcClient);
	pDC->FillSolidRect(rcClient, RGB(255, 255, 255));

	return TRUE;
}

void CPreviewStc::OnPaint() 
{
	CPaintDC dcPaint(this); // device context for painting
	
	if (! (m_pObj || m_pDoc)) return;

	// Setup to scale drawing isotropically into this window
	CRect rcClient;
	GetClientRect(rcClient);
	CSize sizeDrawing;
	if (m_pObj) { // showing obj at location within a whole page
		sizeDrawing = CSize(PAGEWIDTH, PAGEHEIGHT);
	} else { // showing whole doc -- just map drawing extent, not whole page
		sizeDrawing = ((CFBDDoc*)m_pDoc)->GetSize(); // fn not declared const
	}
	dcPaint.SetMapMode(MM_ISOTROPIC);
	dcPaint.SetWindowExt(sizeDrawing.cx, sizeDrawing.cy);
	dcPaint.SetViewportExt(rcClient.Width(), rcClient.Height());
	
	if (m_pObj)
		// render single obj into preview window at its page location
		((CDrawObj*)m_pObj)->Draw(&dcPaint);	// cast since Draw not declared const
	else if (m_pDoc) {
		// render whole document into preview window
		// ((CFBDDoc*)m_pDoc)->Draw(&dc);

		// First draw into memory; since scaling bitmap to fit seems to work better
		// than direct drawing w/scaled map mode due to non-uniform text scaling.
		CDC dcMem;
		CBitmap bmp, *pOldBmp; 
		CDC* pDrawDC = &dcPaint;
		if (dcMem.CreateCompatibleDC(&dcPaint) &&
			bmp.CreateCompatibleBitmap(&dcPaint, sizeDrawing.cx, sizeDrawing.cy) ) {
			pOldBmp = dcMem.SelectObject(&bmp);
			pDrawDC = &dcMem;
		}
		
		pDrawDC->FillSolidRect(CRect(0, 0, sizeDrawing.cx, sizeDrawing.cy), RGB(255, 255, 255));
		((CFBDDoc*)m_pDoc)->Draw(pDrawDC);
	
		// If drew offscreen, blit drawing from memory onto the screen
		if (pDrawDC != &dcPaint) {
			dcPaint.BitBlt(0, 0,  sizeDrawing.cx, sizeDrawing.cy, &dcMem, 0, 0,SRCCOPY);
			dcMem.SelectObject(pOldBmp);
		}
	}
	
	// Do not call CStatic::OnPaint() for painting messages
}
