// 
// EQView.cpp : implementation of the CEQView class
// ANDES Equation view
//
// Manages an array of equation edit controls in a dialog form
//

#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "FBDDoc.h"
#include "EQView.h"
#include "EQEdit.h"
#include "Mainfrm.h"
#include "ChildFrm.h"
#include "HelpIfc.h"
#include "HlpOpDlg.h"
#include "SolveVarDlg.h"
#include "VarView.h"
#include "SymbolMenu.h"
#include "MyMenu.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CEQView

IMPLEMENT_DYNCREATE(CEQView, CFormView)

BEGIN_MESSAGE_MAP(CEQView, CFormView)
	ON_WM_CONTEXTMENU()
	//{{AFX_MSG_MAP(CEQView)
	ON_BN_CLICKED(IDOK, OnEnter)
	ON_COMMAND(ID_HELP_WHATSWRONG, OnHelpWhatswrong)
	ON_UPDATE_COMMAND_UI(ID_HELP_WHATSWRONG, OnUpdateHelpWhatswrong)
	ON_BN_CLICKED(ID_CALCULATOR, OnCalculator)
	ON_UPDATE_COMMAND_UI(ID_EDIT_COPY, OnUpdateEditCopyCut)
	ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE, OnUpdateEditPaste)
	ON_UPDATE_COMMAND_UI(ID_EDIT_UNDO, OnUpdateEditUndo)
	ON_COMMAND(ID_EDIT_COPY, OnEditCopy)
	ON_COMMAND(ID_EDIT_CUT, OnEditCut)
	ON_COMMAND(ID_EDIT_PASTE, OnEditPaste)
	ON_COMMAND(ID_EDIT_UNDO, OnEditUndo)
	ON_COMMAND(ID_EDIT_DELETE, OnEditDelete)
	ON_WM_SIZE()
	ON_COMMAND(ID_CALCULATE, OnCalculate)
	ON_UPDATE_COMMAND_UI(ID_CALCULATE, OnUpdateCalculate)
	ON_COMMAND(ID_SOLVEFOR, OnSolvefor)
	ON_UPDATE_COMMAND_UI(ID_SOLVEFOR, OnUpdateSolvefor)
	ON_UPDATE_COMMAND_UI(ID_SOLVEFOR_MENU, OnUpdateSolveforMenu)
	ON_UPDATE_COMMAND_UI(ID_EDIT_CUT, OnUpdateEditCopyCut)
	ON_UPDATE_COMMAND_UI(ID_EDIT_DELETE, OnUpdateEditCopyCut)
	ON_WM_ACTIVATE()
	ON_UPDATE_COMMAND_UI(ID_GREEK, OnUpdateGreek)
	//}}AFX_MSG_MAP
	ON_COMMAND_RANGE(IDM_GREEKLETTER_FIRST, IDM_GREEKLETTER_LAST, OnInsertGreekLetter)
	ON_COMMAND_RANGE(ID_SOLVEFOR_FIRST, ID_SOLVEFOR_LAST, OnSolveForNth)
	// To catch events for each member of equation array:
	// ClassWizard doesn't manage mapping ranges of controls. 
	ON_CONTROL_RANGE(EN_CHANGE, FIRST_EQ_ID, LAST_EQ_ID, OnChange)
	ON_CONTROL_RANGE(EN_SETFOCUS, FIRST_EQ_ID, LAST_EQ_ID, OnFocus)
	ON_CONTROL_RANGE(EN_KILLFOCUS, FIRST_EQ_ID, LAST_EQ_ID, OnKillFocus)
/* don't implement print commands in equation view; diagram view prints everything.
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, CFormView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CFormView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CFormView::OnFilePrintPreview) */
	// Participate in idle-time command ui updating
	ON_MESSAGE(WM_IDLEUPDATECMDUI, OnIdleUpdateCmdUI)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEQView construction/destruction

CEQView::CEQView()
	: CFormView(CEQView::IDD)
{
	//{{AFX_DATA_INIT(CEQView)
	//}}AFX_DATA_INIT

	m_nLastActive = -1;
	m_bIgnoreChange = FALSE;
	m_bEnabled = TRUE;
}

CEQView::~CEQView()
{
	// nothing to do here
}

void CEQView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CEQView)
//	DDX_Control(pDX, IDC_GREEK, m_btnGreek);
	//}}AFX_DATA_MAP
	
	// DDX mechanism not used, we update on change notifications.
}

// 
// OnInitialUpdate: Load contents of edit controls and  initial status 
//                  from document data.
//	
// Note that setting edit control text generates an EN_CHANGE event back to
// parent. Normally we catch this and our handler sets status to unknown since
// student has changed text. When setting status programmatically from
// saved state in document we set the m_bIgnoreChange flag around the change
// to block this. SyncEqCtl does the work:
//
void CEQView:: SyncEqCtl(UINT nEq)
{
	// Set text and status color: Need to set flag to suppress processing
	// of EN_CHANGE notification sent from the control on text change.
	BOOL bPrevFlag = m_bIgnoreChange;
	m_bIgnoreChange = TRUE; 
	
	m_Edit[nEq].SetRichEditText(GetDocument()->m_strEq[nEq] );
	// Set control's color to display status from document.
	SetCtlStatus( EQ_ID(nEq), GetEqStatus(nEq) );
	
	m_bIgnoreChange = bPrevFlag; 
}

void CEQView::OnInitialUpdate() 
{
	CFormView::OnInitialUpdate();
	
	// Initialize edit controls and status from document data
	CFBDDoc* pDoc = GetDocument();
	for (int i = 0; i < NUM_EQS; ++i) 
	{
#ifndef EQ_RICHEDIT  // following is code to use normal edit controls

		// Subclass each edit ctrl on the form with one of our own
		m_Edit[i].SubclassDlgItem( EQ_ID(i), this);

#else // RICHED: replace existing edit control on form with our richedit derivative
		CWnd* pCtrl = GetDlgItem( EQ_ID(i) );
		// grab styles of existing control
		DWORD dwStyles = ::GetWindowLong(pCtrl->m_hWnd, GWL_STYLE);
		// get the existing control's position on the form
		CRect rcEdit;
		pCtrl->GetWindowRect(rcEdit);
		ScreenToClient(rcEdit);
		// get rid of existing control
		pCtrl->DestroyWindow();
		// Create new rich edit w/same style, position, & id
		m_Edit[i].Create(dwStyles, rcEdit, this, EQ_ID(i));
		// set it to use the dialog's font
		m_Edit[i].SetFont(GetFont());
		
		// Hack -- our subclassed  ctl requires text be protected. It traps protected
		// text change notifications to detect format-only changes and eat the 
		// EN_CHANGE notifications for format-only changes. 
		CHARFORMAT cfDefault;
		cfDefault.cbSize = sizeof(cfDefault);
		cfDefault.dwEffects = CFE_PROTECTED; 
		cfDefault.dwMask = CFM_PROTECTED;
		m_Edit[i].SetDefaultCharFormat(cfDefault);

		// Set up to get change notifications. Must OR in ENM_PROTECTED since 
		// control hack requires it. 
		m_Edit[i].SetEventMask(ENM_CHANGE|ENM_PROTECTED);

#endif EQ_RICHEDIT
		// Set custom edit control to show delegate context menu msg to us
		m_Edit[i].m_bParentMenu = TRUE;

		// And initialize the control from doc data
		SyncEqCtl(i);
	}
}

//
// Update document data with equation values and status from all edit controls
// Used before serializing document data on save. Not needed anymore since we 
// changed to update document data after every change. Now just verifies that
// we have correctly sync'ed the state.
//
void CEQView::UpdateDoc()
{
	CFBDDoc* pDoc = GetDocument();
	for (int i = 0; i < NUM_EQS; i++) {
		CString strEq;
		m_Edit[i].GetRichEditText(strEq); 
		
		ASSERT(pDoc->m_strEq[i] == strEq);
		// pDoc->m_statusEq[i] = GetEqStatus(i);
	}
}

//
// Handle update hints broadcast on document changes
void CEQView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	// ignore update classes that don't affect us at all
	if (IS_DIAGRAM_UPDATE(lHint) || IS_PLAN_UPDATE(lHint))
		return;

	// On change in equation from some other view
	if (lHint == HINT_UPDATE_EQUATION) { 
		int nEq = (int) pHint;	// hint is  eq num
		if (! IS_EQ_INDEX(nEq)) return;
		
		// synch text and status in this view with document data
		SyncEqCtl(nEq);
		return;
	};

	// On change into/out of tutor mode
	if (lHint == HINT_TUTOR_MODE) 
		EnablePane(! DWORD(pHint));
		
	// else default processing:
	CFormView::OnUpdate(pSender, lHint, pHint);
}

//
// Visibly Enable/Disable whole pane
//
void CEQView::EnablePane(BOOL bEnable)
{
	// enable view window appropriately
	EnableWindow(bEnable);

	// enable child equation controls and adjust background color to show
	// (richedits don't grey themselves when disabled.)
	COLORREF crBackground = ::GetSysColor(bEnable ? COLOR_WINDOW : COLOR_3DFACE) ;
	for (int i = 0; i < NUM_EQS; i++) {
		m_Edit[i].EnableWindow(bEnable);
		m_Edit[i].SetBkColor(crBackground);
	}
	// remember enabled state for easy access in command enabling
	m_bEnabled = bEnable;
}

///////////////////////////////////////////////////////////////////////
// For managing status color for child equation controls
///////////////////////////////////////////////////////////////////////
		
// Helper to get equation status from document by index
const Status CEQView::GetEqStatus(UINT nEq)
{ 
	ASSERT(IS_EQ_INDEX(nEq)); 
	return GetDocument()->m_statusEq[nEq]; 
}

//  
// GetCtlStatus -- return eq status given eq control id.
//
Status CEQView::GetCtlStatus(int idEq)
{
	ASSERT(IS_EQ_ID(idEq));
	// now should be the value stored in document
	return GetEqStatus(EQ_INDEX(idEq));
}

//
// SetEqStatus -- update eq status in document and in this view's controls
//
void CEQView::SetEqStatus(UINT nEq, Status statusNew)
{
	ASSERT(IS_EQ_INDEX(nEq));
	// TRACE("Setting EQ %d status to %d\n", nEq, (int) statusNew);

	// See if this is really a change
	Status statusOld = GetEqStatus(nEq);
	if (statusOld != statusNew) 
	{	
		// Trace status changes in log 
		LogEventf(EV_EQ_STATUS, "%d %d", nEq, (int) statusNew);

		// Update saved status in document
		GetDocument()->m_statusEq[nEq] = statusNew;
		// Doc dirty on status change
		GetDocument()->SetModifiedFlag();

		// No need to broadcast update hint on status change since we are currently 
		// only view that presents eq status, and we update ourselves here.
	}

	// Update control's color in any case to ensure. This handles obscure case where
	// text change was due to paste colored text from clipboard while status 
	// is recorded as unknown -- still must recolor text though status unchanged.
	SetCtlStatus(EQ_ID(nEq), statusNew);
}

// 
// SetCtlStatus -- update given control's color to display status
// 
void CEQView::SetCtlStatus (int idEq, Status statusNew)
{
	ASSERT(IS_EQ_ID(idEq));
	int nEq = EQ_INDEX(idEq);

#ifdef EQ_RICHEDIT	// In richedits, color change causes EN_CHANGE to be sent
		BOOL bPrevFlag = m_bIgnoreChange;
		m_bIgnoreChange = TRUE;
#endif 
		if (statusNew == statusCorrect)
			m_Edit[nEq].SetTextColor(RGB(0, 128, 0));
		else if (statusNew == statusError)
			m_Edit[nEq].SetTextColor(RGB(255, 0, 0));
		else
			m_Edit[nEq].SetTextColor(RGB(0, 0, 0));
		
#ifdef EQ_RICHEDIT	
		m_bIgnoreChange = bPrevFlag;
#endif 
}

#if 0 // Now unused since coloring is handled in subclassed control
// 
// Handle query from child edit control to set color of its text when 
// painting 
//
HBRUSH CEQView::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	// get the default drawing brush from base class routine
	HBRUSH hbr = CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
	
	// make sure message is from one of our equation edit controls
	if (! IS_EQ_ID(pWnd->GetDlgCtrlID()) )
		return hbr;

	int id = pWnd->GetDlgCtrlID();
	if (! IS_EQ_ID(id))			
		return hbr;

	// Set appropriate text foreground color
	switch (GetCtlStatus(id))
	{
	case statusCorrect:
		pDC->SetTextColor(RGB(0, 128, 0));	// 
		break;

	case statusError:
		pDC->SetTextColor(RGB(255, 0, 0));  // red
		break;

	default:
		TRACE("OnCtlColor: Bad status value, id = %d\n", id);
		// fall through ...
	case statusUnknown:
		break;								// leave in default color
	}

	// Routine can return a different brush if the 
	// default is not desired. But we did our work with SetTextColor
	return hbr;
}
#endif 0

/////////////////////////////////////////////////////////////////////////////
// CEQView diagnostics

#ifdef _DEBUG
void CEQView::AssertValid() const
{
	CFormView::AssertValid();
}

void CEQView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CFBDDoc* CEQView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return (CFBDDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CEQView message handlers

/* obsolete
void CEQView::OnEditClearAll() 
{
	// reset all data members: 
	for (int i = 0; i < NUM_EQS; i++) {
		m_Edit[i].SetWindowText("");
		m_statusEq[i] = statusUnknown;
	}
}
*/

// participate in idle-time command ui updating:
LRESULT CEQView::OnIdleUpdateCmdUI(WPARAM wParam, LPARAM lParam)
{
	// No longer needed since we removed command buttons from form
	// UpdateDialogControls(AfxGetMainWnd(), TRUE);
	return 0L;
}

///////////////////////////////////////////////////////////////
// Equation entry checking.
////////////////////////////////////////////////////////////////

// Issue: What is the event that encodes the user "submitting"
// an entry for checking? We need this so we can do line by line validation 
// prior to what is normally submission of whole form. 
//
// For now we do it on user hitting ENTER key. The standard dialog keyboard interface
// translates this to a push of the default button, normally IDOK, or an IDOK command
// if none, to submit the whole form.  Note user can hit return with the focus
// outside of an equation entry control so we must check where the focus is 
// before treating this event as a command to submit.
//
// User can also Tab or mouse in and out of edit fields w/o hitting return key.
// It would be possibileis to catch the KILLFOCUS event and 
// treat it as occasion for validation, as in an MFC sample program FCSVAL. 
// But for now we require an explicit ENTER to submit data for checking.
// We could also add a visible "submit" button but this is more natural.

//
// OnEqResult: callback function for handling asynchronous DDE completions.
//
// Note because it is a callback, it can't be a non-static member fn, which
// requires an implicit "this" argument.
// 
// EQCTX: Equation query context record. Pointer is passed to async 
// completion routine. Packages the info the callback needs to process result.
//
typedef struct 
{
	CEQView* pView;				// The view that generated the query
	int		 id;				// equation control ID
} EQCTX;


// Helper parses possibly complex status result from help system, setting appropriate 
// status to equation and executing any attached command.
void CEQView::ApplyStatusResult(LPCTSTR pszResult, UINT nEq)
{
	CString strReturn, strCommand;
	// Note following handles NULL return on error.
	CCheckedObj::SplitResult(pszResult, strReturn, strCommand);
   
   	// set status from return value
	if (strReturn.IsEmpty())
		SetEqStatus(nEq, statusUnknown);
	else if (strReturn == "T") {
		SetEqStatus(nEq, statusCorrect);
		theApp.MakeSound(sndCorrectEntry);
	}
	else {
		SetEqStatus(nEq, statusError);
		theApp.MakeSound(sndErrorEntry);
	}

	// Notify other views of change in equation
	GetDocument()->UpdateAllViews(this, HINT_UPDATE_EQUATION, (CObject*)nEq);
   	
	// if result includes piggybacked command (normally to show an error message),
	// pass it to application's command interpreter
   	if (!strCommand.IsEmpty()) 
   		ExecuteCmd(strCommand);
}

//
// CheckEquation: Verify an equation with the help system. Used after submission
//                of result of an "editing session".
//
// idEq -- control ID of equation edit box 
// 
void CEQView::CheckEquation(UINT idEq)
{
	ASSERT(IS_EQ_ID(idEq));
	int nEq = EQ_INDEX(idEq);

	// If equation checking disabled, just set to unknown
	// (may not be unknown, since can hit Ener to recheck a red equation, say).
	if (!theApp.CheckEquations()) {	
		SetCtlStatus(idEq, statusUnknown);
		return;
	}

	// retrieve equation string from control
	CString strEq = GetEquation(nEq);
#if 0 // Skip because handled on Lisp side
	// Equation argument in helpsys command should be Lisp-readable string. Escape
	// any embedded quotation marks. 
	// strEq.Replace("\"", "\\\""); // quote to backslash+quote
#endif 0
	// Note: We once tried to optimize by not checking equation if it had not changed
	// since last check. But we took that out to allow resubmission to recheck unchanged 
	// equation after defining variables, since new defs may affect eq status.
	
	// Else check equation by calling help server
	if (theApp.m_nFeedback == FEEDBACK_WAIT) // Synchronous communication
	{
		LPCTSTR pszResult = HelpSystemExecf("(lookup-eqn-string \"%s\" %d)", strEq, nEq);
		
		// set status for coloring
		ApplyStatusResult(pszResult, nEq);
	} 
	else  // queue request using async communication
	{
		// Do nothing if simulating help system from log file -- async reply event
		// will be played back later. (Prevents allocating memory for context record, 
		// which would never get freed during log playback).
		if (LogPlayerInPlayback() && ! LogPlayerCallHelp() ) {
			// need following since submission clears any current hint, 
			// changing hint command enabling state.
			theApp.GetMainFrame()->OnDdeSend();
			return;
		}

		// !!! We might have other pending requests to check this item.
		// New data should really cancel them, either here or on the
		// server side. For now we just pump new requests into the queue.
		// In practice results now nearly instantaneous so not an issue.
		CString strCmd;
		strCmd.Format("(lookup-eqn-string \"%s\" %d)", strEq, nEq);
		
		// package up context record for completion function arg
		EQCTX* pCtx = new EQCTX; 
		pCtx->pView = this;
		pCtx->id = idEq;
		HelpSystemExecAsync(strCmd, CEQView::OnEqResult, (DWORD) pCtx);
	}
}

//
// OnEqResult: Callback to handle async validation completion: 
// arg is ptr to EQSTATE context record 
//
void CEQView::OnEqResult (DWORD dwContext, LPCTSTR pszResult)	
{
	// unpack arg to restore context
	EQCTX* pCtx = (EQCTX *) dwContext;
	CEQView* pView = pCtx->pView;
	int idEq = pCtx->id;
	delete pCtx; 			// Now done with it.

#if 0 // harder to tell if there are two current eq views
	// Make sure the view of the query is still valid!
	if (pView != theApp.GetEQView()) {
		TRACE("OnEqResult completion: XACT not for current view; ignored\n");
		return;
	}
#endif 0
	ASSERT(IS_EQ_ID(idEq));
	int nEq = EQ_INDEX(idEq);

	// log the async input event now. Lot entry shows remote errors as no-op empty
	// strings. Don't normally get empty string return on equations so will usually
	// indicate error. Though may get success w/empty string on comment equation lines.
	TRACE("Got DDE reply EQ %d: |%s|\n", nEq, pszResult ? pszResult: "<FAILED>");
	LogEventf(EV_EQ_RESULT, "%d %s", nEq, pszResult ? pszResult: "");
	
	// set status flag for coloring. Safe if pszResult is NULL for error.
	pView->ApplyStatusResult(pszResult, nEq);
}

//
// Handle change in equation text:
// 
void CEQView::OnChange(UINT idEq)
{
	// Do nothing if flag set to ignore changes.
	// This is set when view is initializing the controls from document
	if (m_bIgnoreChange) 
		return; 

	ASSERT(IS_EQ_ID(idEq));
	int nEq = EQ_INDEX(idEq);
	CString strEq = GetEquation(nEq);
	
	// We now sync the document contents with the control's on every text change.
	// Do nothing if text has not changed from last stored in document
	if (strEq == GetDocument()->m_strEq[nEq])
		return;
	// else text has really changed

	// Log the change. 
	LogEventf(EV_EQ_CHANGE, "%d %s", nEq, (LPCSTR) strEq); 

	// The document data should reflect state of controls even if text is not 
	// yet submitted for checking, so update and mark it dirty now
	GetDocument()->m_strEq[nEq] = strEq;
	GetDocument()->SetModifiedFlag();
	
	// Change equation status to unknown until submitted again
	SetEqStatus(nEq, statusUnknown);

	// Notify other views of change in equation text
	GetDocument()->UpdateAllViews(this, HINT_UPDATE_EQUATION, (CObject*)nEq);

#if 0 // following is more correct:
	// On first change since focussed only, clear any existing equation entry 
	// in helpsys, since it is now invalid. Don't need to do if helpsys
	// doesn't hold an earlier entry, but simplest to do every first change.
	// This should supersede use of "DELETE" notifications on killfocus.
	if (m_bFirstChange) 
	{
		// Put a log message in to distinguish this call?
		// suppress clearing of hint on this call. Can happen on focus changes w/o submit
		BOOL bPrev = theApp.GetMainFrame()->SetClearHintOnDde(FALSE);
		HelpSystemSendf("(lookup-eqn-string \"\" %d)", nEq);
		theApp.GetMainFrame()->SetClearHintOnDde(bPrev);
		
		m_bFirstChange = FALSE;
	}
#endif 0

}

// 
// Handle changes in focus. We treat setting focus to an equation as the
// beginning of an "editing session" with the equation. As soon as contents change, we 
// set the status to unknown until it is submitted again. We allow user to kill focus 
// by mousing into another area without submitting the edits. The in-process
// session is resumed by setting the focus again.
// 
// Definitions:
// The "Active" equation is the one that currently has the focus; may be none (-1).
// The "Current" edit control =  most recent Active eq (last one that had the focus). 
// It persists as the current eq even if it loses focus (e.g. if user mouses into 
// another view) and only changes when a new one actually gets the focus. After
// user focuses in eq once, there should always be a "Current" eq. the current
// one is the target of toolbar commands like InsertGreekLetter, since user must
// mouse out of equation (killing focus) to select them.
//
void CEQView::OnFocus(UINT idEq)	// traps setting of focus to an eq	
{
	ASSERT(IS_EQ_ID(idEq));
	int nEq = EQ_INDEX(idEq);

	// Update "Current" edit control.
	m_nLastActive = nEq;

	// remember if it started this "editing session" non-blank, for detecting 
	// true deletions on killfocus event below.
	m_bWasNonBlank = ! IsBlank(nEq);
#if 0
	// set up for first change processing:
	m_bFirstChange = TRUE;
#endif 0
	
	// Log the focus event 
	LogEventf(EV_EQ_FOCUS, "%d", nEq);
}

void CEQView::OnKillFocus(UINT idEq)	// traps loss of focus by an eq
{
	ASSERT(IS_EQ_ID(idEq));
	int nEq = EQ_INDEX(idEq);
	
	// Log killfocus event
	LogEventf(EV_EQ_KILLFOCUS, "%d", nEq);

	// !!! Should clear "current" eq member? For now, persists until new one set.
	// Possibly need a different member for focus eq which is cleared here.

	// Normally changes to equation text are only submitted to help system for checking
	// when user hits enter. But in special case where equation has gone from non-blank
	// to empty, we notify help system of "deletion" on killfocus. This is because, in 
	// the absence of any text to color, there is no visible feedback to the student 
	// to let them see whether this change has been submitted or not, so they could easily forget.
	if (IsBlank(nEq) && m_bWasNonBlank) 		
	{
		// Result unused.  Means status is not changed but that should be OK.
		LogEventf(EV_EQ_DELETE, "%d", nEq);		// log for replay of helpsys call
		// suppress clearing of hint on this call. Can happen on focus changes w/o submit
		BOOL bPrev = theApp.GetMainFrame()->SetClearHintOnDde(FALSE);
		HelpSystemSendf("(lookup-eqn-string \"\" %d)", nEq);
		theApp.GetMainFrame()->SetClearHintOnDde(bPrev);
	}
}

//
// OnEnter: Handles IDOK command sent by hitting return key.
// We treat this as a "submit" command.
//
void CEQView::OnEnter() 
{
	// Get ID of focus control and make sure its one of our equation edits
	CWnd* pWndCtrl = GetFocus();
	if (pWndCtrl == NULL) 
		return;
	int idCtrl = pWndCtrl->GetDlgCtrlID();
	if (! IS_EQ_ID(idCtrl) ) 
		return;
	int nEq = EQ_INDEX(idCtrl);

	// Log the submit event
	LogEventf(EV_EQ_SUBMIT, "%d", nEq);	

	// Initiate check for the new entry. (Updates control status if synchronous).
	CheckEquation(idCtrl);

	// In sync mode, have new status now:
	// if no error, advance the focus to next in tab order, else leave it here
	// in async mode, always just advance (shows them they've entered it). 
	if (theApp.m_nFeedback == FEEDBACK_ASYNC || GetEqStatus(nEq) != statusError ) {
		CWnd* pWndCtrlNext = GetNextDlgTabItem(pWndCtrl);
		ASSERT(pWndCtrlNext != NULL);
		pWndCtrlNext->SetFocus();
	}
}

// Helper returns index of equation that currently has input focus
// Returns -1 if none.
int CEQView::GetActiveEq()
{
	CWnd* pWndCtrl = GetFocus();
	if (pWndCtrl == NULL)
		return -1;
	int idCtrl = pWndCtrl->GetDlgCtrlID();
	return IS_EQ_ID(idCtrl) ? EQ_INDEX(idCtrl) : -1;
}

//
// Commands for getting help on errors
//
void CEQView::OnHelpWhatswrong() 
{
	int nEq = GetActiveEq();
	if (nEq < 0) 
		return;
	ASSERT(IS_EQ_INDEX(nEq));

	LogEventf(EV_EQ_WHATSWRONG, "%d", nEq);	// eq num for info, checking.
	DoHelpWhatswrong(nEq);
	
}

void CEQView::DoHelpWhatswrong(int nEq)
{
	LPCTSTR pszResult = HelpSystemExecf("(Why-wrong-equation %d) ", nEq);
	// Ask frame to show result in hint window
	theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong);
}

void CEQView::OnUpdateHelpWhatswrong(CCmdUI* pCmdUI) 
{
	int nEq = GetActiveEq();
	pCmdUI->Enable(nEq >= 0  && GetEqStatus(nEq) == statusError && m_bEnabled
		           && (theApp.m_wHelpFlags & fWhatsWrong));
}

// Command routing detail: We want the calculator command accessible both from a 
// child button on the EQView form, but also from the Mainframe's menu and/or toolbar, so 
// student can use it anytime.  If chosen from the frame's menu/toolbar, msg goes to the 
// frame first and command routing sends it down along the usual path. That gives this view 
// first crack it, but only  if it is the active view; otherwise must be handled in the frame.
// However if chosen from our child button, command msg goes directly to the View for
// routing, and view *doesn't* send it up to any containing frame. (The view can't route a
// command message up to the frame without causing an endless loop in the case where cmd 
// came down from the menu.) The upshot is we must catch it in both places.
//
void CEQView::OnCalculator() 
{
	// delegate to mainframe's implementation
	CMainFrame* pFrame = theApp.GetMainFrame();
	if (pFrame) pFrame->BringUpCalculator();
}

//
// Public Equation control array operations:
//
// These are not all needed since we changed to sync equation state in the document:
// now better to look in document instead.
//
const CString CEQView::GetEquation(UINT nEq) 
{
	CString strEq;	
	
	ASSERT(IS_EQ_INDEX(nEq));
	if (m_Edit[nEq].IsKindOf(RUNTIME_CLASS(CRichEditCtrl))){
		m_Edit[nEq].GetRichEditText(strEq);
	} else
		m_Edit[nEq].GetWindowText(strEq);
	
	return strEq;	// return by value makes temp clone.
}

void CEQView::DeleteEquation(UINT nEq)
{
	ASSERT(IS_EQ_INDEX(nEq));
	m_Edit[nEq].SetSel(0, -1);
	m_Edit[nEq].Clear();
}

void CEQView::SelectEquation(UINT nEq)
{
	ASSERT(IS_EQ_INDEX(nEq));
	m_Edit[nEq].SetSel(0, -1);
	m_Edit[nEq].SetFocus();
}

BOOL CEQView::IsBlank(UINT nEq)
{
	ASSERT(IS_EQ_INDEX(nEq));
	CString strEq;
	m_Edit[nEq].GetWindowText(strEq);	// untagged text should be sufficient
	strEq.TrimLeft();	// strip leading whitespace and see if anything left
 	return strEq.IsEmpty();
}

//
// Clipboard command handlers. These were inserted by the Component
// Gallery "Clipboard Assistant" mini-wizard. They contain a standard implementation
// that works for any view that contains child edit controls, by delegating
// the appropriate command to the edit control with the focus. 
//
// Note: although CRichEditCtrl is not a subclass of CEdit, this code should still work 
// even for our richedits because the CEdit methods are all simply inline wrappers that 
// send edit control messages to the child window, and the richedit controls support the 
// standard edit control messages.
//

//
// Helper tests if generic CWnd is an edit control. 
// This code is generic, our app could just test child window id instead. But
// good to have the general code in case needed elsewhere.
//
BOOL CEQView::IsEditCtrl(CWnd* pWnd)
{
	// CG: This function was added by the Clipboard Assistant component
	ASSERT_VALID(pWnd);

	TCHAR lpClassName[32];
	::GetClassName(pWnd->GetSafeHwnd(), lpClassName, 32);

	if (!_tcscmp(lpClassName, _T("Edit")) ||
		!_tcscmp(lpClassName, _T("RICHEDIT")) ) // changed to test for riched as well
		return TRUE;

	return FALSE;
}

void CEQView::OnUpdateEditCopyCut(CCmdUI* pCmdUI)
{
	CWnd* pWnd = CWnd::GetFocus();
	if (pWnd && IsEditCtrl(pWnd))
	{
		int nStart, nEnd;
		((CEdit*)pWnd)->GetSel(nStart, nEnd);
		pCmdUI->Enable(nStart != nEnd);
		return;
	}
	pCmdUI->Enable(FALSE);
}

void CEQView::OnUpdateEditPaste(CCmdUI* pCmdUI)
{
	CWnd* pWnd = CWnd::GetFocus();
	if (pWnd && IsEditCtrl(pWnd))
	{
		pCmdUI->Enable(::IsClipboardFormatAvailable(CF_TEXT));
		return;
	}
	pCmdUI->Enable(FALSE);
}

void CEQView::OnUpdateEditUndo(CCmdUI* pCmdUI)
{
	CWnd* pWnd = CWnd::GetFocus();
	if (pWnd && IsEditCtrl(pWnd))
	{
		pCmdUI->Enable(((CEdit*)pWnd)->CanUndo());
		return;
	}
	pCmdUI->Enable(FALSE);
}

void CEQView::OnEditCopy()
{
	CEdit* pEdit = (CEdit*)CWnd::GetFocus();
	pEdit->Copy();
}

void CEQView::OnEditCut()
{
	CEdit* pEdit = (CEdit*)CWnd::GetFocus();
	pEdit->Cut();
}

void CEQView::OnEditPaste()
{
	CEdit* pEdit = (CEdit*)CWnd::GetFocus();
	pEdit->Paste();
}

void CEQView::OnEditUndo()
{
	CEdit* pEdit = (CEdit*)CWnd::GetFocus();
	pEdit->Undo();
}

//
// Following not added by clipboard assistant, Provides handler for our ID_EDIT_DELETE
// command by delegating to the edit control's Clear method. 
// This allows invocation when editing via our mainframe's keyboard accelerator 
// for it (Delete key). Enabled if there is a selection via OnUpdateEditCopyCut
//
void CEQView::OnEditDelete() 
{
	CEdit* pEdit = (CEdit*)CWnd::GetFocus();
	pEdit->Clear();
}

// Handle WM_CONTEXTMENU message by showing context menu. Right button up is
// translated like an accelerator into this by magic built into Windows 95. 
// The component gallery adds a translator for the "standard" Shift-F10 accelerator
// (does anyone know this?) and the context-menu key.
//
// When edit controls have the focus, they get the WM_CONTEXTMENU and normally show
// their built-in context menu, pre-empting the view's handler. We set our subclassed
// edits to show our menu instead, which includes our standard editing commands.
// They also process right-button down and grab focus, so right-click for context menu
// functions like a select.
void CEQView::OnContextMenu(CWnd*, CPoint point)
{
	// CG: This function was added by the Pop-up Menu component
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_EQVIEW));

	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	// Find SolveFor's pull-right submenu to subclass it for Greek letter drawing
	CMenu* pVarMenu = NULL;
	// CMenu* pVarMenu = pPopup->GetSubMenu(4); //hard-coded position for testing
	UINT nItems = pPopup->GetMenuItemCount();
	for (int iItem = 0; iItem < (int)nItems; iItem++){
		CMenu* pSubMenu = pPopup->GetSubMenu(iItem);
		if (pSubMenu &&
			(pSubMenu->GetMenuItemID(0) == ID_SOLVEFOR_MENU
			|| pSubMenu->GetMenuItemID(0) == ID_SOLVEFOR_FIRST) ) {
			pVarMenu = pSubMenu;
			break;
		}
	}

	CGreekMenu mnuGreek;
	if (pVarMenu)
		mnuGreek.Attach(pVarMenu->GetSafeHmenu());

	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);
}

BOOL CEQView::PreTranslateMessage(MSG* pMsg)
{
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

	return CFormView::PreTranslateMessage(pMsg);
}


// Handle command to insert greek letter from palette
// command code defines letter.
void CEQView::OnInsertGreekLetter(UINT nID)
{
	CString alpha = "abgdezhqiklmnxoprstujcyw";//greek translation
	int pos = nID - IDM_GREEKLETTER_FIRST;
	CString strLetter = alpha[pos];
	// insert upper-case letter if SHIFT pressed or CAPSLOCK on
	if (   (::GetKeyState(VK_SHIFT)   & 0x8000)	 // high bit of (promoted) 16-bit result => pressed
		|| (::GetKeyState(VK_CAPITAL) & 0x0001)) // low bit of result => toggle key is "on". 
		strLetter.MakeUpper();

	ASSERT(m_nLastActive != -1);
	ASSERT(IS_EQ_INDEX(m_nLastActive));
	CEQRichEdit* pEdit = &m_Edit[m_nLastActive];
	if (m_Edit[m_nLastActive].IsKindOf(RUNTIME_CLASS(CRichEditCtrl)))
	{
		// Programmatic change triggers OnChange, which will update doc and notify 
		// other views just as for any user change.
		pEdit->InsertGreekText(strLetter);	
		pEdit->SetFocus();
	}
}

// enable command to popup Greek palette
void CEQView::OnUpdateGreek(CCmdUI* pCmdUI) 
{
	BOOL bVisible = theApp.GetMainFrame()->GreekMenuIsVisible();
	pCmdUI->SetRadio(bVisible);
	pCmdUI->Enable((bVisible || m_nLastActive != -1) && m_bEnabled);
}

void CEQView::OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView) 
{
	// Take down Greek letter palette when view loses activation
	if (!bActivate){
		theApp.GetMainFrame()->HideGreekMenu();
	}
	CFormView::OnActivateView(bActivate, pActivateView, pDeactiveView);
}

//
// Find next unused (blank) equation field after given one (inclusive).
// Default if no arg starts from top. Returns index; -1 if none.
//
int CEQView::GetNextEqSlot(int nStart /*= 0 */)
{
	for (int i = nStart; i < NUM_EQS; ++i) {
		if (IsBlank(i))	
			return i;
	}
	return -1;		// all equation slots filled!
}

//
// Command to simplify current equation and add simplified version to
// equation list. Implemented in the help system.
//
void CEQView::OnCalculate() 
{
	if (m_nLastActive == -1) return;
	
	// get current eq, saving over possible changes due to help calls below
	ASSERT(IS_EQ_INDEX(m_nLastActive));
	int nEq = m_nLastActive;			

	// Log the command
	LogEventf(EV_EQ_CALCULATE, "%d", nEq);	// index for readability, checking
	
	// Ensure focus is in "Current" edit, so user can see which it is
	m_Edit[nEq].SetFocus();
	
	// Fetch current equation contents 
	CString strEq = GetEquation(nEq);
	
	// Get slot to use for simplified equation
	int nNextSlot = GetNextEqSlot(nEq);
	if (nNextSlot == -1)					// no room for more equations
		return;	
	ASSERT(IS_EQ_INDEX(nNextSlot));

	// Submit equation for checking to ensure its status is up to date.
	// NB: in asynch case, this just queues a check request and returns.
	// However, this request should return new status before calc request returns.
	// Changed to check always because status can be invalidated by variable definition.
	// So "Calculate" really means: Submit then calculate. 
	CheckEquation(EQ_ID(nEq));

	// Ask help system to do the work. Note: in case where asynch check is
	// pending, DDEML msg loop should pump its result, updating status of cur eq.
	LPCTSTR pszResult = 
		HelpSystemExecf("(calculate-equation-string \"%s\" %d)", strEq, nNextSlot);

	// Helpsys may piggyback hint msg in return value on error 
	// (e.g."you have not entered enough information...").
	CString strReturn, strHint;
	CCheckedObj::SplitResult(pszResult, strReturn, strHint);

	if (!strReturn.IsEmpty() && strReturn != "NIL") {
		// non-NIL result should be simplifed string. Set into new slot
		// note causes change notification, which updates doc & broadcasts update hint
		m_Edit[nNextSlot].SetRichEditText(strReturn);

		// result status should be same as current equation. Updates doc w/o hint
		SetEqStatus(nNextSlot, GetEqStatus(nEq));

		// Notify other views of change in equation
		GetDocument()->UpdateAllViews(this, HINT_UPDATE_EQUATION, (CObject*)nNextSlot);

		// Scroll as needed to make sure result is visible
		EnsureVisible(nNextSlot);

		// set focus to new slot (?or should it be next free after it if OK?)
		m_Edit[nNextSlot].SetFocus();
	}
	
	//show any returned help message in hint window
	if (!strHint.IsEmpty())
		theApp.GetMainFrame()->ShowHint(strHint);
}

void CEQView::OnUpdateCalculate(CCmdUI* pCmdUI) 
{
	if (m_nLastActive == -1 || ! HelpSystemIsConnected() 
		|| ! theApp.m_bFeedback) // make sure helpsys on for this problem
		pCmdUI->Enable(FALSE);
	else {
		ASSERT(IS_EQ_INDEX(m_nLastActive));
		pCmdUI->Enable(!IsBlank(m_nLastActive) && m_bEnabled);
	}
}

// Tell containing tab control what height we would like.
int CEQView::GetIdealHeight()
{
	if (! GetSafeHwnd() ) return 0;	// none if controls not created yet
	// Decided to use half the frame size in case window resized
    // to ensure that the equation view is always visible
	// when variables tab is clicked
	CRect rcFrame;
	theApp.GetChildFrame()->GetClientRect(&rcFrame);
	int eqVwHeight = rcFrame.Height()/2;
	return eqVwHeight;
}

// Scroll as needed to make given eq visible
void CEQView::EnsureVisible(int nEq)
{
	ASSERT(IS_EQ_INDEX(nEq));
	// do nothing if already visible, i.e. top & bottom within view's client area 
	CRect rcClient, rcEq;
	GetClientRect(&rcClient);
	m_Edit[nEq].GetWindowRect(&rcEq); // in screen coords
	ScreenToClient(&rcEq);
	if (rcClient.PtInRect(rcEq.TopLeft()) && rcClient.PtInRect(CPoint(rcEq.left, rcEq.bottom)))
		return;

	// else must scroll: adjust top of pane's scrollpos so its client area 
	// includes eq at bottom
	CPoint posNow = GetScrollPosition();	// in logical coords = device coords (MM_TEXT)
	int yPosEqBot = posNow.y + rcEq.bottom;	// logical pos is client origin plus scroll pos
	int yPosNew = yPosEqBot - rcClient.Height();
	ScrollToPosition(CPoint(posNow.x, yPosNew));
}


// Handle general solve for command
void CEQView::OnSolvefor() 
{
	CStringList strList;
	CFBDDoc* pDoc = GetDocument();
	// pop-up dialog to prompt for variable to solve for
	CSolveVarDlg dlg(&pDoc->m_strVarNames);
	if (dlg.DoModal() == IDOK)
	{
		SolveFor(dlg.m_strVar);
	}
}

// Do the SolveFor command (implemented in help system) for given variable.
void CEQView::SolveFor(CString str)
{
	// Get slot to use for simplified equation
	int nNextSlot = GetNextEqSlot();
	if (nNextSlot == -1)					// no room for more equations
		return;	
	ASSERT(IS_EQ_INDEX(nNextSlot));

	// Log the command
	LogEventf(EV_EQ_SOLVEFOR, "%s %d", str, nNextSlot);	// index for readability, checking

	// Get result from help system. 
	LPCTSTR pszResult =	HelpSystemExecf( "(solve-for-var \"%s\" %d)",
		STR2ARG(str),
		nNextSlot	);
	
	// Helpsys may piggyback hint msg in return value on error 
	// (e.g."you have not entered enough information...").
	CString strReturn, strHint;
	CCheckedObj::SplitResult(pszResult, strReturn, strHint);

	if (!strReturn.IsEmpty()) {
		// non-NIL result should be simplifed string. Set into new slot
		// note causes change notification, which updates doc & broadcasts update hint
		m_Edit[nNextSlot].SetRichEditText(strReturn);

		// Set to correct, since any result help system returns here is correct
		// it may not be simplified, however. If source had error, the help system 
		// will not return anything to be put into equation slot (strReturn empty)
		SetEqStatus(nNextSlot, statusCorrect);

		// Notify other views of change in equation
		GetDocument()->UpdateAllViews(this, HINT_UPDATE_EQUATION, (CObject*)nNextSlot);

		// Scroll as needed to make sure result is visible
		EnsureVisible(nNextSlot);

		// set focus to new slot (?or should it be next free after it if OK?)
		m_Edit[nNextSlot].SetFocus();
	}

	//show any returned help message in hint window
	if (!strHint.IsEmpty())
		theApp.GetMainFrame()->ShowHint(strHint);
}

void CEQView::OnUpdateSolvefor(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(HelpSystemIsConnected() && theApp.m_bFeedback // helpsys on for problem
		           && !GetDocument()->m_strVarNames.IsEmpty()
				   && m_bEnabled);
	
}

// Update handler for ID_SOLVEFOR_MENU, used as initial marker item 
// on popup submenu listing variables (attached to "Solve For>" on parent menu).
// We use this to list all variables on the popup submenu
void CEQView::OnUpdateSolveforMenu(CCmdUI* pCmdUI) 
{
	// can be called to enable/disable whole popup menu (copied from MFC TN21)
	if (pCmdUI->m_pSubMenu != NULL) {
		// enable whole submenu according as solve for command is available
		BOOL bEnable = HelpSystemIsConnected() && !GetDocument()->m_strVarNames.IsEmpty();
        // CCmdUI::Enable is a no-op for this case, so we
        //   must do what it would have done.
        pCmdUI->m_pMenu->EnableMenuItem(pCmdUI->m_nIndex,
            MF_BYPOSITION | (bEnable ? MF_ENABLED : (MF_DISABLED | MF_GRAYED)));
		return;
	}

	// else called to update the initial marker item ID_SOLVEFOR_MENU
	CMenu* pMenu = pCmdUI->m_pMenu;
	if (pMenu == NULL) return;	// this item not on a menu (shouldn't happen)

	// Modify menu, first taking out marker item. Should happen first update only,
	// so this isn't called again on subsequent popups from same context menu.
	pMenu->DeleteMenu(pCmdUI->m_nID, MF_BYCOMMAND);

	// Then add items for all the variables. Consecutive cmd ids for these start
	// *after* marker item's, so we're not called again on subsequent popups from 
	// the same context menu
	int nID = ID_SOLVEFOR_FIRST;	// first command ID to use
	POSITION pos = GetDocument()->m_strVarNames.GetHeadPosition();
	while (pos != NULL) {
		CString strVarName = GetDocument()->m_strVarNames.GetNext(pos);
		// Set owner draw on items w/Greek -- note must subclass popup to handle
		DWORD fOwnerDraw = strVarName.Find('$') != -1 ? MF_OWNERDRAW : 0;
		pMenu->InsertMenu(pCmdUI->m_nIndex++,	MF_STRING | MF_BYPOSITION | fOwnerDraw, 
			              nID++, strVarName);
	}

	// Following magic copied from CRecentFileList::UpdateMenu code
	// update end menu count 
	pCmdUI->m_nIndex--; // point to last menu added
	pCmdUI->m_nIndexMax = pCmdUI->m_pMenu->GetMenuItemCount();

	pCmdUI->m_bEnableChanged = TRUE;    // all the added items are enabled
}

// handle command from popup submenu to solve for nth variable in var list
void CEQView::OnSolveForNth(UINT nID)	
{
	int iVar = nID - ID_SOLVEFOR_FIRST; 
	POSITION pos = GetDocument()->m_strVarNames.FindIndex(iVar);
	if (pos == NULL) return;

	CString strVar = GetDocument()->m_strVarNames.GetAt(pos);
	SolveFor(strVar);
}

//////////////////////////////////////////////////////////////////////////////////
// 
// Event log playback support (IEventHandler implementation):
//
//////////////////////////////////////////////////////////////////////////////////

// Recreate effect of logged event:
BOOL CEQView::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{
	char argstr[32];				// buffer for copy of argument
	const char * pszRest = pszArgs;	// Unparsed Remainder in arg string
	int nEq;						// equation index argument
	int nStatus;

	switch(nEvent)
	{
	case EV_EQ_FOCUS:	// newer unambiguous event
	case EV_FOCUS:		// LogEventf(nEvent, "%d", EQ_INDEX(idEq));
		// Was a danger that setting focus here can cause spurious killfocus events on playback
		// if user mouses elsewhere, say. Shouldn't be a problem now that window is disabled
		// and we no longer validate on killfocus.
		if (sscanf(pszArgs, "%d", &nEq) != 1 || ! IS_EQ_INDEX(nEq))
			return FALSE;
		// programmatically set focus: should send notification
		// This doesn't work if mainframe is disabled during playback; but now we
		// are keeping it enabled and explicitly ignoring all input during playback.
		// else would have to record focus eq in member var rather than rely on Windows
		m_Edit[nEq].SetFocus();
		// Auto-scroll to make sure log viewer can see the focus eq.
		EnsureVisible(nEq);
		break;

	case EV_EQ_KILLFOCUS:	// LogEventf(nEvent, "%d", EQ_INDEX(idEq));
		if ( sscanf(pszArgs, "%d", &nEq) != 1 || ! IS_EQ_INDEX(nEq) )
			return FALSE;
		// don't do anything to kill the focus, will be handled by later set
		return TRUE;
		break;

	case EV_EQ_CHANGE:	// LogEventf(nEvent, "%d %s", EQ_INDEX(idEq), (LPCSTR) strEq); 
		// String which follows first arg may be empty or contain white space
		// Terminates at end of line
		sscanf(pszArgs, "%s", argstr);		// so can skip over it
		pszRest += strlen(argstr) + 1;
		if (sscanf(argstr, "%d", &nEq) != 1)
			return FALSE;
		if (! IS_EQ_INDEX(nEq) )
			return FALSE;
		m_Edit[nEq].SetRichEditText(CString(pszRest));// should trigger EN_CHANGE handler
		// m_Edit[nEq].UpdateWindow();	
		break;
	
	case EV_EQ_SUBMIT:	// LogEventf(EV_EQ_SUBMIT, "%d", EQ_INDEX(idCtrl));		
		if ( sscanf(pszArgs, "%d", &nEq) != 1 || ! IS_EQ_INDEX(nEq) )
			return FALSE;
		CheckEquation(EQ_ID(nEq));
		break;

	case EV_EQ_DELETE:
		if ( sscanf(pszArgs, "%d", &nEq) != 1 || ! IS_EQ_INDEX(nEq) )
			return FALSE;
		HelpSystemSendf("(lookup-eqn-string \"\" %d)", nEq);
		break;

	case EV_EQ_WHATSWRONG:
		if ( sscanf(pszArgs, "%d", &nEq) != 1 || ! IS_EQ_INDEX(nEq) )
			return FALSE;
		DoHelpWhatswrong(nEq);
		break;
	
	case EV_EQ_CALCULATE:
		// could check current eq arg is same as on playback
		OnCalculate();
		break;

	case EV_EQ_SOLVEFOR:	// LogEventf(EV_EQ_SOLVEFOR, "%s %d", strVar, nNextSlot);
		char szVar[128];
		if ( sscanf(pszArgs, "%s %d", szVar, &nEq) != 2 || ! IS_EQ_INDEX(nEq) )
			return FALSE;
		SolveFor(szVar);
		break;

	// Following trace message can be ignored, 
	case EV_EQ_STATUS:		// LogEventf(nEvent, "%d %d", EQ_INDEX(nEqID), (int) statusNew);
		break;

	// replay asynchronous eq query reply:
	case EV_EQ_RESULT:
		sscanf(pszArgs, "%s", argstr);		// so can skip over it
		pszRest += strlen(argstr) + 1;
		if (sscanf(argstr, "%d", &nEq) != 1)return FALSE;
		if (! IS_EQ_INDEX(nEq) ) return FALSE;
		ApplyStatusResult(pszRest, nEq);
		break;

	case EV_EQ_ENTRY:	// re-create initial equation entry
		sscanf(pszArgs, "%s", argstr);	pszRest += strlen(argstr) + 1;
		if (sscanf(argstr, "%d", &nEq) != 1) return FALSE;
		if (! IS_EQ_INDEX(nEq) ) return FALSE;
		sscanf(pszRest, "%s", argstr); pszRest += strlen(argstr) + 1;
		if (sscanf(argstr, "%d", &nStatus) != 1) return FALSE;
		ASSERT(0 <= nStatus && nStatus <= 2);
		// Update text and status from args 
		GetDocument()->m_strEq[nEq] = pszRest;
		GetDocument()->m_statusEq[nEq] = (Status) nStatus;
		// fire update -- no sender so we can update ourselves
		GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_EQUATION, (CObject*)nEq);
		break;

	default:	// unknown events ignored, return TRUE
		TRACE("EQView dispatch: unknown event %d, ignored\n", nEvent);
		break;
	}

	return TRUE;
}

// move the demo mode pointer to an object:
void CEQView::PointToObject(LPCTSTR pszObjID)
{
	CWnd* pWnd = NULL;
	BOOL bEq = FALSE;

	// arg is equation index or command button name
	int nEq;
	if ((sscanf(pszObjID, "%d", &nEq) == 1)
		&& IS_EQ_INDEX(nEq)) {
		pWnd = &m_Edit[nEq];
		bEq = TRUE;
	} else if (_stricmp(pszObjID, "Calculate") == 0) {
		pWnd = GetDlgItem(ID_CALCULATE);
	} else if (_stricmp(pszObjID, "GoGreek") == 0) {
		pWnd = GetDlgItem(IDC_GREEK);
	}
	if (pWnd == NULL) return;
			
	CRect rcCtl;
	pWnd->GetWindowRect(rcCtl);
	if (bEq) {// For equations, set pointer, to left, bottom
		theApp.GetMainFrame()->MovePointerTo(rcCtl.left, rcCtl.bottom, CPtrWnd::UpRight);
	} else { // For buttons, point to midpoint, bottom
		theApp.GetMainFrame()->MovePointerTo(rcCtl.left + rcCtl.Width()/2, rcCtl.bottom, CPtrWnd::UpRight);
	}
}
	


