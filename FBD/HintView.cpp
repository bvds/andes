// HintView.cpp : implementation file
//

#include "stdafx.h"


#include "fbd.h"
#include "helpifc.h"
#include "Childfrm.h"
#include "FBDDoc.h"
#include "PopupWnd.h"
#include "HintView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHintView

IMPLEMENT_DYNCREATE(CHintView, CFormView)

CHintView::CHintView()
	: CFormView(CHintView::IDD)
{
	//{{AFX_DATA_INIT(CHintView)
	//}}AFX_DATA_INIT
	m_pszHintSpec = NULL;
	m_bInHintSequence = NULL;
	// Initially disabled, enabled on hint message.
	m_bEnabled = FALSE;
}

CHintView::~CHintView()
{
	m_fontMsg.DeleteObject();	// free custom font we created
}

void CHintView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHintView)
	DDX_Control(pDX, ID_FOLLOWUP_WHY, m_btnWhy);
	DDX_Control(pDX, ID_FOLLOWUP_HOW, m_btnHow);
	DDX_Control(pDX, ID_FOLLOWUP_EXPLAIN, m_btnExplMore);
	//}}AFX_DATA_MAP
}

void CHintView::OnInitialUpdate() 
{
	CFormView::OnInitialUpdate();

	// We don't want scroll bars to ever appear in this pane
	SetScrollSizes(MM_TEXT, CSize(0, 0));
	// 
	// replace normal edit on form with richedit.
	//
	CWnd* pCtrl = GetDlgItem( IDC_HINT_TEXT );
	// grab styles of existing control
	DWORD dwStyles = pCtrl->GetStyle();
	DWORD dwExStyles = pCtrl->GetExStyle();
	// get the existing control's position on the form
	CRect rcEdit;
	pCtrl->GetWindowRect(rcEdit);
	ScreenToClient(rcEdit);
	// get rid of existing control
	pCtrl->DestroyWindow();
	// Create new rich edit w/same style, position, & id
	m_editHint.Create(dwStyles, rcEdit, this, IDC_HINT_TEXT);
	m_editHint.ModifyStyleEx(/*remove:*/0, /*add:*/ dwExStyles);
	// Set up to get notifications we want
	m_editHint.SetEventMask(ENM_REQUESTRESIZE);

	// Use larger font in message control for greater text visibility
	CClientDC dc(this);
	int nHeight = -((dc.GetDeviceCaps(LOGPIXELSY) * 10) / 72);
	m_fontMsg.CreateFont(nHeight, 0, 0, 0, FW_BOLD, 0, 0, 0,
		DEFAULT_CHARSET, OUT_CHARACTER_PRECIS, CLIP_CHARACTER_PRECIS,
		DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE,
		"MS Sans Serif");

	m_editHint.SetFont(&m_fontMsg, FALSE);

	// enable the appropriate buttons
	// UpdateDialogControls(AfxGetMainWnd(), TRUE);

	// Ensure enabled color is showing
	EnablePane(m_bEnabled);
}

BEGIN_MESSAGE_MAP(CHintView, CFormView)
	//{{AFX_MSG_MAP(CHintView)
	ON_WM_SIZE()
	ON_BN_CLICKED(ID_HINTBTN_HINT, OnBtnHint)
	ON_UPDATE_COMMAND_UI(ID_HINTBTN_HINT, OnUpdateBtnHint)
	ON_BN_CLICKED(ID_HINTBTN_WHATSWRONG, OnBtnWhatswrong)
	ON_UPDATE_COMMAND_UI(ID_HINTBTN_WHATSWRONG, OnUpdateBtnWhatswrong)
	ON_NOTIFY(EN_REQUESTRESIZE, IDC_HINT_TEXT, OnRequestresizeHint)
	//}}AFX_MSG_MAP
	ON_MESSAGE(WM_IDLEUPDATECMDUI, OnIdleUpdateCmdUI)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHintView diagnostics

#ifdef _DEBUG
void CHintView::AssertValid() const
{
	CFormView::AssertValid();
}

void CHintView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}
#endif //_DEBUG

//
// Operations
//

// SetHintSpec: Public method for setting current hint and followups
// Possible to use before Windows window has been created (but see comment.)
void CHintView::SetHintSpec(LPCTSTR pszHintSpec, HintType type /*=Msg*/)
{
	m_pszHintSpec = pszHintSpec;
	m_hintType = type;

	// Update controls if window is created
	if (m_hWnd != NULL){
		ProcessSpec();
//We need to force a resize here because we have some inital default 
//window size and our first single line hint will not set off a request resize message
//We had to create our own function to send the request resize message to the parent
//because the funciton provided by the RichEditCtrl (i.e. m_editHint.ReqestResize())
//worked intermittently  as did just sending the EM_REQUESTRESIZE message to the
//control
		ForceRequestResize();

	}
	// else we are saving pointer to someone else's buffer, probably HelpIfc's.
	// Presumably we will be shown soon and the text copied to controls,
	// Should change to copy the string to member. 
}

// Tags for standard followup command links. We add these in response to
// button codes in HintSpec's. Hyperlink implementation knows to call our 
// DoHelpRequest method passing the help command string as argument.
static const char szExplainLink[] = "{\\h Explain further}{\\v Explain-More}";
static const char szHowLink[] = "{\\h How do I do that?}{\\v Hint-Next-Substep}";
static const char szWhyLink[] = "{\\h Why?}{\\v Why}";
static const char szHideLink[] = "{\\h Hide}{\\v Hide}";

//
// ProcessSpec: worker routine parses spec, updating hint text and followup button 
//              controls.
// Ensures view is visible.
// Flashes control to get user's attention.
void CHintView::ProcessSpec()	
{
	m_strBtns.Empty();	// default unless set in spec
	if (m_pszHintSpec == NULL) {		// Remote call failed
		m_strHint = "Sorry, help could not be obtained for that request.";		
	} else {
		// Split spec at optional tilde separator into hint + followup button codes 
		CString strSpec = m_pszHintSpec;
		int nSepPos = strSpec.Find('~');
		if (nSepPos == -1)	{			// no separator, spec is hint only
			m_strHint = strSpec;
		} else {
			m_strHint = strSpec.Mid(0, nSepPos);
			if (nSepPos+1 < strSpec.GetLength()) // have something after sep
				m_strBtns = strSpec.Mid(nSepPos + 1); 
		}
	}

	// build text for specified followup links and append
	CString strAddLinks;
	if (m_strBtns.Find('e') != -1) 
		strAddLinks += szExplainLink;
	if (m_strBtns.Find('h') != -1) 
		strAddLinks += (strAddLinks.IsEmpty() ? "" : "     ") + CString(szHowLink);
	if (m_strBtns.Find('w') != -1) 
		strAddLinks += (strAddLinks.IsEmpty() ? "" : "     ") + CString(szWhyLink);
	// always add "Hide" button
	strAddLinks += (strAddLinks.IsEmpty() ? "" : "     ") + CString(szHideLink);

	// append space so trailing empty area not hot because nearest char is.
	if (strAddLinks)
		m_strHint += "\r\n\r\n        " + strAddLinks + " "; 


	// Update hint text in dialog
//	m_editHint.SetWindowText(m_strHint);
	m_editHint.SetRichEditText(m_strHint);
	
	// If there are any followup buttons then we say we are inside a "hint sequence".
	// Cmd enabling won't allow user to start another one while inside.
	m_bInHintSequence = !m_strBtns.IsEmpty(); 

	// Whenever new hint is set, ask parent frame to ensure we are visible.
//	EnsureVisible();

	// make a sound to alert user
	if (m_hintType == WhatsWrong)
		theApp.MakeSound(sndWhatsWrongMsg);
	else // its a Hint message
		theApp.MakeSound(sndHintMsg);

	// flash the control twice to highlight change
	m_editHint.SetBackgroundColor(FALSE, RGB(0, 0, 255));
	m_editHint.UpdateWindow();
	m_editHint.SetBackgroundColor(TRUE, 0);
	m_editHint.UpdateWindow();
	m_editHint.SetBackgroundColor(FALSE, RGB(0, 0, 255));
	m_editHint.UpdateWindow();
	m_editHint.SetBackgroundColor(TRUE, 0);

	// Could handle "popup" error message differently, by not entering tutor
	// mode (so rest of app doesn't grey) but enabling window (so can dismiss).
	// if (m_pszHintSpec && typeHint == Msg) EnablePane(TRUE);
	// For now, any message puts app in "tutor mode" until dismissed by student
	if (m_pszHintSpec != NULL)
		theApp.SetTutorMode(TRUE);
	else // helpsys failure: make sure pane is enabled so can dismiss.
		EnablePane(TRUE);
	
/*
	// Enable specified followup buttons
	m_btnHow.EnableWindow(m_strBtns.Find('h') != -1);
	m_btnExplMore.EnableWindow(m_strBtns.Find('e') != -1);
	m_btnWhy.EnableWindow(m_strBtns.Find('w') != -1); 
*/
	// Followup commands might be sent to the app frame from many sources (menu, 
	// toolbar, keyboard). Our m_strBtns member is made public so it can
	// serve to internally represent the current "followups-available" state on 
	// behalf of the whole app. MainFrame contains update handlers for the followup 
	// menu command IDs, which access m_strBtns to enable appropriately.
	// But dialogs don't automatically participate in the idle-time command
	// updating done by the framework -- so we have to force an update any
	// time the followups-available state changes.
	// UpdateDialogControls(AfxGetMainWnd(), TRUE);	
}

// Public method to reset hint state to empty and hide it.
//
// This currently called by mainframe as a side effect of any DDE call to help system, 
// according as its m_bClearHintOnDde flag is set. That includes those for followups questions, 
// so the hint pane is hidden on those too. This seems to be OK  -- the effect is that tutor is 
// "going away to think". It could be suppressed by clearing flag around followup DDE calls.  
void CHintView::ClearHint()
{
	// reset msg and button state (used by followup command update handlers).
	m_strHint.Empty();
	m_strBtns.Empty();
	m_pszHintSpec = NULL;
	// no longer inside a hint sequence
	m_bInHintSequence = FALSE;

	// change string without flashing.
	m_editHint.SetWindowText(m_strHint);

	// And tell container to hide the whole pane:
	theApp.GetChildFrame()->HideHintPane(); 

	// ensure app is out of tutor mode.
	theApp.SetTutorMode(FALSE);
	// Turning off tutor mode won't send an update hint if mode didn't change -- 
	// happens if hint is error message that didn't start TutorMode.
	// So we have to also ensure that pane is disabled for this case, so it
	// shows grey from now on, e.g. if user drags it open.
	EnablePane(FALSE);
}

/////////////////////////////////////////////////////////////////////////////
// CHintView message handlers

LRESULT CHintView::OnIdleUpdateCmdUI(WPARAM wParam, LPARAM lParam)
{
	UpdateDialogControls(AfxGetMainWnd(), TRUE);
	return 0L;
}

// Hint buttom click. This is ID_HINTBTN_HINT, to distinguish it
// from mainframe menu command ID_GET_PROC_HELP. Also to put it in HINTBTN range,
// used in moving button controls on resize.
// 
// We use different command id on form view button to avoid looping when frame
// routes menu command down to view for handling.
void CHintView::OnBtnHint() 
{
	// Delegate to handler in mainframe implementation of menu command
	// (uses custom timeout, etc) by sending command msg to mainframe
	AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_GET_PROC_HELP, 0);
}

// update state of our hint button
void CHintView::OnUpdateBtnHint(CCmdUI* pCmdUI) 
{
	// Update according to the enable state of the global hint menu command on 
	// mainframe menu. See explanation for whatswrong update below (which is more
	// complex since there enabling depends on command routing through views)
	if (!AfxGetMainWnd()->OnCmdMsg(ID_GET_PROC_HELP, CN_UPDATE_COMMAND_UI, pCmdUI, NULL))
		pCmdUI->Enable(FALSE);	// no update handler in mainframe's route
}

// now unused, since got rid of buttons in hint pane:

//
// Whatswrong button -- sends standard HELP_WHATSWRONG command to mainframe
// (which will distribute it along command route for handling in relevant view)
//
void CHintView::OnBtnWhatswrong() 
{
	AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_HELP_WHATSWRONG, 0);
}

void CHintView::OnUpdateBtnWhatswrong(CCmdUI* pCmdUI)
{
	// Tricky: we want to update according to the state of the global whatswrong 
	// menu command on mainframe menu. Menu commands and update requests for them
	// go first to mainframe which forwards it along command route to views; handlers in the 
	// views know if an error item is selected. We can easily simulate sending
	// commands, but have to know a little to send update request through cmd chain.
	if (!AfxGetMainWnd()->OnCmdMsg(ID_HELP_WHATSWRONG, CN_UPDATE_COMMAND_UI, 
		                            pCmdUI, NULL))
		pCmdUI->Enable(FALSE);	// no update handler in mainframe's route
}

// Followup button clicks:

// worker routine to hide the pane (command handler is in mainframe). 
void CHintView::HideHint() 
{
	// must get our containing parent frame to hide us.
	// if we clear the hint, the message handler OnRequestResize will take care of 
	// resizing the window
	ClearHint();
	
	//We need to force a resize here because what if user cleared the hint 
	//and then resized the window and then wanted to hide it again
	//We had to create our own function to send the request resize message to the parent
	//because the funciton provided by the RichEditCtrl (i.e. m_editHint.ReqestResize())
	//worked intermittently  as did just sending the EM_REQUESTRESIZE message to the
	//control
	ForceRequestResize();
	
//	theApp.GetChildFrame()->HideHintPane();
}

// update state of our hide button.
void CHintView::OnUpdateBtnHide(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(TRUE);	// always enabled
}

// for doc-view notifications:
void CHintView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	// Update state if Tutor mode changes
	if (lHint == HINT_TUTOR_MODE)
		EnablePane(DWORD(pHint));
	
	// ignore all other update hints, they apply to document data
	return;  	
}

//
// Enable/Disable whole pane
//
// Note we must enable the pane even when showing messages that do not start TutorMode,
// and disable when they are done (in ClearHint).
//
void CHintView::EnablePane(BOOL bEnable)
{
	// remember enabled state for command updating
	m_bEnabled = bEnable;
	// enable/disable input events to this pane
	EnableWindow(bEnable);
	// adjust background color to show state (richedits don't grey on disabling).
	if (m_bEnabled)
		m_editHint.SetBackgroundColor(TRUE, NULL); // sets to system color
	else
		m_editHint.SetBackgroundColor(FALSE, ::GetSysColor(COLOR_3DFACE));
}

enum { BTN_MARGIN = 6 };			// constant in device units.

void CHintView::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);
	
	if (! m_editHint.m_hWnd)	// skip if called when control doesn't exist
		return;

	// child controls have already been repositioned by Windows?
#if 0
	// first lay out row of buttons along bottom of window
	CRect rcCurrent;
	m_btnExplMore.GetWindowRect(rcCurrent);
	ScreenToClient(rcCurrent);
	
	int yBtnTop = cy - (rcCurrent.Height() + BTN_MARGIN);		
	int yEditBottom = cy - (rcCurrent.Height() + 2 * BTN_MARGIN);

	// move top of each button down to correct offset from pane bottom.
	// button ids must be in HINTBTN id range.
	for (int id = ID_HINTBTN_FIRST; id <= ID_HINTBTN_LAST; id++) 
	{
		CWnd* pBtn = GetDlgItem(id);
		if (pBtn == NULL) 
			continue;
		pBtn->GetWindowRect(rcCurrent);
		ScreenToClient(rcCurrent);
		pBtn->MoveWindow(rcCurrent.left, yBtnTop, rcCurrent.Width(), rcCurrent.Height()); 
	}
	
	// resize the edit box to fill width and go down to top of button row
	m_editHint.MoveWindow(0, 0, cx, yEditBottom); 
#else
	m_editHint.MoveWindow(0, 0, cx, cy);  // resize to occupy whole window
#endif 
}

// Full protocol for popping up hint msg pane is as follows: 
// CMainFrame::ShowHint 
//      CHintView::SetHintSpec
//         CHintView::ResizeHint
//				CChildFrame..ShowHintPane
//                    CHintView::GetIdealHeight
//              [CChildFrame adjusts splitter panes]
//
//Ensure Visible works for showing and hiding. 
//Called in OnRequestResize handler
//Should be renamed
void CHintView::ResizeHint()
{
	// Ask our CChildFrame parent, which manages splitters, to do the work.
	// It will query us back for our desired height by calling GetIdealHeight().
	CFrameWnd* pFrame = GetParentFrame();
	ASSERT_KINDOF(CChildFrame, pFrame); 
	((CChildFrame*)pFrame)->ShowHintPane();
}

// GetIdealHeight -- Return desired height for the pane in pixels.
// Used by containing frame to manipulate splitter pane layout when ensuring
// that hint text and followup buttons are visible.

//GetIdealHeight no longer gets the entire height of the hintview.
//Should be renamed.  Now retrieves the addidtional height needed for the
//buttons and the descent of the characters
int CHintView::GetMarginHeight()
{
	if (! GetSafeHwnd() ) return 0;	// none if controls not created yet

	// Need to calculate a minimum height for the pane in pixels at which text plus
	// buttons can reasonably be assumed to be visible. (We may not have exact
	// text here?)

	// get text metrics for our edit font
	CClientDC dc(this);
	dc.SelectObject(m_fontMsg);
	TEXTMETRIC tm;
	dc.GetTextMetrics(&tm);
#if 0
	// Get button height plus margin.
	CRect rcBtn;
	m_btnWhy.GetWindowRect(rcBtn);
	int cyBtnRow =  rcBtn.Height() + 2 * BTN_MARGIN;
	

	//return room for text descent (bottom of lowercase g, y, etc.) and button row
	return tm.tmDescent + cyBtnRow;
#endif 0
	// one character-height margin.
	return 3* tm.tmDescent; // tm.tmHeight;  // + tm.tmExternalLeading;
}

int CHintView::GetMinimumHeight()
{
	if (! GetSafeHwnd() ) return 0;	// none if controls not created yet

	// Need to calculate a minimum height for the pane in pixels at which text plus
	// buttons can reasonably be assumed to be visible. (We may not have exact
	// text here?)

	// get text metrics for our edit font
	CClientDC dc(this);
	dc.SelectObject(m_fontMsg);
	TEXTMETRIC tm;
	dc.GetTextMetrics(&tm);
	int cyLine = tm.tmHeight + tm.tmExternalLeading;

//	// return room for 4 text lines minimum
	return 4 * cyLine;

}


void CHintView::PopupDef(CString term, CPoint point)
{
	LogEventf(EV_HINT_POPUP, "%s %d %d", term, point.x, point.y);
	CString strDef;
	term.MakeLower();
	if (! theApp.LookupDef(term, strDef) || strDef.IsEmpty())
		strDef = "Definition not found";	// show something in case of error
	TRACE("popping up %s = %s\n", term, strDef);
	CPopupWnd* pWnd = new CPopupWnd(); 
	pWnd->m_strDef = strDef;
	ClientToScreen(&point);
	pWnd->Create(this, point);

	// For log playback only: save pointer to current popup wnd for 
	// easy access when replaying the KILL_POPUP event
	m_pPopup = pWnd;
}

void CHintView::PostNcDestroy() 
{
	delete this;	
	//CFormView::PostNcDestroy();
}



// Replay logged events.
BOOL CHintView::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{
	switch (nEvent) 
	{
	// followup cmd events now moved to mainframe

	case EV_HINT_POPUP: 
		{
			CPoint pt;
			char szTerm[128];
			if (sscanf(pszArgs, "%s %d %d", szTerm, &pt.x, &pt.y) == 0)
				return FALSE;
			PopupDef(szTerm, pt);
		}
		break;
	case EV_KILL_POPUP:
		{	/* finding from screen pos doesn't work if playback w/diff window size
			CPoint pt;
			HWND hwnd;
			int msg;
			if (sscanf(pszArgs, "%d %d %d %d %d %d", &hwnd, &msg,
				&pt.x, &pt.y)==0)
				return FALSE;
			CWnd* pWnd = WindowFromPoint(pt);
			if (pWnd)
				pWnd->SendMessage(WM_KILLPOPUP, (WPARAM)msg, (LPARAM)hwnd); */
			if (::IsWindow(m_pPopup->GetSafeHwnd()))
				m_pPopup->SendMessage(WM_KILLPOPUP); // no params anymore
			else 
				TRACE("HintView.KillPopup -- no current popup window! ignored\n");
		}

		break;
	default:	// just ignore unknown codes.
		TRACE("HintView dispatch: unknown event %d, ignored\n", nEvent);
		break;
	}
	return TRUE;
}

//We had to create our own function to send the request resize message to the parent
//because the funciton provided by the RichEditCtrl (i.e. m_editHint.ReqestResize())
//worked intermittently  as did just sending the EM_REQUESTRESIZE message to the
//control
void CHintView::ForceRequestResize()
{
	REQRESIZE reqrez;
	m_editHint.GetClientRect(&reqrez.rc);
	reqrez.nmhdr.hwndFrom = m_editHint.GetSafeHwnd();
	reqrez.nmhdr.idFrom   = m_editHint.GetDlgCtrlID();
	reqrez.nmhdr.code     = EN_REQUESTRESIZE;

	SendMessage(WM_NOTIFY, (WPARAM)m_editHint.GetSafeHwnd(), (LPARAM)&reqrez);
}

void CHintView::OnRequestresizeHint(NMHDR* pNMHDR, LRESULT* pResult) 
{
	REQRESIZE *pReqResize = reinterpret_cast<REQRESIZE *>(pNMHDR);
	// TODO: The control will not send this notification unless you override the
	// CFormView::OnInitDialog() function to send the EM_SETEVENTMASK message
	// to the control with the ENM_REQUESTRESIZE flag ORed into the lParam mask.
	if (m_editHint.GetTextLength()==0)
		m_nHeight = 0;
	else{
		int cyWnd = pReqResize->rc.bottom;
		if (cyWnd < GetMinimumHeight())
			cyWnd = GetMinimumHeight();
		m_nHeight = GetMarginHeight() + cyWnd;
	}

	ResizeHint();
	
	*pResult = 0;
}

void CHintView::PointToObject(LPCTSTR pszObjID)
{
	// for hint hyperlinks: ObjID should be first word of link (e.g. How, Explain, Why)
	// lookup link by ObjID prefix on edit's link list
	CHyperLnk* pLnk = m_editHint.FindLink(pszObjID);
	if (pLnk == NULL) {
		TRACE("HintView::PointTo -- Unknown object %s\n", pszObjID);
		return;
	}
	// Get screen pos of top-right corner of last char in hyperlink text from edit ctl
	// Looks like posEnd is for space just *after* hyperlink text (?) -- so we sub 1
	// to avoid error if last char in link wound up at end of line 
	CPoint ptLinkEnd = m_editHint.GetCharPos(pLnk->m_posEnd - 1);
	// Edit's client coords are OK since edit is at 0,0 relative to view. 
	ClientToScreen(&ptLinkEnd);
	
	// add character width to get to last char's right edge
	CClientDC dc(this);
	dc.SelectObject(m_fontMsg);
	TEXTMETRIC tm;
	dc.GetTextMetrics(&tm);
	ptLinkEnd.x += tm.tmMaxCharWidth; // extra width adds little margin

	// set pointer at right edge, midway down char to baseline
	theApp.GetMainFrame()->MovePointerTo(ptLinkEnd.x, ptLinkEnd.y + tm.tmHeight/2);
}

