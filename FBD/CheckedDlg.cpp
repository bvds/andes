// CheckedDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "LgDialog.h"
#include "HintView.h"
#include "ChatView.h"
#include "PopupWnd.h"	// for WM_POPUPDEF

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCheckedDlg dialog


CCheckedDlg::CCheckedDlg(int id, CWnd* pParent /*=NULL*/)
	: CLogDialog(id, pParent)
{
	// save id and optional parent for use on dialog creation
	m_nId = id;
	m_pParent = pParent;
	
	// for modeless dialog option:
	m_bModeless = FALSE;
	m_bEndModalLoop = FALSE;
	m_nResult = -1;
	// saved control hwnd that isn't a childwnd of dialog
	m_hwndCtrl = NULL;

}

void CCheckedDlg::DoDataExchange(CDataExchange* pDX)
{
	CLogDialog::DoDataExchange(pDX);
}


IMPLEMENT_DYNAMIC(CCheckedDlg, CLogDialog)
BEGIN_MESSAGE_MAP(CCheckedDlg, CLogDialog)
	//{{AFX_MSG_MAP(CCheckedDlg)
		ON_WM_CTLCOLOR()
		ON_WM_INITMENUPOPUP()
	    ON_UPDATE_COMMAND_UI(ID_CONTROL_WHATSWRONG, OnUpdateControlWhatswrong)
		ON_WM_CONTEXTMENU()
	    ON_WM_SETCURSOR()
	//}}AFX_MSG_MAP
	ON_WM_NCHITTEST()
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCheckedDlg message handlers

//
// Run the dialog as a modeless dialog with a custom-coded modal loop
// in which the hint pane is also enabled, but input to all other application windows 
// is ignored, so they are effectively disabled as in a standard modal dialog. 
// Note this achieves the effect of disabling the other windows *without* using the 
// EnableWindow API, so the other windows don't "know" that they are disabled.
//
// Need special care to enable input to the current popup window (m_hWndCtrl) which
// may be the dropdown list for a combo box or a popup definition in the hint window.
//
BOOL AFXAPI AfxIsDescendant(HWND hWndParent, HWND hWndChild); // MFC internal func

int CCheckedDlg::DoModalWithHints()
{
	// Flag that dialog is running as modeless (with hints) for OK/Cancel processing.
	m_bModeless = TRUE;

	// Pre-modal: Adjust player state in case app is running from log file
	CDialog* pOldDlg = NULL;	// saves pointer to previous dialog, if any
	pOldDlg = LogPlayerBeginModalDlg(this);

	// Create the modeless dialog
	Create(m_nId, m_pParent);
	ShowWindow(SW_SHOW);

	// Run message loop, filtering msgs as appropriate
	// We need to pass all messages to hint window, this dialog,
	// and any popup windows. Popup wnds for dropdown lists are not 
	// children of the dialog. Also includes popups for hint window defs.
	HWND hwndHint = NULL; 
	if (theApp.GetChatView())
		hwndHint = theApp.GetChatView()->GetRichEditCtrl().GetSafeHwnd();
	else if (theApp.GetHintView())
	{
		CHintRichEdit* pHint = &theApp.GetHintView()->m_editHint;
		hwndHint = pHint->GetSafeHwnd();
	}
	HWND hwndHintParent = ::GetParent(hwndHint);

	MSG msg;
	m_bEndModalLoop = FALSE;
	BOOL bQuit = FALSE;
	while (!m_bEndModalLoop && ! (bQuit = !GetMessage(&msg, NULL, 0, 0)))
	{	
		// Top priority: if app is in log playback mode, need to eat all input but input 
		// to log player control, even while in dlg-with-hints modes. 
		if (LogPlayerInPlayback() || theApp.IsRemoteViewer()) 	
		{   
			// App's PreTranslateMessage handles playback mode filtering.
			if (!AfxGetThread()->PreTranslateMessage(&msg)) {
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			}
			continue;	// skip rest of loop
		}
		
		// else get here => app is in normal interactive mode

		// intercept custom popup def notification to remember hwnd of current popup
		if (msg.message == WM_POPUPDEF) 
			m_hwndCtrl = (HWND) msg.lParam;
	
		// ignore input messages for windows other than current dialog, its descendant controls,
		// hint window, or currently active popup (remembered in m_hWnd). This is to effectively 
		// disable the rest of the app. Can't do it by disabling mainframe since that would disable
		// all children including the hint window.
		// Really have two mutually exclusive modes: In tutorial dialog mode, the dialog will be 
		// disabled (though would like to be able to move it!) In dialog editing mode, hint window 
		// is not disabled but has no response to input. These are not switched here.
		if (! ( (::IsWindow(msg.hwnd) && AfxIsDescendant(m_hWnd, msg.hwnd) /*&& !theApp.m_bTutorMode*/)
			  || (m_hwndCtrl && (msg.hwnd == m_hwndCtrl))	// current popup wnd
			  || (msg.hwnd == hwndHint) 					// hint wnd
			  || (msg.hwnd == hwndHintParent)               // hint's parent = splitter bar
			  ))
		{
			// not for an allowed window: ignore all key and mouse strokes 
			if ((msg.message >= WM_MOUSEFIRST && msg.message <= WM_MOUSELAST) 
				 || (msg.message >= WM_KEYFIRST && msg.message <= WM_KEYLAST) 
				 || (msg.message >= WM_NCMOUSEMOVE && msg.message <= WM_NCMBUTTONDBLCLK)
				 /* No good, MOUSEACTIVATE is *sent* to wndproc, not posted to msg queue!
				 || (msg.message == WM_MOUSEACTIVATE) */) {
					continue;	// eat message without dispatching it
			}
		} 

		// In tutor mode, dialog should ignore mouse and keyboard input. But allow NC mouse messages
		// so dialog can be moved. Dialog itself will suppress close button.
		if (theApp.m_bTutorMode && ::IsWindow(msg.hwnd) && AfxIsDescendant(m_hWnd, msg.hwnd)
			&& ((msg.message >= WM_MOUSEFIRST && msg.message <= WM_MOUSELAST) 
				 || (msg.message >= WM_KEYFIRST && msg.message <= WM_KEYLAST)) )
				 continue;

		// get here => not eaten, translate and dispatch normally. Note PreTranslateMessage
		// is necessary to allow keyboard shortcuts for Greek character insertion
		// to be processed by controls in the dialog. The dialog's PreTranslateMessage 
		// will also handle dialog keyboard interface messages via IsDialogMessage
		if (!AfxGetThread()->PreTranslateMessage(&msg)) {
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		}
	} // end message handling loop

	if (bQuit)					// Got end of input notification !
		AfxPostQuitMessage(0);	// repost QUIT message so containing message pump will see it

	// Post Modal Loop: If running from a log, restore previous playback state
	// Note might not be executed in case playback aborted inside dialog.
	LogPlayerEndModalDlg(pOldDlg);	
	
	DestroyWindow();

	return m_nResult;
}





LRESULT CCheckedDlg::DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam) 
{
	// HACK: the dropdown list of a combobox is a window that needs to 
	// receive messages when the dialog is active, but the wnd is a popup
	// window parented to the desktop. Since it is not a child of the dialog
	// our custom message loop will not recognize it as a window that should receive
	// messages in dialog-with-hints mode. The following technique from MS KB article
	// 131845 exploits the fact that the dropdown list window sends WM_CTLCOLORLISTBOX 
	// to the combo box and to the parent window. We use this to find its hwnd and
	// remember it so our custom msg loop can still know to let messages through to 
	// this window. 
	if ( WM_CTLCOLORLISTBOX == message ) // new message in 32 bit windows
    {
		// Just to  be safe, make sure this is not from a listbox control on the dialog 
		// (Body and Resistance dialogs have multi-selection listboxes).
		if (! AfxIsDescendant(m_hWnd, (HWND) lParam))
			m_hwndCtrl = (HWND) lParam ;    
	}

	return CLogDialog::DefWindowProc(message, wParam, lParam);
}

void CCheckedDlg::OnOK()
{
	if (m_bModeless)
	{
		m_nResult = IDOK;
		UpdateData(TRUE);
		OnDialogEnd();
		m_bEndModalLoop = TRUE;	// flag end to our custom modal loop
		TRACE("ending dlg msg loop (OK)\n");
	}
	else // normal modal dlg end
		CLogDialog::OnOK();
}

void CCheckedDlg::OnCancel()
{
	if (m_bModeless)
	{
		m_nResult = IDCANCEL;
		OnDialogEnd();
		m_bEndModalLoop = TRUE; // flag end to our custom modal loop
		TRACE("ending dlg msg loop (CANCEL)\n");
	}
	else // normal modal dlg end
		CLogDialog::OnCancel();
}

// 
// Helpers for dealing with status-bearing (colorable) controls
//
// This is inelegant since they do not all derive from a common base class,
// since they each derive from an MFC control class. Possibly we could
// fix this by using a common mixin class and C++ RTTI for dynamic class inquiries.
// For now methods switch on type.
//
BOOL CCheckedDlg::IsCheckedCtrl(CWnd *pCtrl)
{
	return pCtrl && (   pCtrl->IsKindOf(RUNTIME_CLASS(CLogEdit))
		             || pCtrl->IsKindOf(RUNTIME_CLASS(CLogCombo))
		             || pCtrl->IsKindOf(RUNTIME_CLASS(CLogList)) 
					 || pCtrl->IsKindOf(RUNTIME_CLASS(CLogRichEdit))
					 || pCtrl->IsKindOf(RUNTIME_CLASS(CLogBtn))
		            );
}

Status CCheckedDlg::GetCtrlStatus(CWnd *pCtrl)
{
	ASSERT(pCtrl != NULL);
	ASSERT(IsCheckedCtrl(pCtrl)); 

	if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogCombo)))
		return ((CLogCombo*)pCtrl)->GetStatus();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogEdit)))
		return ((CLogEdit*)pCtrl)->GetStatus();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogList)))
		return ((CLogList*)pCtrl)->GetStatus();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogRichEdit)))
		return ((CLogRichEdit*)pCtrl)->GetStatus();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogBtn)))
		return ((CLogBtn*)pCtrl)->GetStatus();

	TRACE("GetCtrlStatus -- bad control type (id=%d hwnd=%s\n)", pCtrl->GetDlgCtrlID(), pCtrl->m_hWnd);
	return statusUnknown;
}

void CCheckedDlg::SetCtrlStatus(CWnd* pCtrl, Status status)
{
	ASSERT(pCtrl != NULL);
	ASSERT(IsCheckedCtrl(pCtrl));

	if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogCombo)))
		((CLogCombo*)pCtrl)->SetStatus(status);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogEdit)))
		 ((CLogEdit*)pCtrl)->SetStatus(status);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogList)))
		((CLogList*)pCtrl)->SetStatus(status);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogRichEdit)))
		((CLogRichEdit*)pCtrl)->SetStatus(status);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogBtn)))
		((CLogBtn*)pCtrl)->SetStatus(status);
}

BOOL CCheckedDlg::GetCtrlEnabled(CWnd *pCtrl)
{
	ASSERT(pCtrl != NULL);
	ASSERT(IsCheckedCtrl(pCtrl));

	if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogCombo)))
		return ((CLogCombo*)pCtrl)->GetEnabled();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogEdit)))
		return ((CLogEdit*)pCtrl)->GetEnabled();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogList)))
		return ((CLogList*)pCtrl)->GetEnabled();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogRichEdit)))
		return ((CLogRichEdit*)pCtrl)->GetEnabled();
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogBtn)))
		return ((CLogBtn*)pCtrl)->GetEnabled();

	TRACE("GetCtrlEnabled -- bad control type (id=%d hwnd=%s\n)", pCtrl->GetDlgCtrlID(), pCtrl->m_hWnd);
	return TRUE;
}

void CCheckedDlg::SetCtrlEnabled(CWnd* pCtrl, BOOL bEnable)
{
	ASSERT(pCtrl != NULL);
	ASSERT(IsCheckedCtrl(pCtrl));

	if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogCombo)))
		((CLogCombo*)pCtrl)->SetEnabled(bEnable);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogEdit)))
		((CLogEdit*)pCtrl)->SetEnabled(bEnable);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogList)))
		((CLogList*)pCtrl)->SetEnabled(bEnable);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogRichEdit)))
		((CLogRichEdit*)pCtrl)->SetEnabled(bEnable);
	else if (pCtrl->IsKindOf(RUNTIME_CLASS(CLogBtn)))
		((CLogBtn*)pCtrl)->SetEnabled(bEnable);
}

HBRUSH CCheckedDlg::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	HBRUSH hbr = CDialog::OnCtlColor(pDC, pWnd, nCtlColor);

	if (! (nCtlColor == CTLCOLOR_EDIT 
		   || nCtlColor == CTLCOLOR_LISTBOX
		   || nCtlColor == CTLCOLOR_STATIC) ) // STATIC sent for radio buttons
		return hbr;
	
	// For "incorrect" status text, show in red.
	if (pWnd->GetDlgCtrlID() == IDC_INCORRECT) {
		pDC->SetTextColor(RGB(255, 0, 0));  // red
		return hbr;
	}

	// TRACE("CTL_COLOR %d for %s %x\n", nCtlColor, GetCtlName(pWnd), pWnd->m_hWnd);  
	// do nothing if not one of our status-bearing controls
	if (! IsCheckedCtrl(pWnd))
		return hbr;

	// For disabled appearance, set grey background. 
	// Note status setting below may override if it uses background coloring

	// We do this in the case of grayed list/combo/edit boxes to draw colored text 
	// against grey background. (Just disabling won't draw colored text).
	if (! GetCtrlEnabled(pWnd)) 
	{
		// Setting transparent text mode doesn't seem to suffice for listboxes
		// pDC->SetBkMode(TRANSPARENT);
	
		// set background color for text painting 
		pDC->SetBkColor(::GetSysColor(COLOR_3DFACE));
		
		// and return background color brush for rest of control
		hbr = ::GetSysColorBrush(COLOR_3DFACE);
	}

	// Set appropriate color to indicate status. 
	Status status = GetCtrlStatus(pWnd);
	// TRACE("Status = %d\n", status);
	switch (status)
	{
	case statusCorrect:
		pDC->SetTextColor(RGB(0, 128, 0));	// green
		break;

	case statusError:
#if 0   // set red text
		pDC->SetTextColor(RGB(255, 0, 0));  // red
#else // set red background for control, rather than red text
		pDC->SetBkColor(RGB(255, 0, 0));
		if ((HBRUSH) m_brRed == NULL) m_brRed.CreateSolidBrush(RGB(255, 0, 0));
		hbr = m_brRed;
#endif 
		break;

	default:
		TRACE("OnCtlColor: Bad status value, pWnd = %d\n", pWnd);
		// fall through ...
	case statusUnknown:
		break;								// leave in default color
	}

	return hbr;
}

// update all checked control statuses given list of error slot names.
// Returns T if no errors got flagged. 
BOOL CCheckedDlg::UpdateStatuses(const CStringList& errors)
{
	BOOL bAllCorrect = TRUE;// until error found
	WndList errWnds;
	
	// map list of error slot names to list of error ctl CWnd*'s
	// This is just for convenience in UpdateStatus. Note doing lookup from
	// name to id lets us have many names for same control.
	for (POSITION pos = errors.GetHeadPosition(); pos != NULL; ) {
		CString strSlotName = errors.GetNext(pos);
		int nID = CtlNameToID(strSlotName);
		ASSERT(nID != -1);	// bug somewhere if slot name not in table
		if (nID != -1)	{	// but be safe in release build
			// was GetDlgItem
			CWnd* pCtl = GetDescendantWindow(m_hWnd, nID, /*bOnlyPerm:*/FALSE);
			if (pCtl)
				errWnds.AddTail(pCtl);
		}
	}

	// Walk all child window controls, updating checked ctrls appropriately
	// Need to do this even if no error to reset state from previous error
	for (CWnd* pCtl = GetTopWindow(); pCtl != NULL; pCtl = pCtl->GetNextWindow())
		bAllCorrect &= UpdateStatus(pCtl, errWnds);

	return bAllCorrect;
}

BOOL CCheckedDlg::UpdateStatus(CWnd *pCtl, WndList &errWnds)
{
	BOOL bAllCorrect = TRUE;	// until we find an error

	// first recurse through any children of this control
	for (CWnd* pChild = pCtl->GetTopWindow(); pChild != NULL; pChild = pChild->GetNextWindow()) 
			bAllCorrect &= UpdateStatus(pChild, errWnds);
	// now do this one
	
	// skip non-checked controls
	if (! IsCheckedCtrl(pCtl))
		return bAllCorrect;

	// color red or green according as control is in error list
	// Safety precaution: don't update a control whose window is disabled 
	// e.g. disabled dir box on a zero-length vector, since it can't be
	// changed. 
	if (errWnds.Find(pCtl) && pCtl->IsWindowEnabled()){
		SetCtrlStatus(pCtl, statusError);
		return FALSE;
	} 
	// get here -> not in error list: 
	// Reset to unknown for normal appearance
	SetCtrlStatus(pCtl, statusUnknown);
	return bAllCorrect;
}



// To provide update handlers for cmds on control context menus 
// In particular, ID_CONTROL_WHATSWRONG for asking whats wrong on control

//had to force an update here, wasn't updating on its own
//Although I am calling the context menus from inside the controls,
//I seem to need to update within the dialog.
void CCheckedDlg::OnInitMenuPopup(CMenu* pPopupMenu, UINT nIndex, BOOL bSysMenu) 
{
	// AW: this gets the commands on the menu updated through a "state" object
	// which carries their command IDS through the normal command routing chain.
	// The current dialog is hooked into that chain so OnUpdateControlWhatswrong 
	// below gets called through this mechanism.
	CLogDialog::OnInitMenuPopup(pPopupMenu, nIndex, bSysMenu);
	CCmdUI state;
	state.m_nIndexMax = pPopupMenu->GetMenuItemCount();
	state.m_pMenu = pPopupMenu;
	for (state.m_nIndex = 0; state.m_nIndex < state.m_nIndexMax;  state.m_nIndex++)
	{
		state.m_nID = pPopupMenu->GetMenuItemID(state.m_nIndex);
		CWnd::OnCmdMsg(state.m_nID, CN_UPDATE_COMMAND_UI, &state, NULL);
	}
}

// base class provides standard enabling, but derived classes might have to
// override with appropriate handler for "ID_CONTROL_WHATSWRONG" command, since
// help system commands vary for different types of objects.
void CCheckedDlg::OnUpdateControlWhatswrong(CCmdUI* pCmdUI) 
{
	// this enables the command when focus is on an error slot only.
	// Might also want to enable on whole dialog on error.
	CWnd* pWnd = GetFocus();
	pCmdUI->Enable(pWnd && IsCheckedCtrl(pWnd) && GetCtrlStatus(pWnd) == statusError);
}

BOOL CCheckedDlg::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
	if (theApp.GetMainFrame()->m_bInDdeWait)
	{
		theApp.RestoreWaitCursor();
		return TRUE;
	}
	else
		return CLogDialog::OnSetCursor(pWnd, nHitTest, message);
}

//
// Handle change into/out of tutor mode
//
void CCheckedDlg::OnTutorModeChange(BOOL bTutorMode)
{
	// !!! this doesn't seem to be working. Now handle in message loop.

	// Enable/Disable input to this window
	EnableWindow(! bTutorMode);

	// Grey/ungrey all child controls to represent enabled state visually.
	// For now we limit this to ANDES log-aware controls (and not, e.g. static
	// labels, buttons, or radio-buttons.) That should include all editable 
	// choice fields which should be sufficient visual cue.
	for (CWnd* pCtl = GetTopWindow(); pCtl != NULL; pCtl = pCtl->GetNextWindow()) {
		GreyCtrl(pCtl, !bTutorMode);
	}

	UpdateUI();
}

UINT CCheckedDlg::OnNcHitTest(CPoint point)
{
	// in tutor mode, treat all hits as caption hits so can be moved but not closed.
	if (theApp.m_bTutorMode) return HTCAPTION;

	return CLogDialog::OnNcHitTest(point);
}


// update whole state of the dialog (error vs non-error)
void CCheckedDlg::UpdateDlgStatus(Status status, const CStringList& errors)
{
	m_status = status;
	UpdateUI();		// incorrect indicator and whatswrong button
	UpdateStatuses(errors);
}

void CCheckedDlg::UpdateUI()
{
	// Simulate an idle time update, to update UI for the application (toolbars, etc).
	// Needed because MFC's normal idle processing is not triggered during our own
	// modal message loop.  
	
	// Could try to fix this in our modal loop following model from  MFC source 
	// In MFC CWnd modal loop (used by CDialogs) WM_KICKIDLE is posted to the
	// dialog and WM_ENTERIDLE notification is sent to dialog's owner in the idle state.
	// WM_KICKIDLE is never handled anywhere in MFC, but it can be trapped in a CWnd derived
	// class like a CDialog-derivative that is interested in doing idle time procesing.
	// Mainframe handles WM_ENTERIDLE and ultimately thread's OnIdle. This does a 
	// SendMessageToDescendants of WM_IDLEUPDATECMDUI to main windows children and frames to 
	// trigger their idle time updating. Unclear how floating tools participate in this
	// message.
	
	// invoke handler for dialog idle notification to parent to update mainframe controls.
	// Not clear if this really is necessary
	theApp.GetMainFrame()->SendMessage(WM_ENTERIDLE, MSGF_DIALOGBOX, (LPARAM)m_hWnd);

	// show/hide incorrect marker and whatswrong button on error
	CWnd* pWW = GetDlgItem(ID_DIALOG_WHATSWRONG);
	if (pWW) 
		pWW->ShowWindow(m_status == statusError ? SW_SHOW : SW_HIDE);
	CWnd* pInc = GetDlgItem(IDC_INCORRECT);
	if (pInc)
		pInc->ShowWindow(m_status == statusError ? SW_SHOW : SW_HIDE);

	// update controls in the dialog
	UpdateDialogControls(this, TRUE);
}

// Visibly enable/disable control
// Any ctrl accepted as argument, but only has an effect on our checked controls 
// plus richedit derivatives used for labels. 
void CCheckedDlg::GreyCtrl(CWnd *pCtrl, BOOL bEnable)
{
	ASSERT(pCtrl != NULL);

	// first recurse through any children of this control
	for (CWnd* pChild = pCtrl->GetTopWindow(); pChild != NULL; pChild = pChild->GetNextWindow()) 
				GreyCtrl(pChild, bEnable);
	// now do this one

	// Set flag if one of our checked controls
	if (IsCheckedCtrl(pCtrl)) {
		SetCtrlEnabled(pCtrl, bEnable);
	}
	// else do nothing.
}





