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
	ON_UPDATE_COMMAND_UI(ID_DIALOG_WHATSWRONG, OnUpdateDialogWhatswrong)
		ON_WM_CONTEXTMENU()
	ON_WM_SETCURSOR()
	//}}AFX_MSG_MAP
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

		// ignore messages for windows other than dialog or hint
		if (!IsDialogMessage(&msg))	// msg not handled by this dialog
		{
			if (! (   (msg.hwnd == m_hWnd)						// this dlg 
				   || (::IsChild(m_hWnd, msg.hwnd))				// child of this dlg
				   || (m_hwndCtrl && (msg.hwnd == m_hwndCtrl))	// current popup wnd
				   || (msg.hwnd == hwndHint) ) )				// hint wnd
			{
				// For all other windows: ignore all key and mouse strokes 
				if ((msg.message >= WM_MOUSEFIRST && msg.message <= WM_MOUSELAST) 
				 || (msg.message >= WM_KEYFIRST && msg.message <= WM_KEYLAST) 
				 || (msg.message >= WM_NCMOUSEMOVE && msg.message <= WM_NCMBUTTONDBLCLK)
				 /* No good, MOUSEACTIVATE is *sent* to wndproc, not posted to msg queue!
				 || (msg.message == WM_MOUSEACTIVATE) */) {
					continue;	// eat message without processing
				}
			}

			// otherwise, translate and dispatch message normally
			if (!AfxGetThread()->PreTranslateMessage(&msg)) {
				TranslateMessage(&msg);
				DispatchMessage(&msg);
			}
		}
	} // end loop

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
	if ( WM_CTLCOLORLISTBOX == message ) // 32 bits has new message
    {
        // Get the list box handle (combo box drop down list)
        // The parent of the list box is the desktop window
	
		// the drop list of a combobox is a control of the dialog that needs to 
		// receive messages, but the wnd is not a child of the dialog
		// We need to save its hwnd so we can explicitly pass messages along 
		m_hwndCtrl = (HWND) lParam ;     // HWND is 32 bits
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
}

HBRUSH CCheckedDlg::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	HBRUSH hbr = CDialog::OnCtlColor(pDC, pWnd, nCtlColor);

	if (!((nCtlColor == CTLCOLOR_EDIT) || (nCtlColor == CTLCOLOR_LISTBOX)))
		return hbr;

	// do nothing if not one of our status-bearing controls
	if (! IsCheckedCtrl(pWnd))
		return hbr;

	// Set appropriate text foreground color
	Status status = GetCtrlStatus(pWnd);
	switch (status)
	{
	case statusCorrect:
		pDC->SetTextColor(RGB(0, 128, 0));	// green
		break;

	case statusError:
		pDC->SetTextColor(RGB(255, 0, 0));  // red
		break;

	default:
		TRACE("OnCtlColor: Bad status value, pWnd = %d\n", pWnd);
		// fall through ...
	case statusUnknown:
		break;								// leave in default color
	}

	// TODO: Return a different brush if the default is not desired

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

	return hbr;
}

// update all checked control statuses given list of error slot names.
// Returns T if no errors
BOOL CCheckedDlg::UpdateStatuses(const CStringList& errors)
{
	BOOL bAllCorrect = TRUE;// until error found
	if (! errors.IsEmpty())	// must have at least one error spec
	{
		// map list of error slot names to list of error ctl CWnd*'s
		CTypedPtrList<CObList, CWnd*> errWnds;
		for (POSITION pos = errors.GetHeadPosition(); pos != NULL; ) {
			CString strSlotName = errors.GetNext(pos);
			int nID = CtlNameToID(strSlotName);
			ASSERT(nID != -1);	// bug somewhere if slot name not in table
			if (nID != -1)		// but be safe in release build
				errWnds.AddTail(GetDlgItem(nID));
		}

		// Walk all child window controls, coloring checked ctrls appropriately
		for (CWnd* pCtl = GetTopWindow(); pCtl != NULL; pCtl = pCtl->GetNextWindow())
		{
			// skip non-checked controls
			if (! IsCheckedCtrl(pCtl))
				continue;

			// color red or green according as control is in error list
			if (errWnds.Find(pCtl)){
				SetCtrlStatus(pCtl, statusError);
				bAllCorrect = FALSE;
			} else {
				SetCtrlStatus(pCtl, statusCorrect);
			}
		}
	}
	return bAllCorrect;
}

// To provide update handlers for cmds on control context menus 
// In particular, ID_DIALOG_WHATSWRONG for asking whats wrong on control

//had to force an update here, wasn't updating on its own
//Although I am calling the context menus from inside the controls,
//I seem to need to update within the dialog.
void CCheckedDlg::OnInitMenuPopup(CMenu* pPopupMenu, UINT nIndex, BOOL bSysMenu) 
{
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

// base class provides standard enabling, but derived classes must
// override with appropriate handler for "ID_DIALOG_WHATSWRONG" command, since
// help system commands vary for different types of objects.
void CCheckedDlg::OnUpdateDialogWhatswrong(CCmdUI* pCmdUI) 
{
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
	// Enable/Disable input to this window
	EnableWindow(! bTutorMode);

	// Grey/ungrey all child controls to represent enabled state visually.
	// For now we limit this to ANDES log-aware controls (and not, e.g. static
	// labels, buttons, or radio-buttons.) That should include all editable 
	// choice fields which should be sufficient visual cue.
	for (CWnd* pCtl = GetTopWindow(); pCtl != NULL; pCtl = pCtl->GetNextWindow())
		GreyCtrl(pCtl, !bTutorMode);

	// Trigger an IDLE update, to update toolbars. Needed because
	// idle processing is not triggered during our modal message loop.
	// Could try to fix this in modal loop, or use the timer events (which 
	// trigger updates for status bar clock during DDE waits) for this.
	AfxGetMainWnd()->PostMessage(WM_ENTERIDLE); // our handler calls OnIdle
}

// Visibly enable/disable control
// Any ctrl accepted as argument, but only has an effect on our checked controls 
// plus richedit derivatives used for labels. 
void CCheckedDlg::GreyCtrl(CWnd *pCtrl, BOOL bEnable)
{
	ASSERT(pCtrl != NULL);
	
	// special case for richedit, which has method to set background color.
	if (pCtrl->IsKindOf(RUNTIME_CLASS(CRichEditCtrl))) {
		if (bEnable)
			((CRichEditCtrl*)pCtrl)->SetBackgroundColor(TRUE, NULL); // sets to system color
		else
			((CRichEditCtrl*)pCtrl)->SetBackgroundColor(FALSE, ::GetSysColor(COLOR_3DFACE));
		return;
	}

	// Set flag if one of our checked controls
	if (IsCheckedCtrl(pCtrl)) {
		SetCtrlEnabled(pCtrl, bEnable);
	}
	// else do nothing.
}


