//CDrawObjDlg.cpp - implementation file

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "helpifc.h"
#include "MainFrm.h"
#include "FBDObj.h"			// needed only GetBodiesFromLabel
#include "EQView.h"
#include "HintView.h"
#include "DrawObjDlg.h"
#include "PopupWnd.h"
#include "PlanStrs.h"


/////////////////////////////////////////////////////////////////////////////
// CDrawObjDlg dialog
//
// 
//
/////////////////////////////////////////////////////////////////////////////

CDrawObjDlg::CDrawObjDlg(int id, CDrawObj* pObj /*NULL*/, CWnd* pParent /*=NULL*/)
	: CCheckedDlg(id, pParent)
{
	//All objects passed in
	m_pObj = pObj;
	m_pTempObj = NULL;
	if (pObj)		// have object: init data members from its props 
	{
		m_pTempObj = m_pObj->Clone();
		m_pDocument = m_pObj->m_pDocument;
		if (m_pDocument == NULL)
			m_pDocument = (CFBDDoc*)theApp.GetDocument();
	}
	//dialog reused
	m_bSought = FALSE;
	m_bMagnitude = TRUE;
	m_bProp = FALSE;
	m_bNoCheck = FALSE;

	//prevobj used to keep track of temporary object checking
	m_pPrevObj = NULL;
}

// CDrawObjDlg::DoDataExchange -- transfer values between object and controls
//	
// This should be called from derived class DoDataExchange *after* using 
// CDialog::DoDataExchange and derived class DDX routines to subclass controls 
// and fill combo boxes with choices.
//
// The full protocol by which property values move back and forth is quite hairy.
// In any derived drawobj property dialog, slot values move between underlying object, 
// temp object, and dlg controls as follows:
//
// On Construction:
// -CDrawObjDlg base class constructor creates a tempobj as clone of specified underlying object.
//  tempobj is like an editor buffer -- it holds last submitted edited property values before they are
//  written through to underlying object. 
//
// On Initialization:
// -custom OnInitDialog calls CDialog::OnInitDialog which calls DoDataExchange
// -custom DoDataExchange should subclass controls and populate choice lists as appropriate, 
//  then call CDrawObjDlg::DoDataExchange (this routine).
// -this routine sees that it is loading values, so moves current slot values from tempobj into controls 
//  via InitDlg, which delegates to custom 
//     InitObjectDlg|InitVariableDlg to do the work of setting control vals from object
//  and also calls
//     InitLabel to init the label value (via custom GetLabel) w/prefix (GetLabelPrefix)
//
// .... user edits controls then hits OK...
//
// On Submission:
// -Custom OnOK ensures completeness and transfers values back to tempobj,
//   usually branching to UpdateTempVector | UpdateTempVariable to do work of transferring.
// -Custom OnOK checks with help system via CheckDialog.
// -If all OK, custom OnOK will call CDialog::OnOK which will call DoDataExchange again with 
//   bSave = TRUE
// - this routine transfers props to real obj via UpdateObj => pObj->UpdateObj,
//   which knows how to copy properties from temp obj to real obj.
//
void CDrawObjDlg::DoDataExchange(CDataExchange* pDX) 
{
	if (m_pObj)
	{
		if (pDX->m_bSaveAndValidate)
			UpdateObj();//update true object
		else
			InitDlg();
	}
	CCheckedDlg::DoDataExchange(pDX);
}

void CDrawObjDlg::UpdateObj()
{
	if (m_pObj == NULL)
		return;
	m_pObj->UpdateObj(m_pTempObj);
}

IMPLEMENT_DYNAMIC(CDrawObjDlg, CCheckedDlg)

BEGIN_MESSAGE_MAP(CDrawObjDlg, CCheckedDlg)
	//{{AFX_MSG_MAP(CDrawObjDlg)
	ON_COMMAND(ID_CONTROL_WHATSWRONG, OnDialogWhatswrong)
	ON_COMMAND(ID_DIALOG_WHATSWRONG, OnDialogWhatswrong)
	ON_UPDATE_COMMAND_UI(ID_DIALOG_WHATSWRONG, OnUpdateDialogWhatswrong)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

// ID_CONTROL_WHATSWRONG is fired from popup menu on flagged control
// Update UI for this is handled in CCheckedDlg base class. 
// ID_DIALOG_WHATSWRONG fired from button in dialog and handled here
// because need to know it is a drawobj dialog (includes variables) and
// not a checked dialog for some other sort of object.
// Need separate enabling for the two because control version is only used
// over flagged slot. But both commands now mapped to the same handler, which 
// just asks whatswrong onthe whole object. 

/////////////////////////////////////////////////////////////////////////////
// CDrawObjDlg message handlers
//
void CDrawObjDlg::InitDlg()
{
	if (m_pObj->IsKindOf(RUNTIME_CLASS(CVariable)))
		InitVariableDlg();
	else
		InitObjectDlg();
	
	InitLabel();
}

// Initialize custom label control.
// Sets up custom richedit and inits with object's type prefix.
void CDrawObjDlg::InitLabel()
{
	CWnd* pCtrl = GetLabelCtrl();
	if (pCtrl == NULL)		// no label in this dialog
		return;

	PrepareLabelCtrl(GetLabelCtrl());

	// Set "read-only" prefix with object's prefix.
	GetLabelCtrl()->SetPrefix(m_pTempObj->GetLabelPrefix());

	if (!m_pTempObj->m_strName.IsEmpty())
	{
		GetLabelCtrl()->SetRichEditText(m_pTempObj->m_strName);
	}
	GetLabelCtrl()->GetRichEditText(m_pTempObj->m_strName);
}

// Does common work of preparing a label rich edit control for use:
void CDrawObjDlg::PrepareLabelCtrl(CLabelRichEdit* pLabelCtrl)
{
	//font must be set before calling setprefix or setting default font
	//otherwise will override setprefix's format bold change and default's onprotected change
	pLabelCtrl->SetFont(GetFont());

	// Set default style to PROTECTED, used by control to suppress change notifications
	// on format changes.
	CHARFORMAT cfDefault;
	cfDefault.cbSize = sizeof(cfDefault);
	cfDefault.dwEffects = CFE_PROTECTED; 
	cfDefault.dwMask = CFM_PROTECTED;
	pLabelCtrl->SetDefaultCharFormat(cfDefault);

	// Set up to get change notifications, adding ENM_PROTECTED since 
	// control requires it. 
	pLabelCtrl->SetEventMask(ENM_CHANGE |ENM_PROTECTED | ENM_LINK | ENM_KEYEVENTS);
}

BOOL CDrawObjDlg::OnInitDialog() 
{
	CCheckedDlg::OnInitDialog();

	// force whatswrong button to update
	//load whatswrong bitmap onto button 
	if (GetDlgItem(ID_DIALOG_WHATSWRONG))
	{
#if 1 // use bitmap for button
		m_hBitmap = (HBITMAP) ::LoadImage(AfxGetInstanceHandle(),
					MAKEINTRESOURCE(IDB_WHATSWRONG), IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT|LR_LOADMAP3DCOLORS);
		((CButton*)GetDlgItem(ID_DIALOG_WHATSWRONG))->SetBitmap(m_hBitmap); 
#else // use icon 
		((CButton*)GetDlgItem(ID_DIALOG_WHATSWRONG))->SetIcon(AfxGetApp()->LoadIcon(IDI_WHATSWRONG));
#endif 
	}
	// set global dialog status from temp obj
	m_status = m_pTempObj->m_status;
	UpdateUI();
	
	MoveDlgToBtmRight();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


//this function should NOT be used for dialogs in the example problems
//because the EQview is not created.  Instead use MoveEXDlgToBtmRight();
void CDrawObjDlg::MoveDlgToBtmRight()
{
	int left, top;
	CRect dlgRect, frmRect;
	CRect eqRect(0, 0, 0, 0);
		
	GetWindowRect(&dlgRect);
	CFrameWnd* pWnd = GetParentFrame();
	pWnd->GetWindowRect(&frmRect);
	CEQView* pView = theApp.GetEQView();
	if (pView != NULL)
		pView->GetWindowRect(&eqRect);
	int dlgWidth = dlgRect.right-dlgRect.left;
	int dlgHeight = dlgRect.bottom-dlgRect.top;
	int eqWidth = eqRect.right - eqRect.left;
	int eqHeight = eqRect.bottom - eqRect.top;
	if (eqWidth >= dlgWidth)
		left = eqRect.left;
	else
		left = frmRect.right-dlgWidth;
	if (eqHeight >= dlgHeight)
		top = eqRect.top;
	else
		top = frmRect.bottom - dlgHeight-25;
	MoveWindow(left, top, dlgWidth, dlgHeight);
}



/*
//we are assuming buttons are the same size and are 
//aligned across the top
void CDrawObjDlg::CenterTwoButtons(CLogDialog* pDlg, CButton* pBtn1, CButton* pBtn2)
{
	CRect dlgRect, btnRect1, btnRect2;

	pDlg->GetWindowRect(&dlgRect);
	pBtn1->GetWindowRect(&btnRect1);
	pBtn2->GetWindowRect(&btnRect2);
	int midDlg = (dlgRect.right + dlgRect.left)/2;
	int heightBtn=btnRect1.bottom-btnRect1.top;
	int widthBtn=btnRect1.right-btnRect1.left;
	int halfBtn = (widthBtn)/2;
	int leftPos1 = midDlg-widthBtn-halfBtn-dlgRect.left;
	int leftPos2 = midDlg+halfBtn-dlgRect.left;
	int topPos=btnRect1.top-dlgRect.top-heightBtn;
	pBtn1->MoveWindow(leftPos1, topPos, widthBtn, heightBtn,TRUE);
	pBtn2->MoveWindow(leftPos2, topPos, widthBtn, heightBtn,TRUE);

}*/


CString CDrawObjDlg::GetBodiesFromLabel(CString label)
{
	CString str;
	if (!m_pDocument)
		return "";

	POSITION pos = m_pDocument->m_objects.GetTailPosition();
	while (pos!=NULL)
	{
		CDrawObj* pObj=m_pDocument->m_objects.GetPrev(pos);
		if (pObj->m_flag == TEACHER_OBJECT)//want to skip our teacher drawn objects
			continue;
		if (pObj->IsKindOf(RUNTIME_CLASS(CSystem))){
			CSystem* pSys = (CSystem*)pObj;
			if (_stricmp(pSys->m_strName, label) == 0){//case insensitive
				str = pSys->m_strBodies;		
				str.TrimRight();
			}

		}
	}
	return str;
	
}

//
// helper functions for controls
//

void CDrawObjDlg::EnableListBox(CListBox * pBox, BOOL bEnable)
{
	if (!bEnable)
		pBox->SetCurSel(-1);
	pBox->EnableWindow(bEnable);
}

void CDrawObjDlg::EnableComboBox(CComboBox * pBox, BOOL bEnable)
{	
	if (!bEnable)
		pBox->SetCurSel(-1);
	pBox->EnableWindow(bEnable);

}

CString CDrawObjDlg::GetCurString(CComboBox* m_pCboBox)
{
	CString curString;

	//function returns currently selected string in any of the list boxes
	if (!m_pCboBox->IsWindowVisible()||!m_pCboBox->IsWindowEnabled())
		return curString;
	
	int curIndex = m_pCboBox->GetCurSel();

	if (curIndex >= 0)
		m_pCboBox->GetLBText(curIndex, curString);
	
	return curString;
}

void CDrawObjDlg::RemoveTimePeriods(CComboBox* m_pCboBox)
{
	int nCount = m_pCboBox->GetCount();

	if (nCount <= 0)
		return;
	CString strPeriod;
	for (int i = (nCount - 1); i >= 0; i--)
	{
		m_pCboBox->GetLBText(i, strPeriod);
		if ((strPeriod.Find(" to ") > 0) || 
			(m_pCboBox->GetItemData(i) == ID_USER_DEFINED))
		{
			m_pCboBox->DeleteString(i);
		}
	}

}


BOOL CDrawObjDlg::IsEmpty(CComboBox* m_cboBox)
{
	//function returns currently selected string in any of the list boxes
	if (!m_cboBox->IsWindowVisible()||!m_cboBox->IsWindowEnabled())
		return FALSE;
	int curIndex = m_cboBox->GetCurSel();
	if (curIndex < 0)
		return TRUE;
	return FALSE;
}

void CDrawObjDlg::Remove(int nIDBox)
{
	//function is resizing the parent dialog (vertically) by the height of 
	//the static box it is "removing" so to speak
	CWnd* pWnd = GetDlgItem(nIDBox);
	if (pWnd == NULL) return;
		
	CRect rcDlg;
	GetWindowRect(&rcDlg);
	CRect rcBox;
	pWnd->GetWindowRect(&rcBox);
	SetWindowPos(NULL, 0, 0, rcDlg.Width(), rcDlg.Height() - rcBox.Height(),
				SWP_NOZORDER | SWP_NOMOVE);

	// New: hide box and all controls lying within box. (Some client code does this also).
	// (??? Wrong if might relocate some of these elsewhere within dialog later?)
	// Note this only looks at those following box in the tab order. (Could change to test all).
	pWnd->ShowWindow(SW_HIDE);
	CWnd* pCtrl = pWnd->GetNextWindow(GW_HWNDNEXT );
	while (pCtrl != NULL){
		CRect rcCtrl, rcOverlap;
		pCtrl->GetWindowRect(&rcCtrl);
		if (rcOverlap.IntersectRect(rcBox, rcCtrl)) {
			pCtrl->ShowWindow(SW_HIDE);
			// TRACE("Hiding ctrl %d\n", pCtrl->GetDlgCtrlID());
		}
		pCtrl = pCtrl->GetNextWindow(GW_HWNDNEXT );
	}
	
	//move up controls that follow box, as defined by the tabbing order
	UpdateControls(pWnd, rcBox.Height());
}

//for this to work, must be careful with the tab order.
//GetNextWindow will retrieve the control next in the tab order
void CDrawObjDlg::UpdateControls(CWnd* pCtrlBegin, int xDist)
{
	CWnd* pCtrl = pCtrlBegin->GetNextWindow(GW_HWNDNEXT );
	while (pCtrl != NULL){
		// TRACE("Moving up ctrl %d\n", pCtrl->GetDlgCtrlID());
		CRect rect;
		pCtrl->GetWindowRect(&rect);
		ScreenToClient(&rect);
		pCtrl->SetWindowPos(NULL, rect.left, (rect.top-xDist), 0, 0,
				SWP_NOZORDER | SWP_NOSIZE);
		pCtrl = pCtrl->GetNextWindow(GW_HWNDNEXT );
	}
}

void CDrawObjDlg::UpdatePlanStrings(CLogCombo* m_pCboBox)
{
	int nCount = m_strPlanBodies.GetCount();
	if (nCount == 1)
	{
		CString strBody = m_strPlanBodies.GetHead();
		m_pCboBox->SelectStringExact(strBody);
	}
	else if (nCount > 1)
	{
		CString strBodies;
		POSITION pos = m_strPlanBodies.GetHeadPosition();
		while (pos != NULL)
		{
			CString strBody = m_strPlanBodies.GetNext(pos);
			CString str;
			str.Format("%s ", strBody);
			strBodies = strBodies + str;
		}
		CString str;
		str.Format("{%s}", strBodies);
		m_pCboBox->InsertString(-1, str);
		if (m_pCboBox->GetCurSel() < 0)
			m_pCboBox->SelectStringExact(str);

	}
}

// Set given combo-box to have a custom "read-only" appearance.
void CDrawObjDlg::SetComboReadOnly(CComboBox* pBox)
{
	// If we just disabled the combo boxes in this case, the text would be 
	// shown grey and hard to read. In order to show a read-only box with black 
	// lettering we replace our usual dropdown list style combos -- whose display 
	// part is a static -- with regular dropdown combos, whose display part is an edit. 
	// Then we change the edit control subpart to enabled but read-only. 
	DroplistToDropdown(pBox);
	
	// Disable the whole combo box control
	pBox->EnableWindow(FALSE);
	// Make the disabled combo's edit control appear as enabled & R/O
    // 1. The first child window of a combo is its edit control
    CEdit* pComboEdit =(CEdit*)(pBox->GetWindow(GW_CHILD ));
     // 2. Enable combo's edit control, not the combo itself, and set R/O
    pComboEdit->EnableWindow( TRUE );    
	pComboEdit->SetReadOnly();
}

// Change given droplist style combo to dropdown combo w/same id
void CDrawObjDlg::DroplistToDropdown(CComboBox* pBox)
{
	// Can't just change styles because they only take effect on window creation,
	// so have to destroy and recreate the underlying control window.

	// get child ctrl id of existing control window
	int nCtrlID = pBox->GetDlgCtrlID();
	// grab styles of existing control window
	DWORD dwStyles =  ::GetWindowLong(pBox->m_hWnd, GWL_STYLE);
	// get the existing control window's position on the form
	CRect rcList;
	pBox->GetDroppedControlRect(&rcList);
	CRect rcCombo;
	pBox->GetWindowRect(rcCombo);
	ScreenToClient(rcCombo);
	ScreenToClient(rcList);
	rcCombo.bottom = rcList.bottom;
	// We don't have to copy choice list, since new combo won't have any possible choices
	// but may need to copy selected value = window text.
	CString strText;
	pBox->GetWindowText(strText);
	
	// get rid of existing control window
	pBox->DestroyWindow();
	
	// Create new droplist w/same position, & id, attached to same C++ obj
	dwStyles &= ~CBS_DROPDOWNLIST;
	dwStyles |= CBS_DROPDOWN ;
	pBox->Create(dwStyles, rcCombo, this, nCtrlID);
	pBox->SetWindowText(strText);
}


//////////////////////////////////////////////////////////////////////////////////////////
// The following is help calls to check dialog and, if necessary, reveal what's wrong
//
// returns T if OK to close
BOOL CDrawObjDlg::CheckDialog()
{
	BOOL bCorrect = TRUE;	// until error found
	BOOL bFlaggedCtrl = FALSE; 
	
	// don't verify if checking has been suppressed 
	if (! m_bNoCheck)
	{
		//do not want re-entrant DDE calls
		EnableWindow(FALSE);

		// Notify views of update (used by varview)
		CChkObjList pList;
		pList.AddTail((CCheckedObj*)m_pObj);//real obj at head of list
		pList.AddTail((CCheckedObj*)m_pTempObj);//temp obj at list tail
		// Check obj with help system.  Updates its status and error list
		((CCheckedObj*)m_pTempObj)->CheckObject();
		m_pDocument->UpdateAllViews(NULL, HINT_UPDATE_TEMPOBJ, &pList);
		bCorrect = m_pTempObj->m_status == statusCorrect;

		// Update global dialog status
		m_status = m_pTempObj->m_status;
		UpdateUI();

		// apply updated slot statuses to dialog controls 
		bFlaggedCtrl = ! UpdateStatuses(m_pTempObj->m_errors);

		// if unsolicited hint given on submission, don't enable the window 
		// until the student dismisses the dialog mode:
		if (! theApp.m_bTutorMode)
				EnableWindow(TRUE);	// done updating, user can interact again
		
		// check if bad definition is resubmission of previous def.
		if (!bCorrect)
		{
			// Warn if definition is unchanged since last OK, and still bad.
			// Note we need to empty the id string after we update the saved previous 
			// object below. Otherwise HasSameDef function will not return an accurate 
			// comparison because it will think we are comparing an object to itself, 
			// which it ignores (it really means "distinct instance w/same def").
			if (m_pPrevObj && m_pTempObj->HasSameDef(m_pPrevObj) 
					&& m_pTempObj->HasSameName(m_pPrevObj) && m_pTempObj->HasSameDir(m_pPrevObj)
					&& m_pTempObj->HasSameValues(m_pPrevObj))
			{
					CString str = "You still have errors inside this dialog. Are you sure you want to exit?";
					// if (theApp.DoWarningMessage(str, this, MB_YESNO) != IDCANCEL)
						return TRUE;	// close the dialog
			}
		}
	}
	// update prev obj with saved copy of current obj, 
	// clearing id so prev will trip HasSameDef if unchanged.
	if (m_pPrevObj)
		delete m_pPrevObj;
	m_pPrevObj = (CCheckedObj*)m_pTempObj->Clone();
	m_pPrevObj->m_strId.Empty();

	return bCorrect;
}


void CDrawObjDlg::OnDialogWhatswrong() 
{
	LogEventf(EV_DLG_WHATSWRONG, "%s %s", m_pTempObj->m_strName, m_pTempObj->m_strId);

	LPCTSTR	 pszResult = HelpSystemExecf("(Why-wrong-object |%s| %s) ", 
									STR2ARG(m_pTempObj->m_strName), STR2ARG(m_pTempObj->m_strId) );
	
	// Display result in hint dialog, which knows how to parse it.
	// Ask frame to show result in hint window
	theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong);
}

void CDrawObjDlg::OnUpdateDialogWhatswrong(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_pTempObj && m_pTempObj->m_status == statusError);
}

void CDrawObjDlg::PostNcDestroy() // cleanup after window destroyed:
{
	if (m_pPrevObj)
		delete m_pPrevObj;
	if (m_pTempObj)
		delete	m_pTempObj;
	if (m_hBitmap)
		::DeleteObject(m_hBitmap);

	CCheckedDlg::PostNcDestroy();
}

//global helper function to ensure valid object labels
//This function is declared in the drawobjdlg.h file and is used
//by the system dialog and all the vector dialogs

BOOL CDrawObjDlg::IsValidLabel(CString editStr)
{
	char illegalChar[]=" \\/():*?""<>|#[]%.,^-+=~`'{}@!&:$";
	if(editStr.IsEmpty()){
		theApp.DoWarningMessage("Please enter a label", this);
		return FALSE;
	}
	else if ((editStr.FindOneOf(illegalChar)!=-1)||(!isalpha(editStr[0])))
	{
		theApp.DoWarningMessage("Label cannot contain blank spaces or any of the following characters:\n\\/():*?""<>|#[]%.,^-+=~`'{}@!&:$\nPlease enter a valid label.", this);
		return FALSE;
	}
	return TRUE;
}

// get printable string describing dialog operation. Supports "snapshot" printing
CString CDrawObjDlg::GetDlgInfo() 
{
	CString strVerb = m_pObj->m_strName.IsEmpty() ? "Defining" : "Editing";
	CString strQuant; m_pObj->GetTypeName(strQuant);
	CString strType  = m_pObj->IsKindOf(RUNTIME_CLASS(CVariable)) ? "Variable"  
								: m_pObj->IsKindOf(RUNTIME_CLASS(CVector)) ? "Vector" 
																			: "";
	return strVerb + " " + strQuant + " " + strType + " " + m_pObj->m_strName;
};

// To support saving "snapshot" files of current dialog editing state.
// We add comment equation line to indicate what is going on.
void CDrawObjDlg::SaveSnapshot(const CString& strSnapPath)
{
	CDrawObjDlg* pDlg = this;	// temp, for copied code
	CFBDDoc* pDoc = m_pObj->m_pDocument;

	// Simulate an "Apply" by writing edits into the object right now, 
	// so current def will show up under object def in printout
	// This could affect playback since Cancel function should revert m_pObj to
	// pre-dialog state, so we save/restore underlying m_pObj state around snapshot. 
	CDrawObj* pOrigState = m_pObj->Clone();
	UpdateData(); // does an update obj, transferring props from tempobj to obj.
	pDoc->UpdateAllViews(NULL, HINT_UPDATE_DRAWOBJ, m_pObj);
	// !!! This assumes control's state has been transferred into tempObj and we 
	// have enough def to show at this point (need a label to print, for ex.)
	// That will be true of all the snapshots we are interested in since they come 
	// on help requests after submits, but may not be true in general.

	// For showing relevant dialog state: 
	// Add a comment line describing the dialog and definition state.
	// Putting it in the equations means it will be saved for later printing.
	// We remove it when we're done so it doesn't mess up any subsequent playing.
	int iAdd = -1;					// non-neg if added info
	CString strDef = "; " + pDlg->GetDlgInfo();
	/* // Get printable object def from dialog for display.
	CString strDef2 = ";" + pDlg->m_pTempObj->GetPrintDef(); */

	// Search up to find top filled equation slot (= -1 if none filled)
	for (int iTop = NEQS; --iTop >= 0; ) {
		CString strEq = pDoc->m_strEq[iTop];
		strEq.TrimLeft();
		if (!strEq.IsEmpty()) break; 
	}
	iAdd = iTop + 1;	// Add text into top free slot 
	
	// Set text into eq line in document. Status stays black
	// Have to update eqview because serialization does UpdateDoc (obsolete?)
	ASSERT(iAdd >= 0 && iAdd < NEQS);		// will normally have enough room
	pDoc->m_strEq[iAdd] = strDef;
	pDoc->UpdateAllViews(NULL, HINT_UPDATE_EQUATION, (CObject*)iAdd);
/*	ASSERT(iAdd+1 >= 0 && iAdd+1 < NEQS);		// will normally have enough room
	pDoc->m_strEq[iAdd+1] = strDef2;
	pDoc->m_statusEq[iAdd+1] = m_pTempObj->m_status;
	pDoc->UpdateAllViews(NULL, HINT_UPDATE_EQUATION, (CObject*)(iAdd+1)); */
	
	// use DoSave worker routine, not DoSaveCopyAs, since latter prompts user.
	pDoc->DoSave(strSnapPath, FALSE);

	// If we added descriptive lines, clear them after save.
	if (iAdd != -1) {
		pDoc->m_strEq[iAdd].Empty();
		pDoc->UpdateAllViews(NULL, HINT_UPDATE_EQUATION, (CObject*)iAdd);
	/*	pDoc->m_strEq[iAdd+1].Empty();
		pDoc->m_statusEq[iAdd+1] = statusUnknown;
		pDoc->UpdateAllViews(NULL, HINT_UPDATE_EQUATION, (CObject*)(iAdd+1)); */
	}

	// restore original state of underlying object
	// Unclear if this works in general though seems OK.
	m_pObj->UpdateObj(pOrigState);
	pDoc->UpdateAllViews(NULL, HINT_UPDATE_DRAWOBJ, m_pObj);
	// also resync display state with temp obj state (as if resubmitted edits).
	CChkObjList pList;
	pList.AddTail((CCheckedObj*)m_pObj);
	pList.AddTail((CCheckedObj*)m_pTempObj);
	pDoc->UpdateAllViews(NULL, HINT_UPDATE_TEMPOBJ, &pList);
}

//////////////////////////////////////////////////////////////////////////////////////
//
// DDX functions


void AFXAPI DDX_AddUserTimes(CDataExchange* pDX, int nIDC, CVarList* pObjList)
{
#if 0	// turn this whole routine into no-op, since don't want user times anymore.

	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);

	if (!pDX->m_bSaveAndValidate)
	{
		POSITION pos = pObjList->GetTailPosition();
		while (pos != NULL)
		{
			CVariable* pVar = pObjList->GetPrev(pos);
			if ( ( pVar->m_nType == ID_VARIABLE_ADDTIME) &&	! pVar->m_strName.IsEmpty() )
			{
				/////////////////////////////////////
				//Send both messages (for list boxes and combos, will be ignored if incorrect)
				WPARAM nIndex = ::SendMessage(hWndCtrl, CB_ADDSTRING, 0, (LPARAM)(LPSTR)(LPCTSTR)pVar->m_strName);
				::SendMessage(hWndCtrl, CB_SETITEMDATA, nIndex, ID_USER_DEFINED);
					
			}
		}
	}
#else  // end commented out routine body
	// instead, since we know we are being called on a time list after basic times filled in,
	// take the opportunity here to ensure default time is selected if only one time in problem.
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	if (!pDX->m_bSaveAndValidate) {
		if (::SendMessage(hWndCtrl, CB_GETCOUNT, 0, 0) == 1) // only one possible choice
		{
			::SendMessage(hWndCtrl, CB_SETCURSEL, 0, 0);	 // select index 0

			// might also disable window since value is effectively static.
			CWnd* pDlg = pDX->m_pDlgWnd;
			/* SetComboReadOnly doesn't work well because it replaces existing control with
			a new one in the same place on dialog, but this process doesn't preserve tab order.
			But adjustments to dialog for special uses based on marker static boxes depend crucially on tab order to figure
			out which controls should move up when a box is removed, so they are messed up in
			this case. 
			if (pDlg && pDlg->IsKindOf(RUNTIME_CLASS(CDrawObjDlg)))
				((CDrawObjDlg*)pDlg)->SetComboReadOnly((CComboBox*) pDlg->GetDlgItem(nIDC) );
			*/
			//::EnableWindow(hWndCtrl, FALSE);
	  }

	}
#endif !0
}

void AFXAPI DDX_AddCompoundBodies(CDataExchange* pDX, int nIDC, CDrawObjList* pObjList)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);

	if (!pDX->m_bSaveAndValidate)
	{
		// Run through list so more recently added objects come later, adding compound
        // body system names to list of possible body choices. Also, for each defined
        // system of any kind, select it's body name as we walk list, so most recently 
		// defined system's body name is left as default selection when we're done.
		POSITION pos = pObjList->GetHeadPosition();
		while (pos != NULL)
		{
			CDrawObj* pObj = pObjList->GetNext(pos);
			if (pObj->m_flag == TEACHER_OBJECT)
				continue;
			if (  pObj->IsKindOf(RUNTIME_CLASS(CSystem)) &&	! pObj->m_strName.IsEmpty() )
			{
				CSystem* pSys = (CSystem*)pObj;
				
				if (pSys->m_nSystemType == SYSTEM_COMPOUND_BODY) 
				{
					// Add compound-body labels to list of choices in combo box
					::SendMessage(hWndCtrl, CB_ADDSTRING, 0, (LPARAM)(LPSTR)(LPCTSTR)pSys->m_strName);
					
					// Select compound body choice by exact system label
					int nSel = ::SendMessage(hWndCtrl, CB_FINDSTRINGEXACT, -1, 
										(LPARAM)(LPCTSTR)pSys->m_strName);
					if (nSel >= 0) ::SendMessage(hWndCtrl, CB_SETCURSEL, nSel, 0L);
		
				} 
				else // Single body system 
				{
					// Select body choice by exact name of underlying body
					// NB: *not* system label -- that's not on list in this case.
					int nSel = ::SendMessage(hWndCtrl, CB_FINDSTRINGEXACT, -1, 
										(LPARAM)(LPCTSTR)pSys->m_strBodies);
					if (nSel >= 0) ::SendMessage(hWndCtrl, CB_SETCURSEL, nSel, 0L);
				}
			}
		}		
	}
}

void AFXAPI DDX_AddEquivComponents(CDataExchange* pDX, int nIDC, CVarList * pObjList)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);

	if (!pDX->m_bSaveAndValidate)
	{
		POSITION pos = pObjList->GetTailPosition();
		while (pos != NULL)
		{
			CVariable* pVar = pObjList->GetPrev(pos);
			// if we have a compound equivalent resistor, add it to choices.
			if ( (pVar->m_nType == ID_VARIABLE_ADDRESISTANCE) &&	
				 (pVar->m_strForceType == "equiv") &&
				 ! pVar->m_strName.IsEmpty() )
			{
				// Add compound-body labels to list of choices in combo box
				::SendMessage(hWndCtrl, CB_ADDSTRING, 0, (LPARAM)(LPSTR)(LPCTSTR)pVar->m_strName);
					
			}
		}
	}
}

void AFXAPI DDX_AddFieldVectors(CDataExchange* pDX, int nIDC, CDrawObjList* pObjList)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);

	if (!pDX->m_bSaveAndValidate)
	{
		// Run through list so more recently added objects come later, adding compound
        // body system names to list of possible body choices. Also, for each defined
        // system of any kind, select it's body name as we walk list, so most recently 
		// defined system's body name is left as default selection when we're done.
		POSITION pos = pObjList->GetHeadPosition();
		while (pos != NULL)
		{
			CDrawObj* pObj = pObjList->GetNext(pos);
			if (pObj->m_flag == TEACHER_OBJECT)
				continue;
			if (  pObj->IsKindOf(RUNTIME_CLASS(CVector)) &&	! pObj->m_strName.IsEmpty() )
			{
				CVector* pVec= (CVector*)pObj;		
				if (pVec->m_nVectorType == VECTOR_EFIELD || pVec->m_nVectorType == VECTOR_BFIELD) 
				{
					// Add vector label to list of choices in combo box
					::SendMessage(hWndCtrl, CB_ADDSTRING, 0, (LPARAM)(LPSTR)(LPCTSTR)pVec->m_strName);
					
					// Select it by exact label
					int nSel = ::SendMessage(hWndCtrl, CB_FINDSTRINGEXACT, -1, 
										(LPARAM)(LPCTSTR)pVec->m_strName);
					if (nSel >= 0) ::SendMessage(hWndCtrl, CB_SETCURSEL, nSel, 0L);
		
				} 
			}
		}		
	}
}

