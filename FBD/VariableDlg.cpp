// VariableDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "VariableDlg.h"
#include "VarView.h"

#include "PlanStrs.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVariableDlg dialog
//
// This dialog is used to define variables for scalar quantities whose form is not 
// special enough to require their own dialog. The default dialog has three possible 
// slots for the definition: body, time, and "agent" or other body. 
// Special dialogs are used for other quantities, such as radius
// radius, angle and some circuit variables, see CVariable::GetPropertyDlg.
//
// Information about quantities is in tables in CVarView, including the display
// name of the quantity and the dialog spec information.
// A dialog spec is a string of the form 
//      "label1 [slot1name:choicelist1] label2 [slot2name:choicelist2]..."
// where slotnames are "body", "body2", "time" and choicelist names refer to lists
// in the problem "bodies" "times" "positions" "branches". Plan to add extensibility to
// arbitrary slotnames and choicelists later.


CString CVariableDlg::LookupCtrlName(int nID)
{
	CString str = CVarView::LookupDlgValue(nID);
	if (! str.IsEmpty())
		str.SetAt(0, toupper(str[0]));

	return str;
}

CString CVariableDlg::GetSpec()		// get dialog spec for 
{	
	CString strSpec = CVarView::LookupSpec(((CVariable*)m_pTempObj)->m_nType); 
	// Ugly special case: if changing-mass feature is set, then mass variable uses
	// time slot. Adjust spec so other code works without change
	CString strTypeId = CVarView::LookupTypeId(((CVariable*)m_pTempObj)->m_nType);
	if (strTypeId.CompareNoCase("mass") != -1) {
		if (CVarView::IncludeQuant(CString("CHANGING-MASS"))){
			strSpec += " at time [time:times]";
		}
	}
	return strSpec;
}

// helpers to extract info from dialog spec string
BOOL CVariableDlg::UsesSlot(const CString strSpec, const CString &strSlot)
{
	return (strSpec.Find("[" + strSlot) != -1); 
}

extern int split(const CString& str, const CString& Sep, CStringArray& result);

CString CVariableDlg::GetSlotLabel(const CString &strSpec, const CString &strSlot)
{
	// split spec into array of slot records for simpler processing
	CStringArray strSlots;
	int nSlots = split(strSpec, "]", strSlots);
	for (int i = 0; i < nSlots; i++) 
	{
		CString strThisSlot = strSlots[i];
		int iLBracket = strThisSlot.Find("[" + strSlot);
		if (iLBracket != -1) {
			CString strResult =strThisSlot.Left(iLBracket);
			strResult.TrimRight();
			return strResult;
		}
	}
    return "";
}

CString CVariableDlg::GetSlotChoices(const CString &strSpec, const CString &strSlot)
{
	// extract choices from "... [slotname:choices] ....
	CString strSearch = "[" + strSlot + ":";
	int iLBracket = strSpec.Find(strSearch);
	if (iLBracket == -1)  return "";
	int iStart = iLBracket + strSearch.GetLength(); // 1 past colon
	int iRBracket = strSpec.Find("]", iLBracket);
	if (iRBracket == -1) return "";
	return strSpec.Mid(iStart, iRBracket - iStart);
}

CVariableDlg::CVariableDlg(CDrawObj* pObj , CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CVariableDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CVariableDlg)
	//}}AFX_DATA_INIT
}


void CVariableDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVariableDlg)
	DDX_Control(pDX, IDC_STATIC_EQUALS, m_stcEquals);
	DDX_Control(pDX, IDC_GIVEN_BOX, m_stcGiven);
	DDX_Control(pDX, IDC_STATIC_OR, m_stcOr);
	DDX_Control(pDX, IDC_CHECK_UNKNOWN, m_btnUnknown);
	DDX_Control(pDX, IDC_GIVEN_VALUE, m_editValue);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_TXT0, m_stcLet);
	DDX_Control(pDX, IDC_STATIC_BODY, m_stcBody);
	DDX_Control(pDX, IDC_STATIC_DUETO, m_stcDueTo);
	DDX_Control(pDX, IDC_STATIC_TIME, m_stcTime);
	DDX_Control(pDX, IDC_TIME, m_cboTime);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDC_BODY, m_cboBody);
	DDX_Control(pDX, IDC_FORCEAGENT, m_cboAgent);
	DDX_Control(pDX, IDC_STATIC_VALUE, m_stcValue);
	//}}AFX_DATA_MAP

	// fill "body" with appropriate choices for quantity
	if (((CVariable*)m_pTempObj)->m_nType == ID_VARIABLE_ADDTIME)
		DDX_FillList(pDX, IDC_BODY, &m_pDocument->m_strTimes);
	else {
		// get choice list spec from dialog spec. 
	    CString strListSpec = GetSlotChoices(GetSpec(), "body");
		// Allow for union of lists as list1+list2+list3
		CStringArray strLists;
		split(strListSpec, "+", strLists);
		for (int i = 0; i < strLists.GetSize(); i++) {
			CString strListName = strLists[i];
			DDX_FillList(pDX, IDC_BODY, m_pDocument->GetChoiceList(strListName));
			if (strListName.CompareNoCase("bodies") == 0)
				DDX_AddCompoundBodies(pDX, IDC_BODY, &m_pDocument->m_objects);
		}
	}
	// fill "agent" slot with appropriate choices (may wind up hidden)
	DDX_FillList(pDX, IDC_FORCEAGENT, &m_pDocument->m_strObjects);
	// !!! must add special agent args where needed, see below

	// file time slot with appropriate choices for quantity
	DDX_FillList(pDX, IDC_TIME, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME, &m_pDocument->m_Variables);

	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CVariableDlg)
	"name", IDC_CUSTOM_LABEL,
	"type", IDC_STATIC_VALUE,
	"body", IDC_BODY,
	"time", IDC_TIME,
	"agent", IDC_FORCEAGENT,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
	// to interpret old long names for backwards compatibility:
	"variable-name", IDC_CUSTOM_LABEL,
	"variable-type", IDC_STATIC_VALUE,
	"variable-object", IDC_BODY,
	"variable-time", IDC_TIME,
	"variable-agent", IDC_FORCEAGENT,
END_CTL_TBL(CVariableDlg)

BEGIN_MESSAGE_MAP(CVariableDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CVariableDlg)
	ON_BN_CLICKED(IDC_CHECK_UNKNOWN, OnCheckUnknown)
	ON_EN_CHANGE(IDC_GIVEN_VALUE, OnChangeGivenValue)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVariableDlg message handlers

BOOL CVariableDlg::OnInitDialog() 
{
	CDrawObjDlg::OnInitDialog();
	LogEventf(EV_VAR_DLG, "");
	
	// get type code of quantity
	int nType = ((CVariable*)m_pTempObj)->m_nType;

	// adjust dialog for special-purpose uses: to define a sought only (no label)
	// or to define a property for the HiLevel plan (now unused)
	if (m_bSought || m_bProp){
		// remove the whole label box from dialog
		Remove(IDC_BOX_LABEL);
		// hide the label controls
		m_stcLet.ShowWindow(SW_HIDE);
		m_editName.ShowWindow(SW_HIDE);
		// hide the given value controls as well
		//m_stcGiven.ShowWindow(SW_HIDE);
		//m_stcEquals.ShowWindow(SW_HIDE);
		//m_editValue.ShowWindow(SW_HIDE);
		//m_stcOr.ShowWindow(SW_HIDE);
		//m_btnUnknown.ShowWindow(SW_HIDE);
	
		if (m_bSought){
			SetWindowText("Define Sought");
		} else {
			SetWindowText("Define Property");		
		}
		if 	(nType != ID_VARIABLE_ADDTIME) {
			UpdatePlanStrings(&m_cboBody);
		}
	}
		
	// Adjust controls and labels based on main quantity type
	CString strTypeId = CVarView::LookupTypeId(nType);
	CString strDlgValue = LookupCtrlName(nType);
	CString strSpec = GetSpec();

	// set initial type label:
	m_stcValue.SetWindowText(strDlgValue);

	// duration dialog has special case argument layout
	if (strTypeId.CompareNoCase("duration") == 0) // defining time duration
	{
		//remove time intervals from choices
		RemoveTimePeriods(&m_cboBody);
		RemoveTimePeriods(&m_cboTime);
		
		//adjust prepositions and reorganize dialog
		m_stcBody.SetWindowText("from");
		m_stcTime.SetWindowText("to");
		CRect rect1, rect2, rect3, rect4;
		m_stcBody.GetWindowRect(rect1);
		ScreenToClient(&rect1);
		m_cboBody.GetWindowRect(rect2);
		ScreenToClient(&rect2);
		m_stcTime.GetWindowRect(&rect3);
		ScreenToClient(&rect3);
		m_cboTime.GetWindowRect(&rect4);
		ScreenToClient(&rect4);

		m_stcTime.SetWindowPos(NULL, rect1.left, rect3.top, 0, 0,
			SWP_NOSIZE | SWP_NOZORDER );
		m_cboTime.SetWindowPos(NULL, rect2.left, rect4.top, 0, 0,
			SWP_NOSIZE | SWP_NOZORDER );
	}

		
	// If no times in problem or time unused by quant, hide time window, 
	if (m_pDocument->m_strTimes.IsEmpty() || ! UsesSlot(strSpec, "time"))
	{
		m_cboTime.ShowWindow(SW_HIDE);
		m_stcTime.ShowWindow(SW_HIDE);

		// if agent" choice unused by quant, take out whole time+agent line.
		if (! UsesSlot(GetSpec(), "body2"))
			Remove(IDC_BOX_TIME);
	}
    
	// set body label
	if (UsesSlot(strSpec, "body")) 
		m_stcBody.SetWindowText(GetSlotLabel(strSpec, "body"));
	else { // hide unused body choice
		m_cboBody.ShowWindow(SW_HIDE);
		m_stcBody.ShowWindow(SW_HIDE);
	}

	// show second body controls if body2 slot is used.
	if (UsesSlot(strSpec, "body2")) {
		m_stcDueTo.SetWindowText( GetSlotLabel(strSpec, "body2"));
		m_stcDueTo.ShowWindow(SW_SHOW);
		m_cboAgent.ShowWindow(SW_SHOW);
		SwitchTimeAgent();
	}

	// UGH insert special choices for certain quantities. Should be in spec somehow
	if (strTypeId.CompareNoCase("work") == 0) 
	{
		// add special case agent choice for Net work and Wnc
		m_cboAgent.InsertString(0, CVariable::c_szAllForces);
		m_cboAgent.InsertString(1, CVariable::c_szNCForces);
		// Since the AllForces choice wasn't in the list earlier, the first attempt to
		// to initialize value in InitVariableDlg called from CDrawObj::OnInitDialog above
		// may have failed. So do it again now that all choices are there.
		m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent);
	}
	else if (strTypeId.CompareNoCase("power") == 0) 
	{
		// add special case agent choice for Net power
		m_cboAgent.InsertString(0, CVariable::c_szAllForces);
		// Since the AllForces choice wasn't in the list earlier, the first attempt to
		// to initialize value in InitVariableDlg called from CDrawObj::OnInitDialog above
		// may have failed. So do it again now that all choices are there.
		m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent);
	}
	else if (strTypeId.CompareNoCase("potential") == 0) 
	{
		// add special case agent choice for Net potential
		// only enable if there is more than one body OR energy is involved (since Vnet
		// is referenced in Ue equation)
		if ((m_pDocument->m_strObjects.GetCount() > 1) || 
			(m_pDocument->m_wConcept & ID_PROB_ENERGY)) 
		    m_cboAgent.InsertString(0, CVector::c_szAllSources);
		// Since the AllForces choice wasn't in the list earlier, the first attempt to
		// to initialize value in InitVariableDlg called from CDrawObj::OnInitDialog above
		// may have failed. So do it again now that all choices are there.
		m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent);
	} 
	else if (strTypeId.CompareNoCase("intensity") == 0 ||
		     strTypeId.CompareNoCase("db-intensity") == 0) {
		m_cboAgent.InsertString(0, CVector::c_szAllSources);
		// Since the AllForces choice wasn't in the list earlier, the first attempt to
		// to initialize value in InitVariableDlg called from CDrawObj::OnInitDialog above
		// may have failed. So do it again now that all choices are there.
		m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent);
	}

	// Move top-line static and combo box controls so that they line up nicely without
	// all that empty space between controls.  
	if (strTypeId.CompareNoCase("duration") != 0) // time layout was customized above
	{
		// Place body label just right of end of sized-to-fit m_stcValue Label
		(void) MoveToRightOf(SizeToFit(m_stcValue), m_stcBody);
		// Place body combo just right of end of sized to fit body label
		(void) MoveToRightOf(SizeToFit(m_stcBody), m_cboBody);
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// helpers for control layout. Return new client area position
CRect CVariableDlg::SizeToFit(CWnd& wnd)  // adjusts width only
{
	CRect posWnd;
	wnd.GetWindowRect(&posWnd);
	CClientDC dc(NULL);
	CString strText;
	wnd.GetWindowText(strText);
	CSize size = dc.GetTextExtent(strText);
	wnd.SetWindowPos(0, 0, 0, size.cx, posWnd.Height(), SWP_NOMOVE|SWP_NOZORDER);
	// return its revised position in client area coords
	wnd.GetWindowRect(&posWnd); 
	ScreenToClient(&posWnd);
	return posWnd;
}

CRect CVariableDlg::MoveToRightOf(CRect posWndLeft, CWnd& wnd, int nMargin/*=5*/)
{
	CRect posWnd;
	wnd.GetWindowRect(&posWnd); ScreenToClient(&posWnd);
	wnd.SetWindowPos(0, posWndLeft.right+nMargin, posWnd.top, 0, 0, SWP_NOSIZE|SWP_NOZORDER);
	// return its revised position in client area coords
	wnd.GetWindowRect(&posWnd); 
	ScreenToClient(&posWnd);
	return posWnd;
}

// Switch time and agent slot positions.
// Second line in default template has "At time [time] due to [agent]", so agent slot can just
// be hidden if not used. But if agent *is* used we want agent slot to come first, so we swap 
// the default positions, putting agent first and time second. We now also lay out 
// controls based on size of text, so they line up nicely, as we do for top-line controls above.
void CVariableDlg::SwitchTimeAgent()
{
	// get initial top left of "At Time" label
	CRect rectAt;
	m_stcTime.GetWindowRect(&rectAt);
	ScreenToClient(&rectAt);
	// Position "Due To?" label at top-left of At-Time label
	m_stcDueTo.SetWindowPos(NULL, rectAt.left, rectAt.top, 0, 0, SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE);
	// place Agent combo after sized-to-fit "Due To" Label
	CRect rectAgent = MoveToRightOf(SizeToFit(m_stcDueTo), m_cboAgent);
	// place At Time label after agent combo
	(void) MoveToRightOf(rectAgent, m_stcTime);
	// place Time combo box after sized to fit "At Time" Label
	(void) MoveToRightOf(SizeToFit(m_stcTime), m_cboTime);
}

void CVariableDlg::InitVariableDlg()
{
	m_cboBody.SelectStringExact(((CVariable*)m_pTempObj)->m_strObject );
	m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent );
	m_cboTime.SelectStringExact(((CVariable*)m_pTempObj)->m_strTime) ;
	// Transfer given value/unknown bit from controls to variable
	m_editValue.SetWindowText(((CVariable*)m_pTempObj)->m_strValue);
	OnChangeGivenValue();	// to sync unknown check box with value
}

CLabelRichEdit* CVariableDlg::GetLabelCtrl()
{
	return &m_editName;
}

void CVariableDlg::UpdateTempVariable()
{
	((CVariable*)m_pTempObj)->m_strObject = GetCurString(&m_cboBody);
	((CVariable*)m_pTempObj)->m_strAgent = GetCurString(&m_cboAgent);
	((CVariable*)m_pTempObj)->m_strTime = GetCurString(&m_cboTime);
	m_editValue.GetWindowText(((CVariable*)m_pTempObj)->m_strValue);	
	m_editName.GetRichEditText(m_pTempObj->m_strName);
}


void CVariableDlg::OnOK() 
{
	// Ensure dialog complete
	if (IsEmpty(&m_cboBody))
	{
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboAgent))
	{
		theApp.DoWarningMessage("Please select an agent", this);
		return;
	}
	if (IsEmpty(&m_cboTime))
	{
		theApp.DoWarningMessage("Please select a time", this);
		return;
	}

	//Update data into temporary object
	UpdateTempVariable();
	BuildVariable();

	if (m_bProp)
	{
		CTableRow* pThisProp = (CTableRow*)m_pTempObj;
		if (pThisProp->m_pTable){
			CTableRow* pProp = pThisProp->m_pTable->GetMatchingProp(pThisProp->m_strDef);
			if (pProp && (pThisProp->m_strId != pProp->m_strId))
			{
				CString str;
				str.Format(IDS_PROP_EXISTS, pThisProp->m_strDef);
				theApp.DoInfoMessage(str);
				return;
			}
		}

	}
	else if (!m_bSought)
	{
		CString str = m_pTempObj->m_strName;
		str.Remove('$');
		
		if (!IsValidLabel(str))	return;
		
		if (!m_pTempObj->IsValid()) return;
	}

	if (!CheckDialog())	return;
	
	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();

}


// builds variable definition string and sets into m_strDef in the var.
void CVariableDlg::BuildVariable()
{
	// Cast for more convenient reference:
	CVariable* m_pTempVar = (CVariable*) m_pTempObj;

	// Following code extracts prepositions to use in variable definition string
	// from static labels currently set in dialog. 
	// !!! Ugh, should be better way to do this
	CString strDlgValue, strBodyPrep, strAgentPrep, strTimePrep;
	
	// Get body label if non-empty
	if (m_stcBody.IsWindowVisible())
		m_stcBody.GetWindowText(strBodyPrep);
	// include spaces around body prep word
	if (!strBodyPrep.IsEmpty())
		strBodyPrep = " " + strBodyPrep + " ";
	
	// Get agent label if non-empty
	if (m_stcDueTo.IsWindowVisible() && m_stcDueTo.IsWindowEnabled())
		m_stcDueTo.GetWindowText(strAgentPrep);
	// skip possible initial & in agent label
	int startPos = 0;
	if (strAgentPrep.Find("&") != -1)
		startPos = 1;
	strAgentPrep = strAgentPrep.Mid(startPos);
	if (!strAgentPrep.IsEmpty())
		strAgentPrep = " " + strAgentPrep + " ";

	// Get time preposition if time used.
	if (m_stcTime.IsWindowVisible())
	{
		if (m_pTempVar->m_nType == ID_VARIABLE_ADDTIME)
			strTimePrep = " to ";
		else
			strTimePrep = " at time ";
	}

	// set variable's quantity type name ("value") from type code
	// Note for historical reasons, this is the human-readable name which may contain spaces, 
	// not the atomic quantTypeId symbol used helpsys communications and logs.
	// !!! Might be better to use the Id to store in file, since that really functions
	// as a type code in our system. As it stands, this requires human-readable names to be 
	// unique and persistent across versions, since they function as type codes when
	// deserializing a variable (since integer type codes are now not persistent).
	m_pTempVar->m_strQuantName = CVarView::LookupStrValue(m_pTempVar->m_nType);

	// build the definition string displayed in var window and printout. Note this
	// shows language as it reads in dialog box, which may have different "Value"
	// for readability. (We fetch it again without the initial upper-casing done by
	// LookupCtrlName)
	m_pTempVar->m_strDef = CVarView::LookupDlgValue(m_pTempVar->m_nType) +  
						   strBodyPrep + m_pTempVar->m_strObject + 
						   strAgentPrep + m_pTempVar->m_strAgent +
						   strTimePrep + m_pTempVar->m_strTime;
}


/////////////////////////////////////////////////////////////////////////////
// CVarTypeDlg dialog


CVarTypeDlg::CVarTypeDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CVarTypeDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CVarTypeDlg)
	m_nType = 0;
	//}}AFX_DATA_INIT
}


void CVarTypeDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVarTypeDlg)
	DDX_Radio(pDX, IDC_RADIO1, m_nType);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CVarTypeDlg, CDialog)
	//{{AFX_MSG_MAP(CVarTypeDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVarTypeDlg message handlers



// Keep edit control contents and unknown check box in sync:
// blank string <=> unknown checked
void CVariableDlg::OnCheckUnknown() 
{
	if (m_btnUnknown.GetCheck()) {
		m_editValue.SetWindowText("");
	} else {
		m_editValue.SetFocus();
	}
}

void CVariableDlg::OnChangeGivenValue() 
{
	// TODO: If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDrawObjDlg::OnInitDialog()
	// function and call CRichEditCtrl().SetEventMask()
	// with the ENM_CHANGE flag ORed into the mask.
	
	CString strText;
	m_editValue.GetWindowText(strText);
	strText.Remove(' ');
	m_btnUnknown.SetCheck(strText.IsEmpty());
}
