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
	CString str = CVarView::LookupStrValue(nID);
	if (! str.IsEmpty())
		str.SetAt(0, toupper(str[0]));

	return str;
}

CString CVariableDlg::GetSpec()		// get dialog spec for 
{	
	return CVarView::LookupSpec(((CVariable*)m_pTempObj)->m_nType); 
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
		// get choice list name from dialog spec
	    CString strListName = GetSlotChoices(GetSpec(), "body");
		DDX_FillList(pDX, IDC_BODY, m_pDocument->GetChoiceList(strListName));
		if (strListName.CompareNoCase("bodies") == 0)
			DDX_AddCompoundBodies(pDX, IDC_BODY, &m_pDocument->m_objects);
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
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVariableDlg message handlers

BOOL CVariableDlg::OnInitDialog() 
{
	CString strCtrlName;		// Label to display for quantity type name

	CDrawObjDlg::OnInitDialog();
	LogEventf(EV_VAR_DLG, "");
	
	// get type code of quantity
	int nType = ((CVariable*)m_pTempObj)->m_nType;

	// adjust dialog for special-purpose uses: to define a sought only (no label)
	// or to define a property for the HiLevel plan (now unused)
	if (m_bSought || m_bProp){
		m_stcLet.ShowWindow(SW_HIDE);
		m_editName.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_LABEL);
		if (m_bSought){
			SetWindowText("Define Sought");
		} else {
			SetWindowText("Define Property");		
		}
		if 	(nType != ID_VARIABLE_ADDTIME) {
			UpdatePlanStrings(&m_cboBody);
		}
	}
	
	// Set main quantity type label. 
	strCtrlName = LookupCtrlName(nType);	
	m_stcValue.SetWindowText(strCtrlName);
	
	// Adjust controls and labels based on main quantity type
	CString strSpec = GetSpec();

	// time is special case:
	if (strCtrlName.CompareNoCase("duration of time") == 0) // defining time duration
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

	// show second body controls if body2 slot is used.
	if (UsesSlot(strSpec, "body2")) {
		m_stcDueTo.SetWindowText( GetSlotLabel(strSpec, "body2"));
		m_stcDueTo.ShowWindow(SW_SHOW);
		m_cboAgent.ShowWindow(SW_SHOW);
		SwitchTimeAgent();
	}

	// UGH insert special choices for certain quantities. Should be in spec somehow
	if (strCtrlName.CompareNoCase("work") == 0) 
	{
		// add special case agent choice for Net work and Wnc
		m_cboAgent.InsertString(0, CVariable::c_szAllForces);
		m_cboAgent.InsertString(1, CVariable::c_szNCForces);
		// Since the AllForces choice wasn't in the list earlier, the first attempt to
		// to initialize value in InitVariableDlg called from CDrawObj::OnInitDialog above
		// may have failed. So do it again now that all choices are there.
		m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent);
	}
	else if (strCtrlName.CompareNoCase("power") == 0) 
	{
		// add special case agent choice for Net power
		m_cboAgent.InsertString(0, CVariable::c_szAllForces);
		// Since the AllForces choice wasn't in the list earlier, the first attempt to
		// to initialize value in InitVariableDlg called from CDrawObj::OnInitDialog above
		// may have failed. So do it again now that all choices are there.
		m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent);
	}
	else if (strCtrlName.CompareNoCase("potential") == 0) 
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

	// Move top-line static and combo box controls so that they line up nicely without
	// all that empty space between controls.  Will make more elegant later
	if (strCtrlName.CompareNoCase("duration of time")) // time layout customized above
	{
		CClientDC dc(NULL);
		CSize size = dc.GetTextExtent(strCtrlName);
		CRect pos, posBdyStc, posBdyCbo;
		// resize m_stcValue to fit its text:
		m_stcValue.GetWindowRect(&pos);
		m_stcValue.SetWindowPos(0, 0, 0, size.cx, pos.Height(), SWP_NOMOVE|SWP_NOZORDER);
		// fetch its revised client coords:
		m_stcValue.GetWindowRect(&pos); ScreenToClient(&pos);
		// Size body label static to fit its text
		CString strBodyLabel;
		m_stcBody.GetWindowText(strBodyLabel);
		size = dc.GetTextExtent(strBodyLabel);
		m_stcBody.GetWindowRect(&posBdyStc);
		m_stcBody.SetWindowPos(0, 0, 0, size.cx, posBdyStc.Height(), SWP_NOMOVE|SWP_NOZORDER);
		// fetch its revised client coords
		m_stcBody.GetWindowRect(&posBdyStc); ScreenToClient(&posBdyStc);
		// move body label just right of end of m_stcValue
		m_stcBody.SetWindowPos(0, pos.right+5, posBdyStc.top, 0, 0, SWP_NOSIZE|SWP_NOZORDER);
		// fetch its revised client coords
		m_stcBody.GetWindowRect(&posBdyStc); ScreenToClient(&posBdyStc);
		// move body combo just right of end of body label
		m_cboBody.GetWindowRect(&posBdyCbo); ScreenToClient(&posBdyCbo);
		m_cboBody.SetWindowPos(0, posBdyStc.right+5, posBdyCbo.top, 0, 0, SWP_NOSIZE|SWP_NOZORDER);
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CVariableDlg::InitVariableDlg()
{
	m_cboBody.SelectStringExact(((CVariable*)m_pTempObj)->m_strObject );
	m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent );
	m_cboTime.SelectStringExact(((CVariable*)m_pTempObj)->m_strTime) ;
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
	
	CString str;
	m_editName.GetRichEditText(str);
	m_pTempObj->m_strName = str;
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


// Switch time and agent slot positions.
// Second line in template has time then agent [=body2]. If agent not used we just 
// hide it.But if agent *is* used we want agent slot to come first, so we swap 
// the default positions, putting agent first and time second.
void CVariableDlg::SwitchTimeAgent()
{
	CRect rectTime, rectAt, rectAgent, rectDueTo;
	m_cboTime.GetWindowRect(&rectTime);
	m_stcTime.GetWindowRect(&rectAt);
	m_cboAgent.GetWindowRect(&rectAgent);
	m_stcDueTo.GetWindowRect(&rectDueTo);
	ScreenToClient(&rectTime);
	ScreenToClient(&rectAgent);
	ScreenToClient(&rectDueTo);
	ScreenToClient(&rectAt);
	m_cboAgent.SetWindowPos(NULL, rectTime.left, rectTime.top, 0, 0, SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE);
	m_stcDueTo.SetWindowPos(NULL, rectAt.left, rectAt.top, 0, 0, SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE);
	m_cboTime.SetWindowPos(NULL, rectAgent.left, rectAgent.top, 0, 0, SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE);
	m_stcTime.SetWindowPos(NULL, rectDueTo.left, rectDueTo.top, 0, 0, SWP_NOZORDER|SWP_NOSIZE|SWP_NOACTIVATE);
}

// builds variable definition string and sets into m_strDef in the var.
void CVariableDlg::BuildVariable()
{
	// Cast for more convenient reference:
	CVariable* m_pTempVar = (CVariable*) m_pTempObj;

	// Following code extracts prepositions to use in variable definition string
	// from static labels currently set in dialog. 
	// !!! Ugh, should be better way to do this
	CString strBodyPrep, strAgentPrep, strTimePrep;

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
	// Note this is internal name, not friendly display name
	m_pTempVar->m_strValue = CVarView::LookupStrValue(m_pTempVar->m_nType);

	// build the definition string. (Unused slots will be empty strings).
	m_pTempVar->m_strDef = m_pTempVar->m_strValue + 
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



