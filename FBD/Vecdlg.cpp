// VecDlg.cpp : implementation file
// 
// 

#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "Motion.h"			// For vectors in motion diagrams
#include "VecDlg.h"
#include "SysDlg.h"			// For SYSTEM_SINGLE_BODY defines 

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVectorDlg dialog
//
// Note that this dialog encapsulates a pointer to a vector object,
// (and through it can get to the document).
// So it can use the document and object state to initialize control 
// fields in the dialog.
//
////////////////////////////////////////////////////////////////////////////
CVectorDlg::CVectorDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CVectorDlg::IDD, pObj, pParent)
{
	m_nType = 1;
	//{{AFX_DATA_INIT(CVectorDlg)
	//}}AFX_DATA_INIT
}


void CVectorDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVectorDlg)
	DDX_Control(pDX, IDC_STC_GROUP, m_stcGroup);
	DDX_Control(pDX, IDC_NETBTN, m_btnNet);
	DDX_Control(pDX, IDC_FORCEBTN, m_btnForce);
	DDX_Control(pDX, IDC_LVECTOR_NAME_TEXT, m_stcLet);
	DDX_Control(pDX, IDC_COMP, m_stcComp);
	DDX_Control(pDX, IDC_ANGLE, m_stcVecAng);
	DDX_Control(pDX, IDC_LANGLE, m_stcVecAng1);
	DDX_Control(pDX, IDC_L2ANGLE, m_stcVecAng2);
	DDX_Control(pDX, IDC_TIME_TEXT, m_cboTimeList);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_FORCE_TYPE, m_cboForceType);
	DDX_Control(pDX, IDC_BODY_TEXT, m_cboBodyList);
	DDX_Control(pDX, IDC_AGENT, m_cboAgentList);
	DDX_Control(pDX, IDC_ORIENTATION_OLD, m_editOrientation);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDirection);
	//}}AFX_DATA_MAP

	DDX_Radio(pDX, IDC_NETBTN, m_nType);
	// Fill dialog boxes with data from doc (string lists)
	DDX_FillList(pDX, IDC_AGENT, &m_pDocument->m_strObjects);
	// add "Unspecified" choice for force agent as well, to handle given forces
	// will make into DDX_FillAgent routine later.
	if (! pDX->m_bSaveAndValidate) 
		m_cboAgentList.AddString("Unspecified");
	DDX_FillList(pDX, IDC_BODY_TEXT, &m_pDocument->m_strObjects);
	DDX_FillList(pDX, IDC_TIME_TEXT, &m_pDocument->m_strTimes);
	DDX_AddCompoundBodies(pDX, IDC_BODY_TEXT, &m_pDocument->m_objects);
	DDX_AddUserTimes(pDX, IDC_TIME_TEXT, &m_pDocument->m_Variables);
	// Customize force types if loading
	if (! pDX->m_bSaveAndValidate)
		AdjustForceTypes();

	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CVectorDlg)
	"name",		IDC_CUSTOM_LABEL,
	"time",		IDC_TIME_TEXT,
	"type",		IDC_FORCE_TYPE,
	"body",		IDC_BODY_TEXT,
	"agent",	IDC_AGENT,
	"dir",		IDC_ORIENTATION_TEXT,
	"net",      IDC_NETBTN,
	"individual",IDC_FORCEBTN,
	// from value control
	"compo", IDC_COMPO_BTN,
	"magdir", IDC_MAGDIR_BTN,
	"given-mag",IDC_MAG_VALUE,
	"given-xc", IDC_XC_VALUE,
	"given-yc", IDC_YC_VALUE,
	"given-zc", IDC_ZC_VALUE,
	"OK",		IDOK,
	"Cancel",	IDCANCEL,

	// read old long names for backwards compatibility:
	"vector-name",  IDC_CUSTOM_LABEL,
	"vector-time",	IDC_TIME_TEXT,
	"force-type",	IDC_FORCE_TYPE,
	"force-body",	IDC_BODY_TEXT,
	"force-dueto",	IDC_AGENT,
	"orientation",	IDC_ORIENTATION_TEXT,
	// to replay var def ctl events from old logs which used generic variable dialog: 
	// also map relevant ctl names from generic var dcl dialog to our ctls
//	"variable-name", IDC_VECTOR_NAME_TEXT,
//	"variable-time", IDC_TIME_TEXT,
//	"variable-object", IDC_AVBODY_TEXT,
//	"variable-agent", IDC_AGENT, 
END_CTL_TBL(CVectorDlg)

BEGIN_MESSAGE_MAP(CVectorDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CVectorDlg)
	ON_EN_CHANGE(IDC_CUSTOM_LABEL, OnChangeVectorNameText)
	ON_BN_CLICKED(IDC_NETBTN, OnNetbtn)
	ON_BN_CLICKED(IDC_FORCEBTN, OnForcebtn)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CVectorDlg message handlers

BOOL CVectorDlg::OnInitDialog() 
{
	LogEventf(EV_VECTOR_DLG, "%s |%s|",m_pObj->m_strId, OBJ_NAME(m_pObj));

	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVector)) && ! m_bSought) {
			// create vector value sub-dialog and place it
			m_pDlgValues = new CValueDlg((CVector*)m_pTempObj, this);
			m_pDlgValues->Create(CValueDlg::IDD, this);
			CRect rcValues;
			GetDlgItem( IDC_STATIC_PLACEHOLDER)->GetWindowRect( &rcValues );
			ScreenToClient(rcValues);
			m_pDlgValues->SetWindowPos( NULL, rcValues.left + 7, rcValues.top + 7, 0, 0,
				SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE | SWP_SHOWWINDOW );
			
			// Hide old direction controls
			Remove(IDC_BOX_TIMEDIR);
			
	} else {
		Remove(IDC_STATIC_PLACEHOLDER);
		m_pDlgValues = NULL;
	}
	
	// Base class will load mapped data member values 
	// into controls via DDX and bind control member vars.
	// Initializes the droplist control values
	CDrawObjDlg::OnInitDialog();
	
	// Set slot visibility from document & object state
	if (m_pDocument == NULL) 
		return TRUE;	// shouldn't happen!

	// Shouldn't happen; don't use forces in motion diagrams.
	ASSERT(!m_pObj->IsKindOf(RUNTIME_CLASS(CMDVector)));

	// Adjust dialog for special purpose uses.
	if (m_bSought || m_bProp)
	{
		Remove(IDC_BOX_LABEL);
		m_stcComp.ShowWindow(SW_HIDE);
		m_editName.ShowWindow(SW_HIDE);
		m_stcLet.ShowWindow(SW_HIDE);

		CString strDlgHdr;
		if (m_bSought)	(strDlgHdr = "Define Sought");
		else (strDlgHdr = "Select a force acting on the body");
		SetWindowText(strDlgHdr);

		UpdatePlanStrings(&m_cboBodyList);
	}

	// Set visibility of time choice list
	if (m_pDocument->m_strTimes.IsEmpty()) {
		// hide for static problems (no time list)
		m_cboTimeList.ShowWindow(SW_HIDE);
		m_stcTimeList.ShowWindow(SW_HIDE);
		//no direction with Props, soughts and variables
		//all derived from CVariable
		if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVariable)))
			Remove(IDC_BOX_TIMEDIR);
	}

	ASSERT(m_pTempObj);

	UpdateComponents();

	if (theApp.m_bTrainMode)
		theApp.SendTrainer(this, ID_TFORCE_DLG);
	else {
		SelectSingleChoices();
		m_cboBodyList.SetFocus();
		return FALSE;
	}

	SelectSingleChoices();
	return TRUE;  // return TRUE unless you set the focus to a control
}

void CVectorDlg::AdjustForceTypes()
{
	// Replace "Weight" with "Gravitational" on gravitation problems.
	if (m_pDocument->m_wConcept & ID_PROB_GRAVITATION) {
		int iWeight = m_cboForceType.FindStringExact(-1, "Weight");
		m_cboForceType.DeleteString(iWeight);
		m_cboForceType.AddString("Gravitational");
	}

	// Add Electric/Magnetic force types for E&M problems
	if (m_pDocument->m_wConcept & ID_PROB_EM) {
		m_cboForceType.AddString("Electric");
		m_cboForceType.AddString("Magnetic");
	}
}


void CVectorDlg::OnOK() 
{
	// Make sure all visible controls filled in	
	if (IsEmpty(&m_cboBodyList))
	{
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboAgentList))
	{
		theApp.DoWarningMessage("Please select the agent causing the force", this);
		return;
	}
	if (IsEmpty(&m_cboForceType))
	{
		theApp.DoWarningMessage("Please select a force type", this);
		return;
	}
	if (IsEmpty(&m_cboTimeList))
	{
		theApp.DoWarningMessage("Please select a time", this);
		return;
	}

	//Update data from controls into temp obj 
	m_editName.GetWindowText(m_pTempObj->m_strName);
	if (m_pObj->IsKindOf(RUNTIME_CLASS(CVector)))
		UpdateTempVector();
	else
		UpdateTempVariable();

	//Check fo uniqueness & validity
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

		if (! m_pTempObj->IsValid()) return;
	}

	if (!CheckDialog())	return;

	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();
}

int CVectorDlg::GetTrainerId(int ctrlId)
{
	if (ctrlId == IDC_CUSTOM_LABEL)
		ctrlId = IDC_VECTOR_NAME_TEXT;	// old label control id

	return ctrlId + 31890;
}


void CVectorDlg::InitObjectDlg()
{
	CVector* pVec = (CVector*)m_pTempObj;
	if (pVec->m_strForceType == "Net"){
		m_btnForce.SetCheck(0);
		m_btnNet.SetCheck(1);
		OnNetbtn();
	}
	else
		m_cboForceType.SelectStringExact(pVec->m_strForceType);
	m_cboAgentList.SelectStringExact(pVec->m_strAgent);
	m_cboBodyList.SelectStringExact(pVec->m_strBody);
	m_cboTimeList.SelectStringExact(pVec->m_strTime);

	m_pDlgValues->TransferValues(FALSE);

	// Initialize direction 
	if (pVec->IsZeroMag()) {
		// no direction for zero magnitude vectors
		m_editOrientation.EnableWindow(FALSE);
		m_spinDirection.EnableWindow(FALSE);
		m_editOrientation.SetWindowText(""); // NB: OK to transfer back below
	} else {
		int nDegrees = atoi(pVec->m_strOrientation);
	//	ASSERT((0 <= nDegrees) && (nDegrees < 360));
		m_spinDirection.SetRange(nDegrees - 20, nDegrees + 20);
		m_editOrientation.SetWindowText(pVec->m_strOrientation);
	}
}

void CVectorDlg::InitVariableDlg()
{
	SetWindowText("Variable definition");
	CVariable* pVar = (CVariable*)m_pTempObj;
	if (pVar->m_strForceType == "Net"){
		m_btnForce.SetCheck(0);
		m_btnNet.SetCheck(1);
		OnNetbtn();
	}
	else
		m_cboForceType.SelectStringExact(pVar->m_strForceType);
	m_cboAgentList.SelectStringExact(pVar->m_strAgent);
	m_cboBodyList.SelectStringExact(pVar->m_strObject);
	m_cboTimeList.SelectStringExact(pVar->m_strTime);
	m_spinDirection.ShowWindow(SW_HIDE);
	m_editOrientation.ShowWindow(SW_HIDE);
	m_stcVecAng.ShowWindow(SW_HIDE);
	m_stcVecAng1.ShowWindow(SW_HIDE);
	m_stcVecAng2.ShowWindow(SW_HIDE);
	if (!((CFBDDoc*)theApp.GetDocument())->m_strTimes.IsEmpty())
	{
		CRect rect1, rect2;
		m_editOrientation.GetWindowRect(rect1);
		ScreenToClient(&rect1);
		m_stcVecAng2.GetWindowRect(rect2);
		ScreenToClient(&rect2);
		m_cboTimeList.SetWindowPos(NULL, rect1.left, rect1.top, 0, 0,
			SWP_NOSIZE | SWP_NOZORDER );
		m_stcTimeList.SetWindowPos(NULL, rect2.left, rect2.top, 0, 0,
			SWP_NOSIZE | SWP_NOZORDER );
	}

}

CLabelRichEdit* CVectorDlg::GetLabelCtrl()
{
	return &m_editName;
}


void CVectorDlg::UpdateTempVector()
{
	if (m_btnNet.GetCheck())
		((CVector*)m_pTempObj)->m_strForceType = "Net";
	else
		((CVector*)m_pTempObj)->m_strForceType = GetCurString(&m_cboForceType);

	((CVector*)m_pTempObj)->m_strAgent = GetCurString(&m_cboAgentList);
	((CVector*)m_pTempObj)->m_strBody = GetCurString(&m_cboBodyList);
	((CVector*)m_pTempObj)->m_strTime = GetCurString(&m_cboTimeList);
	m_editOrientation.GetWindowText(((CVector*)m_pTempObj)->m_strOrientation);
	m_editName.GetRichEditText(m_pTempObj->m_strName);
	m_pDlgValues->TransferValues(/*bSaving=*/ TRUE);
}

void CVectorDlg::UpdateTempVariable()
{
	if (m_btnNet.GetCheck())
		((CVariable*)m_pTempObj)->m_strForceType = "Net";
	else
		((CVariable*)m_pTempObj)->m_strForceType = GetCurString(&m_cboForceType);
	
	((CVariable*)m_pTempObj)->m_strAgent = GetCurString(&m_cboAgentList);
	((CVariable*)m_pTempObj)->m_strObject = GetCurString(&m_cboBodyList);
	((CVariable*)m_pTempObj)->m_strTime = GetCurString(&m_cboTimeList);
	((CVariable*)m_pTempObj)->m_strQuantName = "Force";
	
	m_editName.GetRichEditText(m_pTempObj->m_strName);

	CString strTime = "";
	if (!((CVariable*)m_pTempObj)->m_strTime.IsEmpty())
		strTime = " at time " + ((CVariable*)m_pTempObj)->m_strTime;

	CString strAgent = "";
	if (!((CVariable*)m_pTempObj)->m_strAgent.IsEmpty())
		strAgent = " due to " + ((CVariable*)m_pTempObj)->m_strAgent;

	
	CString strProp;
	if (!m_bProp)
	{
		if (m_bMagnitude)
			strProp = "magnitude of the ";
		else
			strProp = "direction of the ";
	}

	((CVariable*)m_pTempObj)->m_strDef = strProp + ((CVariable*)m_pTempObj)->m_strForceType + " " +
			((CVariable*)m_pTempObj)->m_strQuantName + " on " + ((CVariable*)m_pTempObj)->m_strObject + 
					strTime + strAgent;


}

void CVectorDlg::UpdateComponents()
{
	if (m_bSought || m_bProp)
		return;

	CFBDDoc* pDoc = ((CFBDDoc*)theApp.GetDocument());
	CString strXComp;
	CString strYComp;
	if (pDoc->IsAxesDrawn()){
		strXComp.Format("X component = %s_x", m_pTempObj->m_strName );
		strYComp.Format("Y component = %s_y", m_pTempObj->m_strName);
	}
	else
		strXComp ="Draw the axes to define the X and Y components of this vector";

	CString strComp = strXComp;
	if(!strYComp.IsEmpty())
		strComp.Format("%s\n%s", strXComp, strYComp);

	m_stcComp.ShowWindow(SW_SHOWNA);
	m_stcComp.SetWindowText(strComp);

	// Also change the label on the orientation field to the theta or phi var name
	CString strAngVar = m_editOrientation.IsWindowEnabled() ?"$q" : "$j";
	strAngVar += m_pTempObj->m_strName + "=";

	// need to force redraw to invoke control's custom paint function to render 
	// Greek, not sure why SetWindowText isn't sufficient.
	// m_stcVecAng2.ShowWindow(SW_SHOWNA);
	m_stcVecAng2.SetRichText(strAngVar);
}

void CVectorDlg::OnChangeVectorNameText() 
{
	// TODO: If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDrawObjDlg::OnInitDialog()
	// function to send the EM_SETEVENTMASK message to the control
	// with the ENM_CHANGE flag ORed into the lParam mask.
	m_editName.GetRichEditText(m_pTempObj->m_strName);

	UpdateComponents();	

	if (m_pDlgValues) m_pDlgValues->OnUpdateName(m_pTempObj->m_strName);
}

void CVectorDlg::OnNetbtn() 
{

	EnableComboBox(&m_cboAgentList, FALSE);	
	EnableComboBox(&m_cboForceType, FALSE);
}

void CVectorDlg::OnForcebtn() 
{
	EnableComboBox(&m_cboAgentList, TRUE);	
	EnableComboBox(&m_cboForceType, TRUE);
}

// filter for backwards-compatible playback of old logs:
BOOL CVectorDlg::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{
	char* pszRest;
	
	// for force type combo: change old "Unspecified" to new "Applied"
	if (nEvent == EV_CBO_SEL && 
		GetCtlArg(pszArgs, pszRest) == &m_cboForceType ) 
	{
		CString strNewArgs(pszArgs);
		strNewArgs.Replace("Unspecified", "Applied");
		// let base class do work using possibly modified argument string
		return CDrawObjDlg::DispatchEvent(nEvent, strNewArgs);
	}
	
	// else just pass along to base class
	return CDrawObjDlg::DispatchEvent(nEvent, pszArgs);
}

