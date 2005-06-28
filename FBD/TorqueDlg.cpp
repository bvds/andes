// TorqueDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "TorqueDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static const char szNet[] = "Net";	// m_strForceType value for Net Torque

/////////////////////////////////////////////////////////////////////////////
// CTorqueDlg dialog


CTorqueDlg::CTorqueDlg(CDrawObj* pObj/*= NULL*/, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CTorqueDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CTorqueDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT

	// CDrawObj constructor makes tempobj clone of underlying object.
}


void CTorqueDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTorqueDlg)
	DDX_Control(pDX, IDC_LVECTOR_NAME_TEXT, m_stcLet);
	DDX_Control(pDX, IDC_LANGLE, m_stcVecAng1);
	DDX_Control(pDX, IDC_ANGLE, m_stcVecAng);
	DDX_Control(pDX, IDC_COMP, m_stcComp);
	DDX_Control(pDX, IDC_L2ANGLE, m_stcVecAng2);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDC_NETBTN, m_btnNet);
	DDX_Control(pDX, IDC_FORCEBTN, m_btnForce);
	DDX_Control(pDX, IDC_TIME_TEXT, m_cboTimeList);
	DDX_Control(pDX, IDC_ORIENTATION_TEXT, m_editOrientation);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDir);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_ZDIR, m_cboZDir);
	DDX_Control(pDX, IDC_BODY_TEXT2, m_cboForcePt);
	DDX_Control(pDX, IDC_BODY_TEXT, m_cboBody);
	DDX_Control(pDX, IDC_AGENT, m_cboAgent);
	//}}AFX_DATA_MAP
	//Fill dialog boxes with data from doc (string lists)
	DDX_FillList(pDX, IDC_BODY_TEXT, &m_pDocument->m_strObjects);
	DDX_AddCompoundBodies(pDX, IDC_BODY_TEXT, &m_pDocument->m_objects);
	DDX_FillList(pDX, IDC_BODY_TEXT2, &m_pDocument->m_strObjects);
	// Force pts in body2 don't need compound bodies
	DDX_FillList(pDX, IDC_AGENT, &m_pDocument->m_strObjects);
	DDX_FillList(pDX, IDC_TIME_TEXT, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME_TEXT, &m_pDocument->m_Variables);

	// Transfer props to/from underlying object:
	//   Inits dialog controls from object on open
	//   Transfers props to underlying object on close (m_bSaveAndValidate=TRUE).
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CTorqueDlg)
	"name",		IDC_CUSTOM_LABEL,
	"time",		IDC_TIME_TEXT,
	"body",		IDC_BODY_TEXT,		// for net torque
	"force-pt",	IDC_BODY_TEXT2,		// for individual torque !!! helpsys will send "body"
	"axis",		IDC_AGENT,
	"dir",		IDC_ORIENTATION_TEXT,
	"zdir",		IDC_ZDIR,
	"OK",		IDOK,
	"Cancel",	IDCANCEL,
END_CTL_TBL(CTorqueDlg)

BEGIN_MESSAGE_MAP(CTorqueDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CTorqueDlg)
	ON_CBN_SELCHANGE(IDC_ZDIR, OnSelchangeZdir)
	ON_BN_CLICKED(IDC_NETBTN, OnUpdateNet)
	ON_BN_CLICKED(IDC_FORCEBTN, OnUpdateNet)
	ON_EN_CHANGE(IDC_CUSTOM_LABEL, OnChangeVectorNameText)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTorqueDlg message handlers

BOOL CTorqueDlg::OnInitDialog() 
{
	//LogEventf(EV_TORQUE_DLG, "%s |%s|",m_pObj->m_strId, m_pObj->m_strName);
	
	// Base class inits lists via DDX. Calls InitDlg/InitObjectDlg to init values
	CDrawObjDlg::OnInitDialog();

	// Adjust visibility of controls here
	
	// Remove fields for special-purpose uses
	if (m_bSought || m_bProp)
	{
		Remove(IDC_BOX_LABEL);
		m_editName.ShowWindow(SW_HIDE);
		m_stcLet.ShowWindow(SW_HIDE);
		
		// take out component label
		m_stcComp.ShowWindow(SW_HIDE);

		if (m_bSought){
			SetWindowText("Define Sought");
		} else{
			SetWindowText("Define Property");
		}
		UpdatePlanStrings(&m_cboBody);
	}


	// Time choice: hide for static problems (no time list)
	// !!! never happens anymore. All problems have at least one default time point
	if (m_pDocument->m_strTimes.IsEmpty()) {
		m_cboTimeList.ShowWindow(SW_HIDE);
		m_stcTimeList.ShowWindow(SW_HIDE);
	}

	// direction choice: hide all for any CVariable derivative (incl soughts & props)
	// else enable ZDir choice if problem requires it
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVariable))) {
		// hide all direction controls
		m_spinDir.ShowWindow(SW_HIDE);
		m_editOrientation.ShowWindow(SW_HIDE);
		m_stcVecAng.ShowWindow(SW_HIDE);
		m_stcVecAng1.ShowWindow(SW_HIDE);
		m_stcVecAng2.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_TIMEDIR);	// take out row moving following ctrls up
	} else if (m_pDocument->UseZAxis()) {
		m_cboZDir.ShowWindow(SW_SHOWNORMAL);
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// !!! Also need to support using for non-drawn torque variable !!!
// Will happen when defining sought quantity. 

void CTorqueDlg::InitObjectDlg()	// Set Control values from temp object
{
	CVector* pVec = (CVector*)m_pTempObj;

	// Init Net vs. Individual button:
	BOOL bNet = ! pVec->m_strForceType.IsEmpty();
	m_btnNet.SetCheck(bNet);
	m_btnForce.SetCheck(! bNet);
	OnUpdateNet();

	if (bNet){// Net Torque
		// Body is body;
		m_cboBody.SelectStringExact(pVec->m_strBody);
	} else {	// individual torque
		// body is force point, 
		m_cboForcePt.SelectStringExact(pVec->m_strBody);
	}
	// agent is axis
	m_cboAgent.SelectStringExact(pVec->m_strAgent);
	m_cboTimeList.SelectStringExact(pVec->m_strTime);

	// initialize direction
	if (pVec->IsZeroMag() && !pVec->IsZAxisVector()) {
		// no direction for zero magnitude vectors
		m_editOrientation.EnableWindow(FALSE);
		m_spinDir.EnableWindow(FALSE);
		m_cboZDir.EnableWindow(FALSE);
		m_editOrientation.SetWindowText(""); // NB: OK to transfer back below
	} else {
		if (! pVec->IsZAxisVector()) {
			int nDegrees;
			if (pVec->GetOrientation(nDegrees)) { 
				// ASSERT((0 <= nDegrees) && (nDegrees < 360));
				m_spinDir.SetRange(nDegrees - 20, nDegrees + 20);
			}
			m_editOrientation.SetWindowText(pVec->m_strOrientation);
		} 
		m_cboZDir.SetCurSel(pVec->m_nZDir);
		OnSelchangeZdir();
	}
}

void CTorqueDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;
	SetWindowText("Variable definition");

	// Init Net vs. Individual button:
	BOOL bNet = ! pVar->m_strForceType.IsEmpty();
	m_btnNet.SetCheck(bNet);
	m_btnForce.SetCheck(! bNet);
	OnUpdateNet();

	if (bNet){// Net Torque
		// Body is body;
		m_cboBody.SelectStringExact(pVar->m_strObject);
	} else {	// individual torque
		// body is force point, 
		m_cboForcePt.SelectStringExact(pVar->m_strObject);
	}
	// agent is axis
	m_cboAgent.SelectStringExact(pVar->m_strAgent);
	m_cboTimeList.SelectStringExact(pVar->m_strTime);
}

void CTorqueDlg::UpdateTempVector() // Update Temp object from control values
{
	 CVector* pTempVec = (CVector*) m_pTempObj;

	 if (m_btnNet.GetCheck()) {
		 pTempVec->m_strBody = GetCurString(&m_cboBody);
		 pTempVec->m_strForceType = szNet;
	 } else {
		 pTempVec->m_strBody = GetCurString(&m_cboForcePt);
		 pTempVec->m_strForceType.Empty();
		 
	 }
	 pTempVec->m_strAgent = GetCurString(&m_cboAgent);
	 pTempVec->m_strTime = GetCurString(&m_cboTimeList);
	
	 m_editOrientation.GetWindowText(pTempVec->m_strOrientation);
	 if (m_cboZDir.IsWindowEnabled()) {
		int nZDir = m_cboZDir.GetCurSel();
		if (nZDir >= 0 && nZDir <= ZDIR_MAX)
			pTempVec->m_nZDir = nZDir;
	 }
	
	m_editName.GetRichEditText(pTempVec->m_strName);
}

void CTorqueDlg::UpdateTempVariable()
{
	CVariable * pTempVar = (CVariable*) m_pTempObj;

	if (m_btnNet.GetCheck()) {
		 pTempVar->m_strObject = GetCurString(&m_cboBody);
		 pTempVar->m_strForceType = szNet;
	} else {
		 pTempVar->m_strObject = GetCurString(&m_cboForcePt);
		 pTempVar->m_strForceType.Empty();
	}
	pTempVar->m_strAgent = GetCurString(&m_cboAgent);
	pTempVar->m_strTime = GetCurString(&m_cboTimeList);\
	m_editName.GetRichEditText(pTempVar->m_strName);
	
	// for vars, also need to set variable quant type and definition strings
	pTempVar->m_strQuantName = "torque";
	CString strTime;
	if (! pTempVar->m_strTime.IsEmpty())
		strTime = " at time " + pTempVar->m_strTime;
	if (pTempVar->m_strForceType.IsEmpty()) {
		pTempVar->m_strDef = "Torque about " + pTempVar->m_strAgent + 
				             " from force at " + pTempVar->m_strObject + strTime;
	} else {
		pTempVar->m_strDef = "Net Torque on " + pTempVar->m_strObject + strTime;
	}
}

void CTorqueDlg::OnOK()		// Check on Ok and update if correct.
{
	// Make sure all visible controls filled in	
	if (IsEmpty(&m_cboBody)) {
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboForcePt)) {
		theApp.DoWarningMessage("Please select the point of application of the force", this);
		return;
	}
	if (IsEmpty(&m_cboAgent)) {
		theApp.DoWarningMessage("Please select the rotation axis", this);
		return;
	}
	if (IsEmpty(&m_cboTimeList)) {
		theApp.DoWarningMessage("Please select a time", this);
		return;
	}

	// Update temp obj with values from controls
	m_editName.GetWindowText(m_pTempObj->m_strName); // ?? temporarily sets plain text
	if (m_pObj->IsKindOf(RUNTIME_CLASS(CVariable)))
		UpdateTempVariable();
	else
		UpdateTempVector();
	
	// Check def for uniqueness & validity
	if (!m_bSought)
	{
		CString str = m_pTempObj->m_strName;
		str.Remove('$');
		
		if (!IsValidLabel(str))	return;

		if (! m_pTempObj->IsValid()) return;
	}

	// Verify current def with help system.
	if (!CheckDialog())	return;

	// Get here => Finished OK: transfer new props into obj via
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();	
}

void CTorqueDlg::OnSelchangeZdir() 
{
	int nZDir = m_cboZDir.GetCurSel();
	if (nZDir < 0 || nZDir > ZDIR_MAX) return;

		// enable degree edit accordingly
	if (nZDir == ZDIR_NONE) {
		m_editOrientation.EnableWindow(TRUE);
		// static icon showing ccw orientation
		m_stcVecAng.ShowWindow(SW_SHOW);	
		// !!!if had been showing zdir direction, probably should resync displayed 
		// degree value with drawn x-y plane direction. For now, too bad if you change
	} else {
		// degree display now read only
		m_editOrientation.EnableWindow(FALSE);
		// hide ccw angle
		m_stcVecAng.ShowWindow(SW_HIDE);
		// display Z-Axis degrees in orientation field
		m_editOrientation.SetWindowText(nZDir == ZDIR_OUTOF ? "0" :
											(nZDir == ZDIR_INTO ? "180" : "?"));
	}

	// in case changed from z to non-z vector:
	UpdateComponents();
}

void CTorqueDlg::OnUpdateNet()
{
	BOOL bNet = m_btnNet.GetCheck();
	// hairy: we don't just enable relevant control, but also set child control id on 
	// active choice box to IDC_BODY_TEXT, using IDC_BODY_TEXT2 for the other one.
	// This is done so control table will map "body" slot name from helpsys on errors
	// to the right dialog control, specified child control ID.
	m_cboBody.EnableWindow(bNet);
	m_cboBody.SetDlgCtrlID( bNet ? IDC_BODY_TEXT : IDC_BODY_TEXT2);
	
	m_cboForcePt.EnableWindow(! bNet);
	m_cboForcePt.SetDlgCtrlID( ! bNet ? IDC_BODY_TEXT : IDC_BODY_TEXT2);
}

void CTorqueDlg::OnChangeVectorNameText() 
{
	// TODO: If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDrawObjDlg::OnInitDialog()
	// function to send the EM_SETEVENTMASK message to the control
	// with the ENM_CHANGE flag ORed into the lParam mask.
	m_editName.GetRichEditText(m_pTempObj->m_strName);
	
	UpdateComponents();	
}

void CTorqueDlg::UpdateComponents()
{
	// copied from general vector dialog, not all needed for torque
	if (m_bSought || m_bProp)
		return;

	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVariable)))
	{
		CVariable* pVar = (CVariable*)m_pTempObj;
		if ((pVar->m_nType == ID_VARIABLE_ADDSPEED) ||
			(pVar->m_nType == ID_VARIABLE_ADDANGVELOCITY) ||
			(pVar->m_nType == ID_VARIABLE_ADDANGACCELERATION) ||
			(pVar->m_nType == ID_VARIABLE_ADDANGDISPLACEMENT) ||
			(pVar->m_nType == ID_VARIABLE_ADDANGMOMENTUM) )
			return;
	}

	CFBDDoc* pDoc = m_pDocument;//((CFBDDoc*)theApp.GetDocument());
	CString strXComp, strYComp;
	CString strx, stry;
	// special case if currently defined as a Z-Axis vector
	if (!m_editOrientation.IsWindowEnabled()) {
		if (!pDoc->IsAxesDrawn())
			strXComp = "Draw axes to define the Z component of this vector";
		else
			strXComp.Format("Z component = %s_z", m_pTempObj->m_strName);
	}
	else if (pDoc->IsAxesDrawn()){
		strx.Format("%s_x", m_pTempObj->m_strName);
		stry.Format("%s_y", m_pTempObj->m_strName);
		//don't use temp, can't change complist from inside here
		//so didn't pass it to temp obj
		if (m_pObj->IsKindOf(RUNTIME_CLASS(CVector)) && 
			!((CVector*)m_pObj)->m_Comps.IsEmpty() )
		{
			POSITION pos = ((CVector*)m_pObj)->m_Comps.GetHeadPosition();
			while (pos != NULL)
			{
				CVector* pVec = (CVector*)((CVector*)m_pObj)->m_Comps.GetNext(pos);
				if (pVec->m_strCompDir.Find('X') != -1)
					strx = pVec->m_strName;
				else if (pVec->m_strCompDir.Find('Y') != -1)
					stry = pVec->m_strName;
			}

		}
		strXComp.Format("X component = %s", strx );
		strYComp.Format("Y component = %s", stry);
	}
	else{
		strXComp ="Draw the axes to define the X and Y components of this vector";
	}
	CString strComp = strXComp;
	if(!strYComp.IsEmpty())
		strComp.Format("%s\n%s", strXComp, strYComp);
	m_stcComp.ShowWindow(SW_SHOWNA);
	m_stcComp.SetRichText(strComp);

	// Also change the label on the orientation field to the theta or phi var name
	CString strAngVar = m_editOrientation.IsWindowEnabled() ?"$q" : "$j";
	strAngVar += m_pTempObj->m_strName + "=";

	// need to force redraw to invoke control's custom paint function to render 
	// Greek, not sure why SetWindowText isn't sufficient.
	m_stcVecAng2.ShowWindow(SW_SHOWNA);
	m_stcVecAng2.SetRichText(strAngVar);
}


