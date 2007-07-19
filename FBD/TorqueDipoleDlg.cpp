// TorqueDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "TorqueDipoleDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static const char szDipole[] = "Dipole";	// m_strForceType value for Dipole Torque

/////////////////////////////////////////////////////////////////////////////
// CTorqueDipoleDlg dialog


CTorqueDipoleDlg::CTorqueDipoleDlg(CDrawObj* pObj/*= NULL*/, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CTorqueDipoleDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CTorqueDipoleDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT

	// CDrawObj constructor makes tempobj clone of underlying object.
}


void CTorqueDipoleDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTorqueDipoleDlg)
	DDX_Control(pDX, IDC_LVECTOR_NAME_TEXT, m_stcLet);
	DDX_Control(pDX, IDC_LANGLE, m_stcVecAng1);
	DDX_Control(pDX, IDC_ANGLE, m_stcVecAng);
	DDX_Control(pDX, IDC_COMP, m_stcComp);
	DDX_Control(pDX, IDC_L2ANGLE, m_stcVecAng2);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDC_TIME_TEXT, m_cboTimeList);
	DDX_Control(pDX, IDC_ORIENTATION_OLD, m_editOrientation);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDir);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_ZDIR, m_cboZDir);
	DDX_Control(pDX, IDC_BODY_TEXT2, m_cboField);
	DDX_Control(pDX, IDC_BODY_TEXT, m_cboBody);
	//}}AFX_DATA_MAP
	//Fill dialog boxes with data from doc (string lists)
	DDX_FillList(pDX, IDC_BODY_TEXT, &m_pDocument->m_strObjects);
	DDX_AddCompoundBodies(pDX, IDC_BODY_TEXT, &m_pDocument->m_objects);
	// Second arg is list of student-named field vectors.
	DDX_AddFieldVectors(pDX, IDC_BODY_TEXT2, &m_pDocument->m_objects);
	DDX_FillList(pDX, IDC_TIME_TEXT, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME_TEXT, &m_pDocument->m_Variables);

	// Transfer props to/from underlying object:
	//   Inits dialog controls from object on open
	//   Transfers props to underlying object on close (m_bSaveAndValidate=TRUE).
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CTorqueDipoleDlg)
	"name",		IDC_CUSTOM_LABEL,
	"time",		IDC_TIME_TEXT,
	"body",		IDC_BODY_TEXT,		
	"field",	IDC_BODY_TEXT2,		
	"dir",		IDC_ORIENTATION_TEXT,
	"zdir",		IDC_ZDIR,
	// from value control
	"given-mag",IDC_MAG_VALUE,
	"given-xc", IDC_XC_VALUE,
	"given-yc", IDC_YC_VALUE,
	"given-zc", IDC_ZC_VALUE,
	"OK",		IDOK,
	"Cancel",	IDCANCEL,
END_CTL_TBL(CTorqueDipoleDlg)

BEGIN_MESSAGE_MAP(CTorqueDipoleDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CTorqueDipoleDlg)
	ON_CBN_SELCHANGE(IDC_ZDIR, OnSelchangeZdir)
	ON_EN_CHANGE(IDC_CUSTOM_LABEL, OnChangeVectorNameText)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTorqueDipoleDlg message handlers

BOOL CTorqueDipoleDlg::OnInitDialog() 
{
	//LogEventf(EV_TORQUE_DLG, "%s |%s|",m_pObj->m_strId, m_pObj->m_strName);

	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVector)) && ! m_bSought) {
			// create vector value sub-dialog and place it
			m_pDlgValues = new CValueDlg((CVector*)m_pTempObj, this);
			m_pDlgValues->Create(CValueDlg::IDD, this);
			CRect rcValues;
			GetDlgItem( IDC_STATIC_PLACEHOLDER)->GetWindowRect( &rcValues );
			ScreenToClient(rcValues);
			m_pDlgValues->SetWindowPos( NULL, rcValues.left + 7, rcValues.top + 7, 0, 0,
				SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE | SWP_SHOWWINDOW );

			Remove(IDC_BOX_TIMEDIR);
	} else {
		Remove(IDC_STATIC_PLACEHOLDER);
		m_pDlgValues = NULL;
	}
		
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
		// m_cboZDir.ShowWindow(SW_SHOWNORMAL);
	}

	// make sure we have some field choices
	if (m_cboField.GetCount() < 1) {
		theApp.DoWarningMessage("You must draw a field vector before the torque due to the field can be defined", this);
		// Don't know how to safely cancel the dialog at this point, so try to
		// queue up a cancel button click for ourselves.
		PostMessage(WM_COMMAND, MAKELONG(IDCANCEL, BN_CLICKED));
	}


	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// NB: Also need to support using for non-drawn torque variable
// Will happen when defining sought quantity. 

void CTorqueDipoleDlg::InitObjectDlg()	// Set Control values from temp object
{
	CVector* pVec = (CVector*)m_pTempObj;

	m_cboBody.SelectStringExact(pVec->m_strBody);
	m_cboField.SelectStringExact(pVec->m_strAgent);
	m_cboTimeList.SelectStringExact(pVec->m_strTime);
	m_pDlgValues->TransferValues(FALSE);

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

void CTorqueDipoleDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;
	SetWindowText("Variable definition");

	m_cboBody.SelectStringExact(pVar->m_strObject);
	m_cboField.SelectStringExact(pVar->m_strObject);
	m_cboTimeList.SelectStringExact(pVar->m_strTime);
}

void CTorqueDipoleDlg::UpdateTempVector() // Update Temp object from control values
{
	 CVector* pTempVec = (CVector*) m_pTempObj;
	 
	 pTempVec->m_strForceType = szDipole;
	 pTempVec->m_strBody = GetCurString(&m_cboBody);
	 pTempVec->m_strAgent= GetCurString(&m_cboField);	
	 pTempVec->m_strTime = GetCurString(&m_cboTimeList);
	
	 m_editOrientation.GetWindowText(pTempVec->m_strOrientation);
	 if (m_cboZDir.IsWindowEnabled()) {
		int nZDir = m_cboZDir.GetCurSel();
		if (nZDir >= 0 && nZDir <= ZDIR_MAX)
			pTempVec->m_nZDir = nZDir;
	 }
	 m_pDlgValues->TransferValues(/*bSaving=*/ TRUE);
	
	m_editName.GetRichEditText(pTempVec->m_strName);
}

void CTorqueDipoleDlg::UpdateTempVariable()
{
	CVariable * pTempVar = (CVariable*) m_pTempObj;

	pTempVar->m_strForceType = szDipole;
	pTempVar->m_strObject = GetCurString(&m_cboBody);
	pTempVar->m_strAgent = GetCurString(&m_cboField);
	pTempVar->m_strTime = GetCurString(&m_cboTimeList);
	m_editName.GetRichEditText(pTempVar->m_strName);
	
	// for vars, also need to set variable quant type and definition strings
	pTempVar->m_strQuantName = "torque";
	CString strTime;
	if (! pTempVar->m_strTime.IsEmpty())
		strTime = " at time " + pTempVar->m_strTime;

	pTempVar->m_strDef = "Torque on " + pTempVar->m_strObject +  
				         " due to " + pTempVar->m_strAgent + strTime;
}

void CTorqueDipoleDlg::OnOK()		// Check on Ok and update if correct.
{
	// Make sure all visible controls filled in	
	if (IsEmpty(&m_cboBody)) {
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboField)) {
		theApp.DoWarningMessage("Please select the field giving rise to the torque", this);
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

void CTorqueDipoleDlg::OnSelchangeZdir() 
{
	int nZDir = m_cboZDir.GetCurSel();
	if (nZDir < 0 || nZDir > ZDIR_MAX) return;

		// enable degree edit accordingly
	if (nZDir == ZDIR_NONE) {
		// m_editOrientation.EnableWindow(TRUE);
		// static icon showing ccw orientation
		// m_stcVecAng.ShowWindow(SW_SHOW);	
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


void CTorqueDipoleDlg::OnChangeVectorNameText() 
{
	// TODO: If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDrawObjDlg::OnInitDialog()
	// function to send the EM_SETEVENTMASK message to the control
	// with the ENM_CHANGE flag ORed into the lParam mask.
	m_editName.GetRichEditText(m_pTempObj->m_strName);
	
	UpdateComponents();	
	if (m_pDlgValues) m_pDlgValues->OnUpdateName(m_pTempObj->m_strName);
}

void CTorqueDipoleDlg::UpdateComponents()
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
	// m_stcVecAng2.ShowWindow(SW_SHOWNA);
	m_stcVecAng2.SetRichText(strAngVar);
}


