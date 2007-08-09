// UnitVectorDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "fbddoc.h"
#include "fbdobj.h"
#include "UnitVectorDlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

static const char szNormal[] = "Normal";	// m_strForceType value for Net T
/////////////////////////////////////////////////////////////////////////////
// CUnitVectorDlg dialog


CUnitVectorDlg::CUnitVectorDlg(CDrawObj*pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CUnitVectorDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CUnitVectorDlg)
	//}}AFX_DATA_INIT
}


void CUnitVectorDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CUnitVectorDlg)
	DDX_Control(pDX, IDC_NORMAL_BTN, m_btnNormal);
	DDX_Control(pDX, IDC_AT_BTN, m_btnAt);
	DDX_Control(pDX, IDC_TOWARDS_AWAY, m_cboTowardsAway);
	DDX_Control(pDX, IDC_L2ANGLE, m_stcVecAng2);
	DDX_Control(pDX, IDC_LANGLE, m_stcVecAng1);
	DDX_Control(pDX, IDC_ANGLE, m_stcVecAng);
	DDX_Control(pDX, IDC_LVECTOR_NAME_TEXT, m_stcLet);
	DDX_Control(pDX, IDC_BODY, m_cboBody);
	DDX_Control(pDX, IDC_AT_BODY, m_cboAtBody);
	DDX_Control(pDX, IDC_AGENT, m_cboAgent);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDC_TIME, m_cboTimeList);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDir);
	DDX_Control(pDX, IDC_ORIENTATION_TEXT, m_editOrientation);
	DDX_Control(pDX, IDC_ZDIR, m_cboZDir);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	//}}AFX_DATA_MAP

	// Choices are bodies plus compound bodies
	DDX_FillList(pDX, IDC_BODY, &m_pDocument->m_strObjects);
	DDX_AddCompoundBodies(pDX, IDC_BODY, &m_pDocument->m_objects);
	DDX_FillList(pDX, IDC_AT_BODY, &m_pDocument->m_strObjects);
	DDX_AddCompoundBodies(pDX, IDC_AT_BODY, &m_pDocument->m_objects);
	DDX_FillList(pDX, IDC_AGENT, &m_pDocument->m_strObjects);
	DDX_AddCompoundBodies(pDX, IDC_AGENT, &m_pDocument->m_objects);
	DDX_FillList(pDX, IDC_TIME, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME, &m_pDocument->m_Variables);

	// transfers slot values to/from controls
	//  on open, from tempobj to controls via InitDlg =>InitObjDlg/InitVarDlg
	//  [OnOK checks and transfers back to tempobj via UpdateTempVector]
	//  on saving, to real obj via UpdateObj => pObj->UpdateObj
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CUnitVectorDlg)
	"name", IDC_CUSTOM_LABEL,
	"body", IDC_BODY,
	"at-body", IDC_AT_BODY,
	"time", IDC_TIME,
	"agent", IDC_AGENT,
	"dir",	IDC_ORIENTATION_TEXT,
	"zdir",	IDC_ZDIR,
	// from value control
	"compo", IDC_COMPO_BTN,
	"magdir", IDC_MAGDIR_BTN,
	"given-mag",IDC_MAG_VALUE,
	"given-xc", IDC_XC_VALUE,
	"given-yc", IDC_YC_VALUE,
	"given-zc", IDC_ZC_VALUE,
	// alternate names:
	// alt name for dir field:
	"orientation",	IDC_ORIENTATION_TEXT,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CUnitVectorDlg)

BEGIN_MESSAGE_MAP(CUnitVectorDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CUnitVectorDlg)
	ON_CBN_SELCHANGE(IDC_ZDIR, OnSelchangeZdir)
	ON_BN_CLICKED(IDC_NORMAL_BTN, OnNormalBtn)
	ON_BN_CLICKED(IDC_AT_BTN, OnNormalBtn)
	ON_EN_CHANGE(IDC_CUSTOM_LABEL, OnChangeVectorNameText)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
	

/////////////////////////////////////////////////////////////////////////////
// CUnitVectorDlg message handlers
BOOL CUnitVectorDlg::OnInitDialog()
{
	// LogEventf(EV_IMPULSE_DLG, "%s |%s|",m_pObj->m_strId, m_pObj->m_strName);
	
	
/* No given values for unit vector
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVector)) && ! m_bSought) {
			// create vector value sub-dialog and place it
			m_pDlgValues = new CValueDlg((CVector*)m_pTempObj, this);
			m_pDlgValues->Create(CValueDlg::IDD, this);
			CRect rcValues;
			GetDlgItem( IDC_STATIC_PLACEHOLDER)->GetWindowRect( &rcValues );
			ScreenToClient(rcValues);
			m_pDlgValues->SetWindowPos( NULL, rcValues.left + 7, rcValues.top + 7, 0, 0,
				SWP_NOZORDER | SWP_NOSIZE | SWP_NOACTIVATE | SWP_SHOWWINDOW );
	} else {
		Remove(IDC_STATIC_PLACEHOLDER);
		m_pDlgValues = NULL;
	}
*/	
	// Base class inits lists via DDX. Calls InitDlg=>InitObjectDlg/InitVarDlg 
	// to transfer values from tempobj to controls.
	CDrawObjDlg::OnInitDialog();

	// Adjust visibility of controls here
	
	// Remove fields for special-purpose uses
	if (m_bSought || m_bProp)
	{
		Remove(IDC_BOX_LABEL);
		m_editName.ShowWindow(SW_HIDE);
		m_stcLet.ShowWindow(SW_HIDE);
		
		// take out component label
		/* m_stcComp.ShowWindow(SW_HIDE); // no compo label in this!! */
 
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

	// direction choice: hide for any CVariable derivative (handles soughts & props)
	// else include ZDir choice if problem requires it
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVariable))) {
		// hide all direction controls
		m_spinDir.ShowWindow(SW_HIDE);
		m_editOrientation.ShowWindow(SW_HIDE);
		m_stcVecAng.ShowWindow(SW_HIDE);
		m_stcVecAng1.ShowWindow(SW_HIDE);
		m_stcVecAng2.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_TIMEDIR);	// resize dlg to take out row, moving rest up
	} else if (m_pDocument->UseZAxis()) {
		m_cboZDir.ShowWindow(SW_SHOWNORMAL);
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


void CUnitVectorDlg::InitObjectDlg()
{
	CVector* pVec = (CVector*) m_pTempObj;

	// Init Normal vs. At button
	if (pVec->m_strForceType.IsEmpty())  // if type unset
		pVec->m_strForceType = szNormal; // default to Normal
	BOOL bNormal = pVec->m_strForceType == szNormal;
	m_btnNormal.SetCheck(bNormal);
	m_btnAt.SetCheck(! bNormal);
	OnNormalBtn(); // enable combo boxes appropriately

	if (bNormal)
		m_cboBody.SelectStringExact(pVec->m_strBody );
	else {
		m_cboAtBody.SelectStringExact(pVec->m_strBody );
		m_cboTowardsAway.SelectStringExact(pVec->m_strForceType);
		m_cboAgent.SelectStringExact(pVec->m_strAgent );
	}
	m_cboTimeList.SelectStringExact(pVec->m_strTime) ;
	//m_pDlgValues->TransferValues(FALSE);

	// initialize direction
	if (pVec->IsZeroMag() && !pVec->IsZAxisVector()) {
		// no direction for zero magnitude vectors
		m_editOrientation.EnableWindow(FALSE);
		m_spinDir.EnableWindow(FALSE);
		m_cboZDir.EnableWindow(FALSE);
		m_editOrientation.SetWindowText(""); // NB: OK to transfer back below
	} else {
		if (! pVec->IsZAxisVector()) {
			int nDegrees = atoi(pVec->m_strOrientation);
			// ASSERT((0 <= nDegrees) && (nDegrees < 360));
			m_spinDir.SetRange(nDegrees - 20, nDegrees + 20);
			m_editOrientation.SetWindowText(pVec->m_strOrientation);
		} 
		m_cboZDir.SetCurSel(pVec->m_nZDir);
		OnSelchangeZdir();
	}
}

// Note also need to support using for non-drawn vector variable
// Will happen when defining sought quantity. 
// (rare for unit vector, though might happen for direction

void CUnitVectorDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;
	SetWindowText("Variable definition");

	// Init Normal vs. At button
	BOOL bNormal = pVar->m_strForceType == "Normal";
	m_btnNormal.SetCheck(bNormal);
	m_btnAt.SetCheck(! bNormal);
	OnNormalBtn(); // enable combo boxes appropriately

	if (bNormal)
		m_cboBody.SelectStringExact(pVar->m_strObject);
	else {
		m_cboAtBody.SelectStringExact(pVar->m_strObject);
		m_cboTowardsAway.SelectStringExact(pVar->m_strForceType);
		m_cboAgent.SelectStringExact(pVar->m_strAgent);
	}
	m_cboTimeList.SelectStringExact(pVar->m_strTime) ;
}

void CUnitVectorDlg::UpdateTempVector()
{
	 CVector* pTempVec = (CVector*) m_pTempObj;

	 if (m_btnNormal.GetCheck()) {
		pTempVec->m_strForceType = szNormal;
		pTempVec->m_strBody = GetCurString(&m_cboBody);
		
	 } else {
		 pTempVec->m_strForceType = GetCurString(&m_cboTowardsAway);
		 pTempVec->m_strBody = GetCurString(&m_cboAtBody);
		 pTempVec->m_strAgent = GetCurString(&m_cboAgent);
	 } 
	 pTempVec->m_strTime = GetCurString(&m_cboTimeList);
	
	 m_editOrientation.GetWindowText(pTempVec->m_strOrientation);
	 if (m_cboZDir.IsWindowEnabled()) {
		int nZDir = m_cboZDir.GetCurSel();
		if (nZDir >= 0 && nZDir <= ZDIR_MAX)
			pTempVec->m_nZDir = nZDir;
	 }
	 //m_pDlgValues->TransferValues(/*bSaving=*/ TRUE);
	
	m_editName.GetRichEditText(pTempVec->m_strName);
}

void CUnitVectorDlg::UpdateTempVariable()
{
	CVariable * pTempVar = (CVariable*) m_pTempObj;


	if (m_btnNormal.GetCheck()) {
		pTempVar->m_strObject = GetCurString(&m_cboBody);
		pTempVar->m_strForceType = szNormal;
	} else {
		pTempVar->m_strObject = GetCurString(&m_cboAtBody);
		pTempVar->m_strForceType = GetCurString(&m_cboTowardsAway);
		pTempVar->m_strAgent = GetCurString(&m_cboAgent);
	}
	pTempVar->m_strTime = GetCurString(&m_cboTimeList);
	m_editName.GetRichEditText(pTempVar->m_strName);
	
	// for vars, also need to set variable quant type and definition strings
	pTempVar->m_strQuantName = "unit-vector";
	CString strTime;
	if (! pTempVar->m_strTime.IsEmpty())
		strTime = " at time " + pTempVar->m_strTime;
	
	if (m_btnNormal.GetCheck()) {
		pTempVar->m_strDef =  pTempVar->m_strQuantName
						  + " normal to " + pTempVar->m_strObject 
						  + strTime;
	} else {
		pTempVar->m_strDef =  pTempVar->m_strQuantName
						  + " at " + pTempVar->m_strObject 
						  + " " + pTempVar->m_strForceType  
						  + " " + pTempVar->m_strAgent 
						  + strTime;
	}
}

void CUnitVectorDlg::OnOK()
{
	// Make sure all visible controls filled in	
	if (IsEmpty(&m_cboBody)) {
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboAtBody)) {
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboTowardsAway)) {
		theApp.DoWarningMessage("Please select towards or away", this);
		return;
	}
	if (IsEmpty(&m_cboAgent)) {
		theApp.DoWarningMessage("Please select another body", this);
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

void CUnitVectorDlg::OnSelchangeZdir() 
{
	int nZDir = m_cboZDir.GetCurSel();
	if (nZDir < 0 || nZDir > ZDIR_MAX) return;

	// enable degree edit accordingly
	if (nZDir == ZDIR_NONE) {
		m_editOrientation.EnableWindow(TRUE);
	} else {
		m_editOrientation.EnableWindow(FALSE);
	}
}

void CUnitVectorDlg::OnNormalBtn() 
{
	BOOL bNormal = m_btnNormal.GetCheck();

	m_cboBody.EnableWindow(bNormal);
	m_cboAtBody.EnableWindow(! bNormal);
	m_cboAgent.EnableWindow(! bNormal);
	m_cboTowardsAway.EnableWindow(! bNormal);
}


void CUnitVectorDlg::OnChangeVectorNameText() 
{
	// TODO: If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDrawObjDlg::OnInitDialog()
	// function and call CRichEditCtrl().SetEventMask()
	// with the ENM_CHANGE flag ORed into the mask.
	
	// TODO: Add your control notification handler code here
	m_editName.GetRichEditText(m_pTempObj->m_strName);
}
