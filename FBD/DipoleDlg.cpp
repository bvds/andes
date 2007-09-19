// DipoleDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "fbddoc.h"
#include "fbdobj.h"
#include "DipoleDlg.h"
#include "VarView.h"  // for CVarView::HasFeature

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDipoleDlg dialog


CDipoleDlg::CDipoleDlg(CDrawObj*pObj, BOOL bMagnetic/*=FALSE*/, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CDipoleDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CDipoleDlg)
	//}}AFX_DATA_INIT
	m_bMagnetic = bMagnetic;
}

void CDipoleDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDipoleDlg)
	DDX_Control(pDX, IDC_STATIC_VALUE, m_stcType);
	DDX_Control(pDX, IDC_BODY, m_cboBody);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDC_TIME, m_cboTimeList);
	DDX_Control(pDX, IDC_L2ANGLE, m_stcVecAng2);
	DDX_Control(pDX, IDC_ORIENTATION_OLD, m_editOrientation);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDir);
	DDX_Control(pDX, IDC_LANGLE, m_stcVecAng1);
	DDX_Control(pDX, IDC_ANGLE, m_stcVecAng);
	DDX_Control(pDX, IDC_ZDIR, m_cboZDir);
	DDX_Control(pDX, IDC_TXT0, m_stcLet);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	//}}AFX_DATA_MAP

	// Point choices  named positions declared.
	DDX_FillList(pDX, IDC_BODY, &m_pDocument->m_strObjects);
	//DDX_AddCompoundBodies(pDX, IDC_BODY, &m_pDocument->m_objects);
	DDX_FillList(pDX, IDC_TIME, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME, &m_pDocument->m_Variables);

	// transfers slot values to/from controls
	//  on open, from tempobj to controls via InitDlg =>InitObjDlg/InitVarDlg
	//  [OnOK checks and transfers back to tempobj via UpdateTempVector]
	//  on saving, to real obj via UpdateObj => pObj->UpdateObj
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CDipoleDlg)
	"name", IDC_CUSTOM_LABEL,
	"body", IDC_BODY,
	"time", IDC_TIME,
	"agent", IDC_FORCEAGENT,
	"dir",	IDC_ORIENTATION_TEXT,
	"zdir",	IDC_ZDIR_VALUE,
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
	// to catch "standard" label on agent slot:
	"agent", IDC_FORCEAGENT,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CDipoleDlg)


BEGIN_MESSAGE_MAP(CDipoleDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CDipoleDlg)
		ON_CBN_SELCHANGE(IDC_ZDIR, OnSelchangeZdir)
	ON_EN_CHANGE(IDC_CUSTOM_LABEL, OnChangeVectorNameText)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDipoleDlg message handlers

BOOL CDipoleDlg::OnInitDialog()
{
	// LogEventf(EV_FIELD_DLG, "%s |%s|",m_pObj->m_strId, m_pObj->m_strName);
	
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

	// Base class inits lists via DDX. Calls InitDlg=>InitObjectDlg/InitVarDlg 
	// to transfer values from tempobj to controls.
	CDrawObjDlg::OnInitDialog();


	// Adjust label if this is for magnetic dipole, as opposed to default Electric
	if (m_bMagnetic)
		m_stcType.SetWindowText("Magnetic");
		           
	// Adjust visibility of controls here
	// Note need to support using dialog for non-drawn vector variable
	// Will happen when defining sought quantity to next-step-help.

	// Remove fields for special-purpose uses
	if (m_bSought)
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
		//m_cboZDir.ShowWindow(SW_SHOWNORMAL);
	}

	SelectSingleChoices();
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CDipoleDlg::InitObjectDlg()
{
	CVector* pVec = (CVector*) m_pTempObj;
	m_cboBody.SelectStringExact(pVec->m_strBody );
	m_cboTimeList.SelectStringExact(pVec->m_strTime) ;

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
			int nDegrees = atoi(pVec->m_strOrientation);
			// ASSERT((0 <= nDegrees) && (nDegrees < 360));
			m_spinDir.SetRange(nDegrees - 20, nDegrees + 20);
			m_editOrientation.SetWindowText(pVec->m_strOrientation);
		} 
		m_cboZDir.SetCurSel(pVec->m_nZDir);
		OnSelchangeZdir();
	}
}

void CDipoleDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;
	SetWindowText("Variable definition");

	m_cboBody.SelectStringExact(pVar->m_strObject);
	m_cboTimeList.SelectStringExact(pVar->m_strTime) ;
}

void CDipoleDlg::UpdateTempVector()
{
	 CVector* pTempVec = (CVector*) m_pTempObj;

	 pTempVec->m_strBody = GetCurString(&m_cboBody);
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

void CDipoleDlg::UpdateTempVariable()
{
	CVariable * pTempVar = (CVariable*) m_pTempObj;

	pTempVar->m_strObject = GetCurString(&m_cboBody);
	pTempVar->m_strTime = GetCurString(&m_cboTimeList);
	m_editName.GetRichEditText(pTempVar->m_strName);
	
	// for vars, also need to set variable quant type and definition strings
	pTempVar->m_strQuantName.Format("%s %s", (m_bMagnetic ? "Magnetic":"Electric"), "dipole moment");
	CString strTime;
	if (! pTempVar->m_strTime.IsEmpty())
		strTime = " at time " + pTempVar->m_strTime;
	
	pTempVar->m_strDef =  pTempVar->m_strQuantName
						  + " of " + pTempVar->m_strObject 
						  + strTime;
}

void CDipoleDlg::OnOK()
{
	// Make sure all visible controls filled in	
	if (IsEmpty(&m_cboBody)) {
		theApp.DoWarningMessage("Please select the object whose dipole moment is being defined", this);
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

void CDipoleDlg::OnSelchangeZdir() 
{
	int nZDir = m_cboZDir.GetCurSel();
	if (nZDir < 0 || nZDir > ZDIR_MAX) return;

	// enable degree edit accordingly
	if (nZDir == ZDIR_NONE) {
		//m_editOrientation.EnableWindow(TRUE);
	} else {
		m_editOrientation.EnableWindow(FALSE);
	}
}

void CDipoleDlg::OnChangeVectorNameText() 
{
	// TODO: If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDrawObjDlg::OnInitDialog()
	// function and call CRichEditCtrl().SetEventMask()
	// with the ENM_CHANGE flag ORed into the mask.
	
	m_editName.GetRichEditText(m_pTempObj->m_strName);
	if (m_pDlgValues) m_pDlgValues->OnUpdateName(m_pTempObj->m_strName);
}
