// VecAVDlg.cpp : implementation file
//
//$Id: VecAVDlg.cpp,v 1.4 2007/06/22 01:10:40 anders Exp $

#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "Motion.h"		// for vectors in motion dialogs
#include "VecAVDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVectorMoveDlg dialog


CVectorMoveDlg::CVectorMoveDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CVectorMoveDlg::IDD, pObj, pParent)
{
	//temporary object created & initialized in base class
	//{{AFX_DATA_INIT(CVectorMoveDlg)
	m_bAddGrav = FALSE;
	//}}AFX_DATA_INIT
}

// macro to test if being used for motion diagram vector (special case)
inline BOOL CVectorMoveDlg::IsMDVector()
	{ return m_pObj->IsKindOf(RUNTIME_CLASS(CMDVector)); }

void CVectorMoveDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVectorMoveDlg)
	DDX_Control(pDX, IDC_STATIC_EQUALS, m_stcEquals);
	DDX_Control(pDX, IDC_GIVEN_BOX, m_stcGiven);
	DDX_Control(pDX, IDC_STATIC_OR, m_stcOr);
	DDX_Control(pDX, IDC_CHECK_UNKNOWN, m_btnUnknown);
	DDX_Control(pDX, IDC_GIVEN_VALUE, m_editValue);
	DDX_Control(pDX, IDC_ZDIR, m_cboZDir);
	DDX_Control(pDX, IDC_ANGULAR, m_cboAngular);
	DDX_Control(pDX, IDC_LABEL_AVBODY, m_stcBody);
	DDX_Control(pDX, IDC_STC_TYPE, m_stcType);
	DDX_Control(pDX, IDC_MOVEMENT_TYPE, m_cboMvmntType);
	DDX_Control(pDX, IDC_LVECTOR_NAME_TEXT, m_stcLet);
	DDX_Control(pDX, IDC_COMP, m_stcComp);
	DDX_Control(pDX, IDC_ANGLE, m_stcVecAng);
	DDX_Control(pDX, IDC_LANGLE, m_stcVecAng1);
	DDX_Control(pDX, IDC_L2ANGLE, m_stcVecAng2);
	DDX_Control(pDX, IDC_TIME_TEXT, m_cboTimeList);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_STATIC_TEXT, m_txtDescription);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDirection);
	DDX_Control(pDX, IDC_ORIENTATION_TEXT, m_editOrientation);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDC_AVBODY_TEXT, m_cboBodyList);
	//}}AFX_DATA_MAP
	if (! IsMDVector()) {
		DDX_FillList(pDX, IDC_TIME_TEXT, &m_pDocument->m_strTimes);
		DDX_FillList(pDX, IDC_AVBODY_TEXT, &m_pDocument->m_strObjects);
		DDX_AddCompoundBodies(pDX, IDC_AVBODY_TEXT, &m_pDocument->m_objects);
		DDX_AddUserTimes(pDX, IDC_TIME_TEXT, &m_pDocument->m_Variables);
	}

	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CVectorMoveDlg)
	"name", IDC_CUSTOM_LABEL,
	"time",	IDC_TIME_TEXT,
	"body",	IDC_AVBODY_TEXT,
	"dir",	IDC_ORIENTATION_TEXT,
	"type",	IDC_MOVEMENT_TYPE,
	"ang",	IDC_ANGULAR,
	"zdir",	IDC_ZDIR,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
	// to read old long names for backwards compatibility:
	"vector-name", IDC_CUSTOM_LABEL,
	"vector-time",	IDC_TIME_TEXT,
	"vector-body",	IDC_AVBODY_TEXT,
	"orientation",	IDC_ORIENTATION_TEXT,
	"vector-type",	IDC_MOVEMENT_TYPE,
	// to replay var def ctl events from old logs which used generic variable dialog: 
	// also map relevant ctl names from generic var dcl dialog to our ctls
//	"variable-name", IDC_VECTOR_NAME_TEXT,
//	"variable-time", IDC_TIME_TEXT,
//	"variable-object", IDC_AVBODY_TEXT,
	/* "variable-agent", IDC_AGENT, */
END_CTL_TBL(CVectorMoveDlg)

IMPLEMENT_DYNAMIC(CVectorMoveDlg, CDrawObjDlg)

BEGIN_MESSAGE_MAP(CVectorMoveDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CVectorMoveDlg)
	ON_EN_CHANGE(IDC_CUSTOM_LABEL, OnChangeVectorNameText)
	ON_CBN_SELCHANGE(IDC_MOVEMENT_TYPE, OnSelchangeMovementType)
	ON_CBN_SELCHANGE(IDC_AVBODY_TEXT, OnSelchangeBody)
	ON_CBN_SELCHANGE(IDC_ANGULAR, OnSelchangeAngular)
	ON_CBN_SELCHANGE(IDC_ZDIR, OnSelchangeZdir)
	ON_BN_CLICKED(IDC_CHECK_UNKNOWN, OnCheckUnknown)
	ON_EN_CHANGE(IDC_GIVEN_VALUE, OnChangeGivenValue)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVectorMoveDlg message handlers

BOOL CVectorMoveDlg::OnInitDialog() 
{
	LogEventf(EV_VECTOR_DLG, "%s |%s|",m_pObj->m_strId, OBJ_NAME(m_pObj));

	// create the value sub-dialog early so it exists before property
	// transfers in the base class
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
	}
	
	// Base class will load mapped data member values 
	// into controls via DDX and bind control member vars.
	// Will also call Init[Object|Var]Dlg to initialize all droplist control values
	CDrawObjDlg::OnInitDialog();

	// Set control visibility from document & object state
	if (m_pDocument == NULL) return TRUE;	// shouldn't happen!
	ASSERT(m_pTempObj);
	ASSERT(!m_strDescription.IsEmpty());

	// Remove fields for special-purpose uses
	if (m_bSought || m_bProp)
	{
		Remove(IDC_BOX_LABEL);
		m_editName.ShowWindow(SW_HIDE);
		m_txtDescription.SetWindowText(m_strDescription);
		// take out component label
		m_stcComp.ShowWindow(SW_HIDE);

		if (m_bSought){
			//m_stcLet.SetWindowText("");
			m_stcLet.ShowWindow(SW_HIDE);
			SetWindowText("Define Sought");
		}
		else{
			SetWindowText("Define Property");
			m_stcLet.ShowWindow(SW_HIDE);
			if (m_bAddGrav){
				int nIndex = m_cboMvmntType.AddString("Gravitational");
				m_cboMvmntType.SetItemData(nIndex, ID_GRAVACCEL);
			}
		}

		UpdatePlanStrings(&m_cboBodyList);
	}

	// Angular/linear choice: show in rotational problems
	BOOL bShowAngular = FALSE;
	if (m_pDocument->m_wConcept & ID_PROB_ROTKINEMATICS) {
		m_cboAngular.ShowWindow(SW_SHOWNORMAL);
		bShowAngular = TRUE;
	}
	
	// Time choice: hide for static problems (no time list)
	if (m_pDocument->m_strTimes.IsEmpty()) {
		m_cboTimeList.ShowWindow(SW_HIDE);
		m_stcTimeList.ShowWindow(SW_HIDE);
	}
	
	//"instantaneous or average" choice: hide for displacement and momentum
	if (m_strDescription.GetAt(0) == 'D' || m_strDescription.GetAt(0) == 'M'){
		m_stcType.ShowWindow(SW_HIDE);
		m_cboMvmntType.ShowWindow(SW_HIDE);

		// if no time choices then can get rid of whole row with type + time
		if (m_pDocument->m_strTimes.IsEmpty())
			Remove(IDC_BOX_TYPE);
		else {
			// must move time box over where type box used to be. assumes same size
			CRect rcType;
			m_stcType.GetWindowRect(rcType); ScreenToClient(rcType);
			m_stcTimeList.MoveWindow(rcType);
			m_cboMvmntType.GetWindowRect(rcType); ScreenToClient(rcType);
			m_cboTimeList.MoveWindow(rcType);
		}
	}

	// direction choice: hide for any CVariable derivative (incl soughts & props)
	// else include ZDir choice if problem requires it
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVariable))) {
		Remove(IDC_BOX_TIMEDIR);	// remove whole row moving rest up
	} else if (m_pDocument->UseZAxis()) {
		// m_cboZDir.ShowWindow(SW_SHOWNORMAL);
	}

	// Given value box: enable for any variable if not being used to define sought
	// Really for speed variable only.
	if (!m_bSought) 
	{
		if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVariable))) {
			m_stcEquals.ShowWindow(SW_SHOW);
			m_stcGiven.ShowWindow(SW_SHOW);
			m_stcOr.ShowWindow(SW_SHOW);
			m_btnUnknown.ShowWindow(SW_SHOW);
			m_editValue.ShowWindow(SW_SHOW);
		} 
	}

	// Move top-line static and combo box controls so that they line up nicely without
	// all that empty space between controls.  Will make more elegant later
	CClientDC dc(NULL);
	CSize size = dc.GetTextExtent(m_strDescription);
	CRect pos, posBdyStc, posBdyCbo;
	
	m_txtDescription.GetWindowRect(&pos); ScreenToClient(pos);
	int xStart = pos.left;		// top, right start of the description label
	int yStart = pos.top;
	if (bShowAngular) {
		CRect posAng;
		m_cboAngular.GetWindowRect(&posAng); ScreenToClient(&posAng);
		xStart = posAng.right + 5;
	}
	m_txtDescription.SetWindowPos(0, xStart, yStart, size.cx, pos.Height(), SWP_NOZORDER);
	
	m_txtDescription.GetWindowRect(&pos); ScreenToClient(&pos);
	m_stcBody.GetWindowRect(&posBdyStc); ScreenToClient(&posBdyStc);
	m_stcBody.SetWindowPos(0, pos.right+5, posBdyStc.top, 0, 0, SWP_NOSIZE|SWP_NOZORDER);
	
	m_stcBody.GetWindowRect(&posBdyStc); ScreenToClient(&posBdyStc);
	m_cboBodyList.GetWindowRect(&posBdyCbo); ScreenToClient(&posBdyCbo);
	m_cboBodyList.SetWindowPos(0, posBdyStc.right+5, posBdyCbo.top, 0, 0, SWP_NOSIZE|SWP_NOZORDER);
	////////////////////////////////////////////////////////////////////

	m_cboMvmntType.SetItemData(0, ID_AVERAGE);
	m_cboMvmntType.SetItemData(1, ID_INSTANTANEOUS);
	
	// For motion diagram vectors: populate body list with time point choices
	// !!! should update this to use time field, but would have to update md code as well.
	if (IsMDVector())	//  contained in 1 or 2D motion diagram
	{
		CMDVector* pMDVec = (CMDVector*) m_pObj;
		
		// If body is set coming in, then shouldn't change it, so make it only choice
		if (! pMDVec->m_strBody.IsEmpty()) {
			m_cboBodyList.AddString(pMDVec->m_strBody);
			// select current string
			m_cboBodyList.SelectStringExact(pMDVec->m_strBody);
			m_cboBodyList.EnableWindow(FALSE); // show can't be changed
		} else {
			// fill body (system) field  with possible time point choices
			// Must get from appropriate 1D container
			CMotionDiagram* pDiagram = pMDVec->m_pDiagram;	// source diagram for fill		
			if (pMDVec->m_p2DDiagram)						// adjust in case its in 2D diagram.
				pDiagram = pMDVec->m_p2DDiagram->m_pmdX;	// take X componenent diagram.
		
			if (pDiagram) {
				pDiagram->BodiesToCbo(m_cboBodyList, pMDVec->m_nVectorType);
			}
			// default to "Whole Period" if unset
			m_cboBodyList.SelectStringExact(CMotionDiagram::szWholePeriod);
		}

		// make type match body choice
		UpdateMDVecType(pMDVec->m_strBody);
		
		// Change "body" field label label (could update with type, but don't currently)
		m_stcBody.SetWindowText("At time:"); 

		// All other vector controls but body and orientation are "read-only" dependents
		// m_stcType.EnableWindow(FALSE);  // gray label to show it's read only 
		m_cboMvmntType.EnableWindow(FALSE);
		m_cboAngular.EnableWindow(FALSE);
		m_editName.EnableWindow(FALSE);
		m_editName.SetBkColor(RGB(192, 192, 192));
		
		// m_stcLet.EnableWindow(FALSE);	// grey label to show it's read only 
		
		// take out component label
		m_stcComp.ShowWindow(SW_HIDE);
	}
	
	
	UpdateComponents();

	if (theApp.m_bTrainMode){
		DWORD dwId;
		ASSERT(!m_strDescription.IsEmpty());
		if (m_strDescription.GetAt(0) == 'V')
			dwId = ID_TVELOCITY_DLG;
		else if (m_strDescription.GetAt(0) == 'A')
			dwId = ID_TACCELERATION_DLG;
		else
			dwId = ID_TDISPLACEMENT_DLG;
		theApp.SendTrainer(this, dwId);
	}
	else {
		m_cboBodyList.SetFocus();
		return FALSE;
	}

	return TRUE;  // return TRUE unless you set the focus to a control
}
	

void CVectorMoveDlg::OnOK() 
{
	//Make sure all of dialog completed	
	if (IsEmpty(&m_cboBodyList))
	{
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboTimeList))
	{
		theApp.DoWarningMessage("Please select a time", this);
		return;
	}
	if (IsEmpty(&m_cboMvmntType))
	{
		CString str;
		str.Format("Please select whether %s is instantaneous or average", m_strDescription);
		theApp.DoWarningMessage(str, this);
		return;
	}
	if (IsEmpty(&m_cboAngular))
	{
		CString str;
		str.Format("Please select whether %s is linear or angular", m_strDescription);
		theApp.DoWarningMessage(str, this);
		return;
	}
	if (!TimeAgrees())
		return;

	//Update info from controls into temp object
	m_editName.GetWindowText(m_pTempObj->m_strName); // ?? gets plain text ?
	if (m_pObj->IsKindOf(RUNTIME_CLASS(CVector)))
		UpdateTempVector();							 // updates name w/tagged text
	else
		UpdateTempVariable();
	
	// Check for uniqueness and validity
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

		// Allow anything if vec is in motion diagram. (MDs only in qual
		// problems, which don't use variable defs).
		if (m_pObj && !IsMDVector())
		{
			if (! m_pTempObj->IsValid()) return;
		}
	}

	// update possible training card on OK dialog event.
	if (IsMDVector())
		theApp.SendTrainer(this, ID_ENDMOTIONDGRM);
	else
		theApp.SendTrainer(this, 0, HELP_QUIT);

	// For MD vectors don't check inside dlg, just go through to update obj
	if (! IsMDVector() ) {
		if (!CheckDialog())
			return;
	}

	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();
}

BOOL CVectorMoveDlg::TimeAgrees()
{
	if (m_pDocument->m_strTimes.IsEmpty())
		return TRUE;
	// not visible for displacement
	if (!m_cboMvmntType.IsWindowVisible())
		return TRUE;

	CString strTime = GetCurString(&m_cboTimeList);
	CString strType = GetCurString(&m_cboMvmntType);

	int nSel = m_cboTimeList.GetCurSel();
	int pData = m_cboTimeList.GetItemData(nSel);

	CString str;
	if ( ( (strTime.Find(" to ")>0) ||(pData == ID_USER_DEFINED) )
		&& (strType[0] == 'i' ))
	{
		str.Format("Instantaneous %s occurs at a point in time, not during a time interval", m_strDescription);
		theApp.DoWarningMessage(str, this);
		return FALSE;
	}
	else if (  !( (strTime.Find(" to ")>0)||(pData == ID_USER_DEFINED) )
		&& (strType[0] == 'a' )  ) 
	{
		str.Format("Average %s occurs during a time interval, not at a point in time", m_strDescription);
		theApp.DoWarningMessage(str, this);
		return FALSE;
	}
	return TRUE;

}

int CVectorMoveDlg::GetTrainerId(int ctrlId)
{
	if (ctrlId == IDC_CUSTOM_LABEL)
		ctrlId = IDC_VECTOR_NAME_TEXT; // old label control id

	int tcardId;	
	if (m_strDescription.GetAt(0) == 'A')
		tcardId = ctrlId + 31910;
	else if (m_strDescription.GetAt(0) == 'V')//type = velocity
		tcardId = ctrlId + 31900;
	else // displacement
		tcardId = ctrlId + 32022;
	
	//  "body" needs different tcard when dialog used for md accelerations. 
	if (IsMDVector() && tcardId == ID_TACCELERATION_BODY)
		tcardId = ID_TMOTACC_TIME ;
	
	return tcardId;

}

static const char szAngular[] = "Angular";
static const char szLinear[] = "Linear";

void CVectorMoveDlg::InitObjectDlg()
{
	CVector* pVec = (CVector*)m_pTempObj;

	// Log type of vector (old)
	LogEventf(EV_DLG_VECTOR_TYPE, "%d", pVec->m_nVectorType );

	// Set initial values of choices from object
	m_cboMvmntType.SelectStringExact(pVec->m_strForceType);
	m_cboBodyList.SelectStringExact(pVec->m_strBody);
	m_cboTimeList.SelectStringExact(pVec->m_strTime);
	m_cboAngular.SelectStringExact(pVec->m_bAngular ? szAngular : szLinear);

	m_pDlgValues->TransferValues(FALSE);

	// initialize direction
	if (pVec->IsZeroMag() && !pVec->IsZAxisVector()) {
		// no direction for zero magnitude vectors
		m_editOrientation.EnableWindow(FALSE);
		m_spinDirection.EnableWindow(FALSE);
		m_cboZDir.EnableWindow(FALSE);
		m_editOrientation.SetWindowText(""); // NB: OK to transfer back below
	} else {
		if (! pVec->IsZAxisVector()) {
			int nDegrees;
			if (pVec->GetOrientation(nDegrees)) { 
				// ASSERT((0 <= nDegrees) && (nDegrees < 360));
				m_spinDirection.SetRange(nDegrees - 20, nDegrees + 20);
			}
			m_editOrientation.SetWindowText(pVec->m_strOrientation);
		} 
		m_cboZDir.SetCurSel(pVec->m_nZDir);
		OnSelchangeZdir();
	}

	// get major type name by removing possible angular prefix in type name
	pVec->GetTypeName(m_strDescription);
	if (_strnicmp(m_strDescription, "ang-", 4) == 0)
		m_strDescription = m_strDescription.Mid(4);
	m_txtDescription.SetWindowText(m_strDescription);
}

void CVectorMoveDlg::InitVariableDlg()
{
	SetWindowText("Variable definition");
	CVariable* pVar = (CVariable*)m_pTempObj;

	m_cboMvmntType.SelectStringExact(pVar->m_strForceType);
	m_cboBodyList.SelectStringExact(pVar->m_strObject);
	m_cboTimeList.SelectStringExact(pVar->m_strTime);
	m_cboAngular.SelectStringExact(pVar->IsAngularVector() ? szAngular : szLinear);
	m_txtDescription.SetWindowText(m_strDescription);//string set coming in

	// Transfer given value/unknown bit from controls to variable
	m_editValue.SetWindowText(((CVariable*)m_pTempObj)->m_strValue);
	// sync unknown check box with value
	OnChangeGivenValue();

	// Don't show orientation angle controls/statics
	m_spinDirection.ShowWindow(SW_HIDE);
	m_editOrientation.ShowWindow(SW_HIDE);
	m_stcVecAng.ShowWindow(SW_HIDE);
	m_stcVecAng1.ShowWindow(SW_HIDE);
	m_stcVecAng2.ShowWindow(SW_HIDE);
#if 0
	if (!m_pDocument->m_strTimes.IsEmpty())
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
#endif
}

CLabelRichEdit* CVectorMoveDlg::GetLabelCtrl()
{
	return &m_editName;
}

// transfer value of angular flag into object
// slight difference between calls for vectors and variables
void CVectorMoveDlg::UpdateAngularFlag()
{
	CString strAngular = GetCurString(&m_cboAngular); // empty if unused
	if (strAngular.IsEmpty()) return;
	
	// else have a value
	BOOL bAngular = (strAngular == szAngular);
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVector))) {
		((CVector*)m_pTempObj)->m_bAngular = bAngular;
	} else {
		ASSERT_KINDOF(CVariable, m_pTempObj);
		((CVariable*)m_pTempObj)->SetAngular(bAngular);
	}
}

void CVectorMoveDlg::UpdateTempVector()
{
	 CVector* pTempVec = (CVector*) m_pTempObj;

	 pTempVec->m_strBody = GetCurString(&m_cboBodyList);
	 pTempVec->m_strTime = GetCurString(&m_cboTimeList);
	 pTempVec->m_strForceType = GetCurString(&m_cboMvmntType);
	 m_editOrientation.GetWindowText(pTempVec->m_strOrientation);
	 if (m_cboZDir.IsWindowEnabled()) {
		int nZDir = m_cboZDir.GetCurSel();
		if (nZDir >= 0 && nZDir <= ZDIR_MAX)
			pTempVec->m_nZDir = nZDir;
	 }
	 m_pDlgValues->TransferValues(/*bSaving=*/ TRUE);
	 // For md vectors, must sync drawing direction with told exact direction
	 // here, *before checking*, since MDVector checking normally uses drawn direction.
	 // (for mdvec's, don't normally use dialog to specify an exact direction.)
	 // Could do this for all vectors, but don't want to mess with other code - AW.
	 int nDeg;
	 if (IsMDVector() && sscanf(pTempVec->m_strOrientation, "%d", &nDeg) == 1) {
		m_pTempObj->SetDirection(nDeg);
	 }
	 
	UpdateAngularFlag();

	m_editName.GetRichEditText(pTempVec->m_strName);
}

void CVectorMoveDlg::UpdateTempVariable()
{
	CVariable* pTempVar = (CVariable*) m_pTempObj;
	pTempVar->m_strObject = GetCurString(&m_cboBodyList);
	pTempVar->m_strTime = GetCurString(&m_cboTimeList);
	pTempVar->m_strForceType = GetCurString(&m_cboMvmntType);
	m_editName.GetRichEditText(pTempVar->m_strName);
	// need to account for angular flag when setting type name ("strValue");
	UpdateAngularFlag();
	pTempVar->m_strQuantName = pTempVar->IsAngularVector() ? "Angular " + m_strDescription
										         : m_strDescription;
	CString strValue;
	m_editValue.GetWindowText(strValue);
	((CVariable*)m_pTempObj)->m_strValue= strValue;
	
						
	CString strTime = "";
	if (!pTempVar->m_strTime.IsEmpty())
	{
		CString str;
		m_stcTimeList.GetWindowText(str);
		if (str.Find('&')!=-1)
			strTime = " at time " + pTempVar->m_strTime;
		else
			strTime = " during " + pTempVar->m_strTime;
	}

	CString strProp;
	if ( (pTempVar->m_nType != ID_VARIABLE_ADDSPEED) && !m_bProp)
	{
		strProp = m_bMagnitude? "magnitude of the " :"direction of the ";
	}

	CString strOptModifier;
	if (!pTempVar->m_strForceType.IsEmpty())
		strOptModifier = pTempVar->m_strForceType + " ";
	pTempVar->m_strDef = strProp + strOptModifier + pTempVar->m_strQuantName 
					     + " of " +  pTempVar->m_strObject + strTime ;
}

void CVectorMoveDlg::UpdateComponents()
{
	if (m_bSought || m_bProp || IsMDVector())
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
	//m_stcVecAng2.ShowWindow(SW_SHOWNA);
	m_stcVecAng2.SetRichText(strAngVar);
}

void CVectorMoveDlg::OnChangeVectorNameText() 
{
	// TODO: If this is a RICHEDIT control, the control will not
	// send this notification unless you override the CDrawObjDlg::OnInitDialog()
	// function to send the EM_SETEVENTMASK message to the control
	// with the ENM_CHANGE flag ORed into the lParam mask.
	m_editName.GetRichEditText(m_pTempObj->m_strName);
	
	UpdateComponents();	
	m_pDlgValues->OnUpdateName(m_pTempObj->m_strName);
}

void CVectorMoveDlg::OnSelchangeMovementType() 
{
	if (m_pDocument->m_strTimes.IsEmpty())
		return;
//	if (((CFBDDoc*)theApp.GetDocument())->m_strTimes.IsEmpty())
//		return;

	int nSel = m_cboMvmntType.GetCurSel();
	if (m_cboMvmntType.GetItemData(nSel) == ID_GRAVACCEL)
	{
		m_stcTimeList.ShowWindow(SW_HIDE);
		m_cboTimeList.ShowWindow(SW_HIDE);

	}
	else{
		if (m_cboMvmntType.GetItemData(nSel) == ID_AVERAGE)
			m_stcTimeList.SetWindowText("during");
		else if (m_cboMvmntType.GetItemData(nSel) == ID_INSTANTANEOUS)
			m_stcTimeList.SetWindowText("&At time");
		m_stcTimeList.ShowWindow(SW_SHOW);
		m_cboTimeList.ShowWindow(SW_SHOW);
	}
}

void CVectorMoveDlg::OnSelchangeBody() 
{
	// this is for MD vectors only, in which "body" is really time point
	if (! IsMDVector()) return;

	// must generate vector name automatically from time point choice.
	// ! should be method in motion diagram, for now do it here.
	CString strTimePt;
	CString strName = m_strDescription[0]; // 'V' or 'A'
	m_cboBodyList.GetWindowText(strTimePt);
	if (strTimePt != CMotionDiagram::szWholePeriod) {
		// Time pt is of form: T0, T1, .. or N units, 2N units, 3N units, ..
		CString strTimeSuffix;
		if (strTimePt[0] == 'T') // default intervals
			strTimeSuffix = strTimePt.Mid(1);
		else {	// measured intervals
			strTimeSuffix = strTimePt.Left(strTimePt.Find(' '));
		}
		strName += strTimeSuffix;
	}
	m_editName.SetRichEditText(strName);

	UpdateMDVecType(strTimePt);
}


void CVectorMoveDlg::UpdateMDVecType(CString &strTimePt)
{
	// "read-only" combo has only one choice at a time
	m_cboMvmntType.ResetContent();
	if (strTimePt.IsEmpty() || strTimePt == CMotionDiagram::szWholePeriod) {
		m_cboMvmntType.AddString("average");
	} else { 
		m_cboMvmntType.AddString("instantaneous");
	}
	m_cboMvmntType.SetCurSel(0);
}

void CVectorMoveDlg::OnSelchangeAngular() 
{
	// update temp obj here so can use it to set prefix.
	UpdateAngularFlag();

	// and update name prefix. (Leaves rest of text)
	m_editName.SetPrefix(m_pTempObj->GetLabelPrefix());
}

void CVectorMoveDlg::OnSelchangeZdir() 
{
	int nZDir = m_cboZDir.GetCurSel();
	if (nZDir < 0 || nZDir > ZDIR_MAX) return;

	// enable degree edit accordingly
	if (nZDir == ZDIR_NONE) {
		//m_editOrientation.EnableWindow(TRUE);
		// static icon showing ccw orientation
		//m_stcVecAng.ShowWindow(SW_SHOW);	
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
	
	// mainly to change phi/theta label if needed:
	UpdateComponents();
}

// Keep edit control contents and unknown check box in sync:
// blank string <=> unknown checked
void CVectorMoveDlg::OnCheckUnknown() 
{
	if (m_btnUnknown.GetCheck()) {
		m_editValue.SetWindowText("");
	} else {
		m_editValue.SetFocus();
	}
}

void CVectorMoveDlg::OnChangeGivenValue() 
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

