// CurrentDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "CurrentDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCurrentDlg dialog


CCurrentDlg::CCurrentDlg(CDrawObj* pObj /*=NULL*/, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CCurrentDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CCurrentDlg)
	//}}AFX_DATA_INIT
}


void CCurrentDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CCurrentDlg)
	DDX_Control(pDX, IDC_LVARIABLE_NAME, m_stcLet);
	DDX_Control(pDX, IDC_CURRENT_THRU, m_btnThrough);
	DDX_Control(pDX, IDC_CURRENT_IN, m_btnIn);
	DDX_Control(pDX, IDC_COMPONENT, m_cboComponent);
	DDX_Control(pDX, IDC_BRANCH, m_cboBranch);
	DDX_Control(pDX, IDC_TIME, m_cboTime);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDOK, m_btnOK);
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	//}}AFX_DATA_MAP

	// init choices
	// "current-through" choices = components + points
	DDX_FillList(pDX, IDC_COMPONENT, &m_pDocument->m_strObjects);
	DDX_AddEquivComponents(pDX, IDC_COMPONENT, &m_pDocument->m_Variables);
	DDX_FillList(pDX, IDC_COMPONENT, &m_pDocument->m_strPositions);
	DDX_FillList(pDX, IDC_BRANCH, &m_pDocument->m_strBranches);
	DDX_FillList(pDX, IDC_TIME, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME, &m_pDocument->m_Variables);

	// init values from object (via InitVariableDlg)
	CDrawObjDlg::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(CCurrentDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CCurrentDlg)
	ON_BN_CLICKED(IDC_CURRENT_THRU, OnUpdateType)
	ON_BN_CLICKED(IDC_CURRENT_IN, OnUpdateType)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCurrentDlg message handlers

BOOL CCurrentDlg::OnInitDialog() 
{
	CDrawObjDlg::OnInitDialog();
	
	// TODO: Add extra initialization here
	if (m_bSought){
		m_stcLet.ShowWindow(SW_HIDE);
		m_editName.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_LABEL);
		SetWindowText("Define Sought");
	}
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CCurrentDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;
	SetWindowText("Variable definition");

	// Init subtype components button:
	
	if (pVar->m_strForceType == "through" // current through component
		|| pVar->m_strForceType.IsEmpty())		// default if unset
	{
		m_btnThrough.SetCheck(TRUE);
		m_cboComponent.SelectStringExact(pVar->m_strObject);
	} 
	else if (pVar->m_strForceType == "in") 	// current in branch
	{
		m_btnIn.SetCheck(TRUE);
		m_cboBranch.SelectStringExact(pVar->m_strObject);
	}
	OnUpdateType();

	m_cboTime.SelectStringExact(pVar->m_strTime);;
}

void CCurrentDlg::UpdateTempVariable()
{
	CVariable * pTempVar = (CVariable*) m_pTempObj;

	if (m_btnThrough.GetCheck()) {
		 pTempVar->m_strForceType = "through";
		 pTempVar->m_strObject = GetCurString(&m_cboComponent);
	}  else {
		 pTempVar->m_strForceType = "in";
		 pTempVar->m_strObject = GetCurString(&m_cboBranch);
		 
	}
	pTempVar->m_strTime = GetCurString(&m_cboTime);
	m_editName.GetRichEditText(pTempVar->m_strName);
	
	// for vars, also need to set variable quant type and definition strings
	pTempVar->m_strValue = "current";
	CString strTime;
	if (! pTempVar->m_strTime.IsEmpty())
		strTime = " at time " + pTempVar->m_strTime;

	pTempVar->m_strDef = "Current " + pTempVar->m_strForceType + " " + pTempVar->m_strObject + strTime;
}

void CCurrentDlg::OnOK()
{
	// Make sure all visible controls filled in	
	if (IsEmpty(&m_cboBranch)) {
		theApp.DoWarningMessage("Please select a branch", this);
		return;
	}
	if (IsEmpty(&m_cboComponent)) {
		theApp.DoWarningMessage("Please select a component or point", this);
		return;
	}
	if (IsEmpty(&m_cboTime)) {
		theApp.DoWarningMessage("Please select a time", this);
		return;
	}

	// Update temp obj with values from controls
	m_editName.GetWindowText(m_pTempObj->m_strName); // ?? temporarily sets plain text
	UpdateTempVariable();

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

void CCurrentDlg::OnUpdateType() 
{
	m_cboComponent.EnableWindow(m_btnThrough.GetCheck());
	m_cboBranch.EnableWindow(m_btnIn.GetCheck());
}
