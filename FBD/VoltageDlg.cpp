// VoltageDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "VoltageDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CVoltageDlg dialog


CVoltageDlg::CVoltageDlg(CDrawObj* pObj /*= NULL*/, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CVoltageDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CVoltageDlg)
	m_nType = -1;
	//}}AFX_DATA_INIT
}


void CVoltageDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVoltageDlg)
	DDX_Control(pDX, IDC_LVARIABLE_NAME, m_stcLet);
	DDX_Control(pDX, IDC_TIME, m_cboTime);
	DDX_Control(pDX, IDC_COMPONENT, m_cboComponent);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDOK, m_btnOk);
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	//}}AFX_DATA_MAP

	// init choices
	DDX_FillList(pDX, IDC_COMPONENT, &m_pDocument->m_strObjects);
	DDX_AddEquivComponents(pDX, IDC_COMPONENT, &m_pDocument->m_Variables);
	DDX_FillList(pDX, IDC_TIME, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME, &m_pDocument->m_Variables);

	// init values from object (via InitVariableDlg)
	CDrawObjDlg::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(CVoltageDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CVoltageDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVoltageDlg message handlers


BOOL CVoltageDlg::OnInitDialog() 
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

void CVoltageDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;
	SetWindowText("Variable definition");


	// voltage across component
	m_cboComponent.SelectStringExact(pVar->m_strObject);
	m_cboTime.SelectStringExact(pVar->m_strTime);;
}

void CVoltageDlg::UpdateTempVariable()
{
	CVariable * pTempVar = (CVariable*) m_pTempObj;

	pTempVar->m_strForceType = "across";
	pTempVar->m_strObject = GetCurString(&m_cboComponent);
	pTempVar->m_strAgent.Empty(); // clear any prior value
	pTempVar->m_strTime = GetCurString(&m_cboTime);
	m_editName.GetRichEditText(pTempVar->m_strName);
	
	// for vars, also need to set variable quant type and definition strings
	pTempVar->m_strValue = "voltage";
	CString strTime;
	if (! pTempVar->m_strTime.IsEmpty())
		strTime = " at time " + pTempVar->m_strTime;
	if (pTempVar->m_strForceType == "between") {
		pTempVar->m_strDef = "Voltage between " + pTempVar->m_strObject + 
				             " and " + pTempVar->m_strAgent + strTime;
	} else {
		pTempVar->m_strDef = "Voltage across " + pTempVar->m_strObject + strTime;
	}

}

/* Now unused
void CVoltageDlg::OnUpdateType() 
{
	BOOL bBetween = m_btnBetween.GetCheck();
	m_cboPoint1.EnableWindow(bBetween);
	m_cboPoint2.EnableWindow(bBetween);
	m_cboComponent.EnableWindow(!bBetween);
}
*/

void CVoltageDlg::OnOK()
{
	// Make sure all visible controls filled in	
	if (IsEmpty(&m_cboComponent)) {
		theApp.DoWarningMessage("Please select a component", this);
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
