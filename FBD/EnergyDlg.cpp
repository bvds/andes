// EnergyDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "EQEDit.h"
#include "EnergyDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEnergyDlg dialog


CEnergyDlg::CEnergyDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CEnergyDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CEnergyDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CEnergyDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CEnergyDlg)
	DDX_Control(pDX, IDC_LVARIABLE_NAME, m_stcLet);
	DDX_Control(pDX, IDC_PE_LABEL, m_stcPE);
	DDX_Control(pDX, IDOK, m_btnOk);
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	DDX_Control(pDX, IDC_AGENT_LABEL, m_stcAgent);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTime);
	DDX_Control(pDX, IDC_NRGBODY, m_cboBody);
	DDX_Control(pDX, IDC_STATIC_VALUE, m_stcValue);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDC_TIME, m_cboTime);
	DDX_Control(pDX, IDC_NRG_TYPE, m_cboEnergyType);
	DDX_Control(pDX, IDC_NRG_AGENT, m_cboAgent);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_NRG_AGENT, &m_pDocument->m_strObjects);
	DDX_FillList(pDX, IDC_NRGBODY, &m_pDocument->m_strObjects);
	DDX_FillList(pDX, IDC_TIME, &m_pDocument->m_strTimes);
	DDX_AddCompoundBodies(pDX, IDC_NRGBODY, &m_pDocument->m_objects);
	DDX_AddUserTimes(pDX, IDC_TIME, &m_pDocument->m_Variables);

	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CEnergyDlg)
	"name", IDC_CUSTOM_LABEL,
	"type", IDC_NRG_TYPE,
	"body", IDC_NRGBODY,
	"time", IDC_TIME,
	"agent", IDC_NRG_AGENT,
	// accept alternate names for backwards compatibility:
	// earlier name for body slot:
	"object", IDC_NRGBODY,
	// even older qualified names: May occur in old logs. 
	// Past helpsys returned "vector-body" etc here, which was silently ignored.
	"energy-name", IDC_CUSTOM_LABEL,
	"energy-type", IDC_NRG_TYPE,
	"energy-object", IDC_NRGBODY,
	"energy-time", IDC_TIME,
	"energy-agent", IDC_NRG_AGENT,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CEnergyDlg)

IMPLEMENT_DYNAMIC(CEnergyDlg, CDrawObjDlg)

BEGIN_MESSAGE_MAP(CEnergyDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CEnergyDlg)
	ON_CBN_SELCHANGE(IDC_NRG_TYPE, OnSelchangeNrgType)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEnergyDlg message handlers

BOOL CEnergyDlg::OnInitDialog() 
{
	CDrawObjDlg::OnInitDialog();
	
	// adjust dialog for special-purpose uses. High-level plan never got to include
	// energy as system property so that code is unneeded, but included anyway.
	if (m_bSought || m_bProp){
		m_editName.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_LABEL);
		m_stcLet.ShowWindow(SW_HIDE);
		if (m_bSought){
			SetWindowText("Define Sought");
		}
		else {
			SetWindowText("Define Property");
		}
		UpdatePlanStrings(&m_cboBody);
	}

/*
	m_cboEnergyType.SetItemData(0, ID_NRGPOTENTIAL);
	m_cboEnergyType.SetItemData(1, ID_NRGKINETIC);
	m_cboEnergyType.SetItemData(2, ID_NRGTOTAL);
*/
	OnSelchangeNrgType();
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CEnergyDlg::InitVariableDlg()
{
	m_cboBody.SelectStringExact(((CVariable*)m_pTempObj)->m_strObject );
	m_cboAgent.SelectStringExact(((CVariable*)m_pTempObj)->m_strAgent );
	m_cboTime.SelectStringExact(((CVariable*)m_pTempObj)->m_strTime) ;
	m_cboEnergyType.SelectStringExact(((CVariable*)m_pTempObj)->m_strForceType) ;
}

void CEnergyDlg::OnOK() 
{
	//Ensure dialog complete
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
	if (IsEmpty(&m_cboEnergyType))
	{
		theApp.DoWarningMessage("Please select an energy type", this);
		return;
	}

	//Update data into temporary object
	UpdateTempVariable();

	if (! m_bSought)
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

void CEnergyDlg::UpdateTempVariable()
{
	((CVariable*)m_pTempObj)->m_strObject = GetCurString(&m_cboBody);
	((CVariable*)m_pTempObj)->m_strAgent = GetCurString(&m_cboAgent);
	((CVariable*)m_pTempObj)->m_strTime = GetCurString(&m_cboTime);
	((CVariable*)m_pTempObj)->m_strForceType = GetCurString(&m_cboEnergyType);
	((CVariable*)m_pTempObj)->m_strValue = "Energy";

	CString str;
	m_editName.GetRichEditText(str);
	m_pTempObj->m_strName = str;

	CString strTime = "";
	if (!((CVariable*)m_pTempObj)->m_strTime.IsEmpty())
	{
		CString str;
		m_stcTime.GetWindowText(str);
		if (str.Find('&')!=-1)	// obsolete test ? -AW
			strTime = " at time " + ((CVariable*)m_pTempObj)->m_strTime;
		else
			strTime = " during " + ((CVariable*)m_pTempObj)->m_strTime;
	}
	CString strAgent = "";
	if (!((CVariable*)m_pTempObj)->m_strAgent.IsEmpty())
		strAgent = " due to " + ((CVariable*)m_pTempObj)->m_strAgent;

	((CVariable*)m_pTempObj)->m_strDef = 
		((CVariable*)m_pTempObj)->m_strForceType + " " +
			((CVariable*)m_pTempObj)->m_strValue + " of " + 
				((CVariable*)m_pTempObj)->m_strObject + 
					strAgent +	strTime ;

}

void CEnergyDlg::OnSelchangeNrgType() 
{
/*  // changed to use energy type, and no agent
	int Sel = m_cboEnergyType.GetCurSel();
	int nData = m_cboEnergyType.GetItemData(nSel);
	
	// KE and Total Mechanical have one body, 
	// For poential energy we have two bodies:
	// Energy of type [Potential] "due to interaction" 
	// "of body" _____ "and" _______
	m_stcPE.ShowWindow(nData == ID_NRGPOTENTIAL ? SW_SHOW : SW_HIDE);
	m_stcAgent.EnableWindow(nData == ID_NRGPOTENTIAL);
	EnableComboBox(&m_cboAgent, (nData == ID_NRGPOTENTIAL));

	// set appropriate prefix
	if (nData == ID_NRGPOTENTIAL)
		m_editName.SetPrefix("U");
	else if (nData == ID_NRGKINETIC)
		m_editName.SetPrefix("K");
	else if (nData == ID_NRGTOTAL)
		m_editName.SetPrefix("ME");
*/
	EnergyType type = (EnergyType) m_cboEnergyType.GetCurSel();

	if (type == TotalMechanical)
		m_editName.SetPrefix("ME");
	else if (type == Kinetic)
		m_editName.SetPrefix("K");
	else if (type == Gravitational || type == Elastic 
		   || type == Electric)
		m_editName.SetPrefix("U");
}

CLabelRichEdit* CEnergyDlg::GetLabelCtrl()
{
	return &m_editName;
}




