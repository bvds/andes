// ResistanceDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ResistanceDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CResistanceDlg dialog
//
// Generally copied from CSystemDlg, so uses same control names.
// Differs slightly in that it always defines a variable rather than a drawn object.
// Note also variable's m_strBody member may contain a space-separated list of body names
// in case of equivalent resistance; this must be packed/unpacked when transferring
// to list box selection.


CResistanceDlg::CResistanceDlg(CDrawObj* pObj /*=NULL*/, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CResistanceDlg::IDD, pObj, pParent)
{
	m_bCapacitance = FALSE;
	//{{AFX_DATA_INIT(CResistanceDlg)
	//}}AFX_DATA_INIT
}

//
// CCapacitanceDlg is just a CResistanceDlg with m_bCapacitance initialized to TRUE
//
CCapacitanceDlg::CCapacitanceDlg(CDrawObj* pObj /*=NULL*/, CWnd* pParent /*=NULL*/)
	: CResistanceDlg(pObj, pParent)
{
	m_bCapacitance = TRUE;
}

void CResistanceDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CResistanceDlg)
	DDX_Control(pDX, IDC_STATIC_EQUALS, m_stcEquals);
	DDX_Control(pDX, IDC_GIVEN_BOX, m_stcGiven);
	DDX_Control(pDX, IDC_STATIC_OR, m_stcOr);
	DDX_Control(pDX, IDC_CHECK_UNKNOWN, m_btnUnknown);
	DDX_Control(pDX, IDC_GIVEN_VALUE, m_editValue);
	DDX_Control(pDX, IDC_MULTIPLE_INSTRUCTIONS, m_grpBox);
	DDX_Control(pDX, IDC_INSTRUCTIONS, m_stcInstructions);
	DDX_Control(pDX, IDC_LSYSTEM_NAME, m_stcLet);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDC_MULTIPLE_BODY_LIST, m_listBodies);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_TIME_TEXT, m_cboTimeList);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_MULTIPLE_BODY_LIST, &m_pDocument->m_strObjects);
	// time not used for resistance
 	// DDX_FillList(pDX, IDC_TIME_TEXT, &m_pDocument->m_strTimes);
	// DDX_AddUserTimes(pDX, IDC_TIME_TEXT, &m_pDocument->m_Variables);
	
	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(CResistanceDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CResistanceDlg)
	ON_BN_CLICKED(IDC_CHECK_UNKNOWN, OnCheckUnknown)
	ON_EN_CHANGE(IDC_GIVEN_VALUE, OnChangeGivenValue)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CResistanceDlg message handlers

BOOL CResistanceDlg::OnInitDialog() 
{
	CDrawObjDlg::OnInitDialog();
	
	// TODO: Add extra initialization here
	if (m_bSought){
		m_stcLet.ShowWindow(SW_HIDE);
		m_editName.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_LABEL);
		SetWindowText("Define Sought");
	} else {
		m_editValue.SetEventMask(ENM_CHANGE);
	}
	
	// if being used for capacitance, adjust default labels:
	if (m_bCapacitance) {
		m_grpBox.SetWindowText("Capacitance of");
		CString strTemp;
		m_stcInstructions.GetWindowText(strTemp);
		strTemp.Replace("resistance", "capacitance");
		m_stcInstructions.SetWindowText(strTemp);
	}

	m_listBodies.SetFocus();
	return FALSE;
	
	//return TRUE;  // return TRUE unless you set the focus to a control
	                // EXCEPTION: OCX Property Pages should return FALSE
}

void CResistanceDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;

	// select each name in body string
	if (!pVar->m_strObject.IsEmpty()) {
		CStringList listBodies;
		SplitStr(pVar->m_strObject, listBodies);
		for (POSITION pos = listBodies.GetHeadPosition(); pos != NULL; ) {
			CString strBody = listBodies.GetNext(pos);
			m_listBodies.SetSel(m_listBodies.FindString(-1, strBody), TRUE);
		}
	}
	// m_cboTimeList.SelectStringExact(pVar->m_strTime);

	// Transfer given value/unknown bit from controls to variable
	m_editValue.SetRichEditText(((CVariable*)m_pTempObj)->m_strValue);
	// sync unknown check box with value
	OnChangeGivenValue();
}

void CResistanceDlg::UpdateTempVariable()
{
	CVariable * pVar = (CVariable*) m_pTempObj;

	// set subtype to "equiv" if more than one resistor in list.
	int nSelItems = m_listBodies.GetSelCount();// number of items selected
	if (nSelItems > 1)
		pVar->m_strForceType = "equiv";
	else 
		pVar->m_strForceType.Empty();

	// in case of multiple bodies, get list of selected strings in multi-sel list box
	// into single space-separated list for storage into var's m_strObject member
	CString strBodyList;					// space separated list of selected bodies
	int SelectSet[32];						// holds set of selected indices
	m_listBodies.GetSelItems(32, SelectSet);
	CString strBody;
	for (int i=0; i < nSelItems; i++){
		m_listBodies.GetText(SelectSet[i], strBody);
		strBodyList += strBody + " ";
	}
	strBodyList.TrimRight();
	
	pVar->m_strObject = strBodyList;

	// pVar->m_strTime = GetCurString(&m_cboTimeList);
	m_editName.GetRichEditText(pVar->m_strName);

	CString strValue;
	m_editValue.GetRichEditText(strValue);
	((CVariable*)m_pTempObj)->m_strValue= strValue;
	
	// for vars, also need to set variable quant type and definition strings
	pVar->m_strQuantName = !m_bCapacitance ? "resistance" : "capacitance";
	// CString strTime;
	//if (! pVar->m_strTime.IsEmpty())
	//	strTime = " at time " + pVar->m_strTime;
	
	pVar->m_strDef = (!m_bCapacitance ? "Resistance of " : "Capacitance of ") 
		              + pVar->m_strObject; // +  strTime;
}

void CResistanceDlg::OnOK()
{
	// Make sure all visible controls filled in	
	if (m_listBodies.GetSelCount() == 0)
	{
		theApp.DoWarningMessage("Please select one or more components", this);
		return;
	}
	if (IsEmpty(&m_cboTimeList))
	{
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

// Keep edit control contents and unknown check box in sync:
// blank string <=> unknown checked
void CResistanceDlg::OnCheckUnknown() 
{
	if (m_btnUnknown.GetCheck()) {
		m_editValue.SetWindowText("");
	} else {
		m_editValue.SetFocus();
	}
}

void CResistanceDlg::OnChangeGivenValue() 
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
