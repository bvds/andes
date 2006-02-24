// TimeConstantDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "TimeConstantDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTimeConstantDlg dialog
//
// Generally copied from CSystemDlg, so uses same control names.
// Differs slightly in that it always defines a variable rather than a drawn object.
// Note also variable's m_strBody member may contain a space-separated list of body names
// in case of equivalent resistance; this must be packed/unpacked when transferring
// to list box selection.


CTimeConstantDlg::CTimeConstantDlg(CDrawObj* pObj /*=NULL*/, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CTimeConstantDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CTimeConstantDlg)
	//}}AFX_DATA_INIT
}

void CTimeConstantDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTimeConstantDlg)
	DDX_Control(pDX, IDC_STATIC_EQUALS, m_stcEquals);
	DDX_Control(pDX, IDC_GIVEN_BOX, m_stcGiven);
	DDX_Control(pDX, IDC_STATIC_OR, m_stcOr);
	DDX_Control(pDX, IDC_CHECK_UNKNOWN, m_btnUnknown);
	DDX_Control(pDX, IDC_GIVEN_VALUE, m_editValue);
	DDX_Control(pDX, IDC_LSYSTEM_NAME, m_stcLet);
	DDX_Control(pDX, IDC_MULTIPLE_BODY_LIST, m_listBodies);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_MULTIPLE_BODY_LIST, &m_pDocument->m_strObjects);
	// time not used for resistance
 	// DDX_FillList(pDX, IDC_TIME_TEXT, &m_pDocument->m_strTimes);
	// DDX_AddUserTimes(pDX, IDC_TIME_TEXT, &m_pDocument->m_Variables);
	
	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(CTimeConstantDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CTimeConstantDlg)
	ON_BN_CLICKED(IDC_CHECK_UNKNOWN, OnCheckUnknown)
	ON_EN_CHANGE(IDC_GIVEN_VALUE, OnChangeGivenValue)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTimeConstantDlg message handlers

BOOL CTimeConstantDlg::OnInitDialog() 
{
	CDrawObjDlg::OnInitDialog();
	
	// TODO: Add extra initialization here
	if (m_bSought){
		m_stcLet.ShowWindow(SW_HIDE);
		m_editName.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_LABEL);
		SetWindowText("Define Sought");
	}

	m_listBodies.SetFocus();
	return FALSE;
	
	//return TRUE;  // return TRUE unless you set the focus to a control
	                // EXCEPTION: OCX Property Pages should return FALSE
}

void CTimeConstantDlg::InitVariableDlg()
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

	// Transfer given value/unknown bit from controls to variable
	m_editValue.SetWindowText(((CVariable*)m_pTempObj)->m_strValue);
	// sync unknown check box with value
	OnChangeGivenValue();
}

void CTimeConstantDlg::UpdateTempVariable()
{
	CVariable * pVar = (CVariable*) m_pTempObj;
	
	// in case of multiple bodies, get list of selected strings in multi-sel list box
	// into single space-separated list for storage into var's m_strObject member
	int nSelItems = m_listBodies.GetSelCount();// number of items selected
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

	m_editName.GetRichEditText(pVar->m_strName);

	CString strValue;
	m_editValue.GetWindowText(strValue);
	((CVariable*)m_pTempObj)->m_strValue= strValue;
	
	// for vars, also need to set variable quant type and definition strings
	pVar->m_strQuantName = "time constant";
	pVar->m_strDef = "Time constant of " + pVar->m_strObject; 
}

void CTimeConstantDlg::OnOK()
{
	// Make sure all visible controls filled in	
	if (m_listBodies.GetSelCount() < 2)
	{
		theApp.DoWarningMessage("Please select at least two components", this);
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
void CTimeConstantDlg::OnCheckUnknown() 
{
	if (m_btnUnknown.GetCheck()) {
		m_editValue.SetWindowText("");
	} else {
		m_editValue.SetFocus();
	}
}

void CTimeConstantDlg::OnChangeGivenValue() 
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
