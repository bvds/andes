// SystemDlg.cpp : implementation file
// 
// $Id: sysdlg.cpp,v 1.5 2007/06/27 01:53:03 anders Exp $

#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "SysDlg.h"
#include "VarView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSystemDlg dialog

CSystemDlg::CSystemDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CSystemDlg::IDD, pObj, pParent)
{
	m_bInPlan = FALSE;
	//{{AFX_DATA_INIT(CSystemDlg)
	//}}AFX_DATA_INIT
}

void CSystemDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSystemDlg)
	DDX_Control(pDX, IDC_LSYSTEM_NAME, m_stcLabel);
	DDX_Control(pDX, IDC_TIME_LABEL, m_stcTimeList);
	DDX_Control(pDX, IDC_MULTIPLE_BODY_LIST, m_listBodies);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_TIME_TEXT, m_cboTimeList);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_MULTIPLE_BODY_LIST, &m_pDocument->m_strObjects);
 	DDX_FillList(pDX, IDC_TIME_TEXT, &m_pDocument->m_strTimes);
	DDX_AddUserTimes(pDX, IDC_TIME_TEXT, &m_pDocument->m_Variables);
	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CSystemDlg)
	"name",	 IDC_CUSTOM_LABEL,
	"bodies", IDC_MULTIPLE_BODY_LIST,
	"time",	 IDC_TIME_TEXT,
	// to accept old long names for backwards compatibility:
	"system-name",	 IDC_CUSTOM_LABEL,
	"system-bodies", IDC_MULTIPLE_BODY_LIST,
	"system-time",	 IDC_TIME_TEXT,
	"OK",			 IDOK,
	"Cancel",		 IDCANCEL,
END_CTL_TBL(CSystemDlg)

BEGIN_MESSAGE_MAP(CSystemDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CSystemDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSystemDlg message handlers

BOOL CSystemDlg::OnInitDialog() 
{
	LogEventf(EV_DLG_SYSTEM, "%s |%s|",m_pObj->m_strId, OBJ_NAME(m_pObj));

	CDrawObjDlg::OnInitDialog();

	if (!m_pDocument)	return TRUE;


	if (!CVarView::HasFeature("BODY-TIME")) 
	{
		m_cboTimeList.ShowWindow(SW_HIDE);
		m_stcTimeList.ShowWindow(SW_HIDE);
		//remove space taken by these hidden controls
		Remove(IDC_BOX_TIME);
	}

	// set initial choice values 
	// following reloads combo box values from member vars now that the
	// choices are initialized
	if (!m_pTempObj)//if temp obj doesn't exist coming in, create it
		m_pTempObj = new CSystem();
 
	if (theApp.m_bTrainMode){
		theApp.SendTrainer(this, ID_TBODY_DLG);
	}
	else if (m_bInPlan)
	{
		m_editName.ShowWindow(SW_HIDE);
		m_stcLabel.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_LABEL);

	}
	else {
		m_listBodies.SetFocus();
		return FALSE;
	}

	return TRUE;  // return TRUE unless you set the focus to a control
}

void CSystemDlg::InitObjectDlg()
{
	// show initial defaults if values not set coming in
	if (!((CSystem*)m_pTempObj)->m_strBodies.IsEmpty())
		SelectBodies(((CSystem*)m_pTempObj)->m_strBodies);

	m_cboTimeList.SelectStringExact(((CSystem*)m_pTempObj)->m_strTime);
	m_editName.SetWindowText(m_pTempObj->m_strName);

}

void CSystemDlg::OnOK() 
{
	// Ensure dialog complete
	if (m_listBodies.GetSelCount() == 0)
	{
		theApp.DoWarningMessage("Please select one or more bodies", this);
		return;
	}
	if (IsEmpty(&m_cboTimeList))
	{
		theApp.DoWarningMessage("Please select a time", this);
		return;
	}

	UpdateTempSystem();
	// First make sure a valid label is entered
	// if in plan, no label needed
	if (!m_bInPlan){

		CString str = m_pTempObj->m_strName;
		str.Remove('$');
		
		if (!IsValidLabel(str))	return;
		//check for uniqeness
		if (!m_pTempObj->IsValid())	return;
	}

	if (!CheckDialog())	return;
	
	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();
}

void CSystemDlg::UpdateTempSystem()
{
	int nSelItems = m_listBodies.GetSelCount();// number of items selected

	if (nSelItems == 1)
		((CSystem*)m_pTempObj)->m_nSystemType = SYSTEM_SINGLE_BODY;
	else if (nSelItems > 1)
		((CSystem*)m_pTempObj)->m_nSystemType = SYSTEM_COMPOUND_BODY;

	// if multiple bodies, copy strings from multiple selection list box into variable
	// pack selected items into space-separated list left in m_strBodies string
	int SelectSet[10];						// set of selected indices
	m_listBodies.GetSelItems(10, SelectSet);
	
	CString strBody;
	CString strBuf;
	for (int i=0; i < nSelItems; i++)
	{
		m_listBodies.GetText(SelectSet[i], strBody);
		strBuf += strBody + " ";
	}
	strBuf.TrimRight();
	((CSystem*)m_pTempObj)->m_strBodies = strBuf;// space-separated list of Bodies

	m_editName.GetWindowText(m_pTempObj->m_strName);
	((CSystem*)m_pTempObj)->m_strTime = GetCurString(&m_cboTimeList);


}
// SelectBodies -- makes sure dialog's body selection list box reflects 
// user's last selection when dialog reopened
// arg is space-separated list of body names
void CSystemDlg::SelectBodies(CString& strBodies)
{
	char* buf = (char*) malloc(strlen(strBodies) + 1);	
	strcpy(buf, strBodies);
	char seps[]   = " ";
	char *token;
	// Non re-entrant strtok tokenizes in place by inserting NULs into buf
	token = strtok( buf, seps ); 
	int i = 0;
	while( token != NULL )
	{
		CString strBody = (token);

		int nSel = m_listBodies.FindString(-1, strBody);
		m_listBodies.SetSel(nSel, TRUE);

		token = strtok( NULL, seps );	//	returns ptr to next token in buf

	}
	free((void*)buf);

}

int CSystemDlg::GetTrainerId(int ctrlId)
{
	if (ctrlId == IDC_CUSTOM_LABEL)
		ctrlId = IDC_SYSTEM_NAME;	// map to old label ctrl id 

	return ctrlId + 31880;	// magic offset to tcard command id
}

CLabelRichEdit* CSystemDlg::GetLabelCtrl()
{
	return &m_editName;
}
