// MotDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"	// for logging
#include "MotDlg.h"
#include "FBDDoc.h"		// for body lists

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMotionDlg dialog


CMotionDlg::CMotionDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CMotionDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CMotionDlg)
	m_nIntervalType = 0;
	m_strName = _T("");
	m_strBody = _T("");
	m_strUnits = _T("");
	m_nInterval = 0;
	//}}AFX_DATA_INIT
}


void CMotionDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMotionDlg)
	DDX_Control(pDX, IDC_TIME_INTERVAL, m_editInterval);
	DDX_Control(pDX, IDC_TIME_UNITS, m_cboUnits);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_MOTION_BODY_LIST, m_listBodies);
	DDX_Radio(pDX, IDC_DEFAULT_TIMES, m_nIntervalType);
	DDX_Text(pDX, IDC_SYSTEM_NAME, m_strName);
	DDX_Text(pDX, IDC_TIME_INTERVAL, m_nInterval);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_MOTION_BODY_LIST, &((CFBDDoc*)theApp.GetDocument())->m_strObjects);
	DDX_LBStringExact(pDX, IDC_MOTION_BODY_LIST, m_strBody);
	DDX_CBStringExact(pDX, IDC_TIME_UNITS, m_strUnits);
	 
}


BEGIN_MESSAGE_MAP(CMotionDlg, CLogDialog)
	//{{AFX_MSG_MAP(CMotionDlg)
	ON_BN_CLICKED(IDC_DEFAULT_TIMES, OnDefaultTimes)
	ON_BN_CLICKED(IDC_MEASURED_TIMES, OnMeasuredTimes)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMotionDlg message handlers

BOOL CMotionDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	LogEventf(EV_DLG_RULER, "");
	
	// if there's only one body in problem, select it by default
	if (m_listBodies.GetCount() == 1) {
		m_listBodies.SetCurSel(0);
	}

	if (theApp.m_bTrainMode){
		theApp.SendTrainer(this, ID_TRULER_DLG);
	}
	else{
		m_listBodies.SetFocus();
		return FALSE;
	}

	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

int CMotionDlg::GetTrainerId(int ctrlId)
{
	ctrlId = ctrlId + 31850;
	return ctrlId;
}

void CMotionDlg::OnOK() 
{
	theApp.SendTrainer(this, ID_HELP_DRAWPOSITION);
	
	LogEventf(EV_BTN_CLICK, "%d OK", m_Ok.GetDlgCtrlID());
	CLogDialog::OnOK();
}

void CMotionDlg::OnDefaultTimes() 
{
	theApp.SendTrainer(this, ID_TRULER_DTIME);
}

void CMotionDlg::OnMeasuredTimes() 
{
	theApp.SendTrainer(this, ID_TRULER_MTIME);
}
