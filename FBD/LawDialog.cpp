// LawDialog.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "helpifc.h"
#include "mainfrm.h"
#include "fbddoc.h"
#include "LgDialog.h"
#include "LawDialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#include "LogEdit.h"
#include "Lgdialog.h"


/////////////////////////////////////////////////////////////////////////////
// CLawDialog dialog


CLawDialog::CLawDialog(CPrincObj* pPrinc /*=NULL*/, CWnd* pParent /*=NULL*/)
: CCheckedDlg(CLawDialog::IDD, pParent)
{
	m_pPrinc = pPrinc;
	//{{AFX_DATA_INIT(CLawDialog)
	m_strPrinciple = _T("");
	//}}AFX_DATA_INIT
	if (m_pPrinc) { 
		// transfer props from object into members
		m_strPrinciple = m_pPrinc->m_strLaw;
	
		// copy body list. (m_listBodies is empty since just constructed)
		POSITION pos = m_pPrinc->m_listBodies.GetHeadPosition();
		while (pos != NULL) {
			m_listBodies.AddTail(m_pPrinc->m_listBodies.GetNext(pos));
		}
		// also save original state of object to revert on cancel
		m_defOrig = *m_pPrinc;
	}
}


void CLawDialog::DoDataExchange(CDataExchange* pDX)
{
	CCheckedDlg::DoDataExchange(pDX);

	//{{AFX_DATA_MAP(CLawDialog)
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	DDX_Control(pDX, IDOK, m_btnOK);
	DDX_Control(pDX, IDC_BODY, m_lbBodies);
	DDX_Control(pDX, IDC_PRINCIPLE, m_lbPrinciples);
	//}}AFX_DATA_MAP
	if (!pDX->m_bSaveAndValidate)
		FillLawList();	// must do after subclass, before transfer selected string
	DDX_LBString(pDX, IDC_PRINCIPLE, m_strPrinciple);
	DDX_FillList(pDX, IDC_BODY, &((CFBDDoc*)theApp.GetDocument())->m_strObjects);
	DDX_LBMultiSel(pDX, IDC_BODY, m_listBodies);
}

BEGIN_CTL_TBL(CLawDialog)
	"law-name",		IDC_PRINCIPLE,
	"system-bodies", IDC_BODY,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CLawDialog)

BEGIN_MESSAGE_MAP(CLawDialog, CCheckedDlg)
	//{{AFX_MSG_MAP(CLawDialog)
	ON_COMMAND(ID_CONTROL_WHATSWRONG, OnControlWhatswrong)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLawDialog message handlers

BOOL CLawDialog::OnInitDialog() 
{
	LogEventf(EV_DLG_PRINCIPLE, "");

	CCheckedDlg::OnInitDialog();	// inits everything in DoDataExchange
	
	// We must explicitly center ourselves since we will be created modeless
	// (DrawObjDialogs place themselves with MoveDlgToBtmRight here)
	CenterWindow();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
void CLawDialog::FillLawList()
{
	// Populate Principle choice list box:
	m_lbPrinciples.AddString(CPrincObj::szKinematics);
	m_lbPrinciples.AddString(CPrincObj::szNewtonsFirst);
	m_lbPrinciples.AddString(CPrincObj::szNewtonsSecond);
	m_lbPrinciples.AddString(CPrincObj::szConsOfEnergy);
	m_lbPrinciples.AddString(CPrincObj::szNetWork);
}

void CLawDialog::OnOK() 
{
	int nSelItems = m_lbBodies.GetSelCount();
	if (nSelItems == 0)
	{
		theApp.DoWarningMessage("Please select one or more bodies", this);
		return;
	}
	if (m_lbPrinciples.GetCurSel() == LB_ERR)
	{
		theApp.DoWarningMessage("Please select a law application", this);
		return;
	}

	// Must transfer props from controls into member vars 
	UpdateData();	// controls into member vars

	// and then apply into checkable object.
	m_pPrinc->SetLaw(m_strPrinciple);
	m_pPrinc->SetBodies(m_listBodies);
	m_pPrinc->m_status = statusUnknown; // until new props checked.
	// property change means doc modified here. 
	// May revert if edit is cancelled, but we won't track that.
	theApp.GetDocument()->SetModifiedFlag();

	// Could send hint now to show updated principle immediately with current status
	// = unknown (black). Or could just wait till the check result to avoid flickering.
	theApp.GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_PRINC, m_pPrinc);
	
	if (! CheckDialog())
		return;

	CCheckedDlg::OnOK(); 
}

BOOL CLawDialog::CheckDialog()
{
	BOOL bCorrect = TRUE;	// until error found

	//do not want re-entrant DDE calls
	EnableWindow(FALSE);

	// Check obj with help system.  Updates its status and error list
	m_pPrinc->CheckObject();

	// apply updated slot statuses to dialog controls 
	bCorrect = UpdateStatuses(m_pPrinc->m_errors);

	// display updated principle with new status.
	theApp.GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_PRINC, m_pPrinc);

	// done updating, user can interact again
	EnableWindow(TRUE);	

	if (!bCorrect)
	{
		// errors: prompt if object unchanged from last submission 
		if (m_defLast == *m_pPrinc)
		{
			CString str = "You still have errors inside this dialog. Are you sure you want to exit?";
			if (theApp.DoWarningMessage(str, this, MB_YESNO) != IDCANCEL)
				return TRUE;
		}
	}

	// update save last definition
	m_defLast = *m_pPrinc;

	return bCorrect;
}

void CLawDialog::OnControlWhatswrong() 
{
	// normal args are label, id. But principles don't have labels
	// just include law-name (conc'd with bodylist?) for readability
	CString strLawName = m_pPrinc->GetHelpSysName();
	CString strStageId = m_pPrinc->GetStageId();
	LogEventf(EV_DLG_WHATSWRONG, "%s %s", strLawName, strStageId);
	
	// need to find out if it's law or system being asked about, since
	// help API uses different arguments for each (?)
	CWnd* pCtrl = GetFocus();
	if (pCtrl == NULL) return;
	CString strItem;
	if (pCtrl->GetDlgCtrlID() == IDC_PRINCIPLE) {
		strItem = "Law";
	} else if (pCtrl->GetDlgCtrlID() == IDC_BODY) {
		strItem = "System";
	} else {
		TRACE("LawDlg::Whatswrong called on bad ctrl (id=%d)\n",pCtrl->GetDlgCtrlID());
		return;
	}
	
	LPCTSTR pszResult = HelpSystemExecf("(why-wrong-plan-item %s %s)", strStageId, strItem);
	// Display result in hint dialog, which knows how to parse it.
	// Ask frame to show result in hint window
	theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong);
}

void CLawDialog::OnCancel()
{
	// see if princ modified from orig
	if (! (m_defOrig == *m_pPrinc)) {

		// revert object to saved original state (should also be assignment op)
		m_defOrig.CopyTo(*m_pPrinc);

		// send an update hint to display updated principle with new status.
		theApp.GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_PRINC, m_pPrinc);
	}

	CCheckedDlg::OnCancel();
}


