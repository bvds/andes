// AxesDlg.cpp : implementation file
//
// $Id: Axesdlg.cpp,v 1.2 2005/04/11 18:53:54 anders Exp $
//

#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "AxesDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAxesDlg dialog


CAxesDlg::CAxesDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CAxesDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CAxesDlg)
	m_strXLabel = _T("X");
	//}}AFX_DATA_INIT
}


void CAxesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAxesDlg)
	DDX_Control(pDX, IDOK, m_btnOK);
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	DDX_Control(pDX, IDC_DIRECTION_TEXT, m_editDirection);
	DDX_Control(pDX, IDC_SYSTEM_LIST, m_cboSystemList);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDirection);
	DDV_MinMaxInt(pDX, ((CAxes*)m_pTempObj)->m_nDirection, 0, 89);
	//}}AFX_DATA_MAP
	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);

}

BEGIN_CTL_TBL(CAxesDlg)
	"dir",	IDC_DIRECTION_TEXT,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
	// long name for backwards compatibility
	"axes-direction",	IDC_DIRECTION_TEXT,
END_CTL_TBL(CAxesDlg)

BEGIN_MESSAGE_MAP(CAxesDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CAxesDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAxesDlg message handlers

BOOL CAxesDlg::OnInitDialog() 
{
	LogEventf(EV_DLG_AXES, "%s |%s|",m_pObj->m_strId, OBJ_NAME(m_pObj));
	// Base class will initialize control values from data members via ddx
	// Note won't work to init values in dropdown choice lists since the 
	// lists have not been initialized yet. 
	CDrawObjDlg::OnInitDialog();
	
	// fill system list with choices from document
	// use reverse order so most recently added systems are first
	ASSERT(m_pDocument != NULL);
	POSITION pos = m_pDocument->m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = m_pDocument->m_objects.GetPrev(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CSystem)))
			if (! pObj->m_strName.IsEmpty())
				m_cboSystemList.AddString(pObj->m_strName);
	}

	// !! cheat: if system not yet set, init to most recently added one
	// regardless of where axes drawn.
	if (m_cboSystemList.GetCurSel() < 0)
		m_cboSystemList.SetCurSel(0);

	// Initialize direction spinner control range.
	m_spinDirection.SetRange(0, 89);

	return TRUE;  // return TRUE unless you set the focus to a control
}


void CAxesDlg::OnOK() 
{
	UpdateTempAxes();

	// verify angle is in range. Our property transferring protocol means DDV is only 
	// used once get through check to final OK, and also that it will write new properties
	// through to underlying object. So we can't just rely on that to
	// enforce range constaints here.
	int nDir = ((CAxes*)m_pTempObj)->m_nDirection;
	if (nDir < 0 || nDir >= 90){
		theApp.DoWarningMessage("Please enter an integer between 0 and 89");
		return;
	}

	if ( !CheckDialog() )
		return;

	CDrawObjDlg::OnOK();
}


void CAxesDlg::UpdateTempAxes()
{
	((CAxes*)m_pTempObj)->m_strSystem = GetCurString(& m_cboSystemList);
	CString str;
	m_editDirection.GetWindowText(str);
		
	// !! what if following fails (not an integer)? value just remains unchanged.	
	sscanf(str, "%d", &((CAxes*)m_pTempObj)->m_nDirection);
}

void CAxesDlg::InitObjectDlg()
{
	m_cboSystemList.SelectString(-1, ((CAxes*)m_pTempObj)->m_strSystem );
	CString str;
	str.Format("%d", ((CAxes*)m_pTempObj)->m_nDirection);
	m_editDirection.SetWindowText(str);
	
}