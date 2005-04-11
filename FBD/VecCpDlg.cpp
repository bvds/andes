// VecCpDlg.cpp : implementation file
//
//$Id: VecCpDlg.cpp,v 1.2 2005/04/11 18:53:54 anders Exp $

#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "VecCpDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVectorCompDlg dialog


CVectorCompDlg::CVectorCompDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CVectorCompDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CVectorCompDlg)
	//}}AFX_DATA_INIT
}

void CVectorCompDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVectorCompDlg)
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_VECTOR_NAME_TEXT, m_editName);
	DDX_Control(pDX, IDC_COMPONENT_OF, m_cboCompOf);
	DDX_Control(pDX, IDC_COMP_DIRECTION, m_cboCompDirList);
	//}}AFX_DATA_MAP
	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CVectorCompDlg)
	"name",	IDC_VECTOR_NAME_TEXT,
	"of", IDC_COMPONENT_OF,
	"dir", IDC_COMP_DIRECTION,
	// accept old long names for backwards compatibility:
	"vector-name",	IDC_VECTOR_NAME_TEXT,
	"component-of", IDC_COMPONENT_OF,
	"component-direction", IDC_COMP_DIRECTION,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CVectorCompDlg)

BEGIN_MESSAGE_MAP(CVectorCompDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CVectorCompDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVectorCompDlg message handlers

BOOL CVectorCompDlg::OnInitDialog() 
{
	LogEventf(EV_VECTOR_DLG, "%s |%s|", m_pObj->m_strId, OBJ_NAME(m_pObj));
	// Base class will load mapped data member values 
	// into controls via DDX and bind control member vars.
	// Note this doesn't initialize the droplist control
	// values, since the choice lists  have not been initialized. 
	CDrawObjDlg::OnInitDialog();

	if (!m_pTempObj)
	{// if  temp obj doesn't exist, create it
		m_pTempObj = new CVector();
		((CVector*)m_pTempObj)->m_nVectorType = VECTOR_COMPONENT;
	}
	// Further initialize the controls.

	if (m_pDocument == NULL) return TRUE;	// shouldn't happen!

	// run through document to populate Body, Agent, and
	// ComponentOf lists. Use reverse order so more recent obs are first
	POSITION pos = m_pDocument->m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = m_pDocument->m_objects.GetPrev(pos);
		if (  pObj->IsKindOf(RUNTIME_CLASS(CVector)) &&
			!( ((CVector*)pObj)->m_nVectorType == VECTOR_COMPONENT ) &&
			! pObj->m_strName.IsEmpty()   )
				m_cboCompOf.AddString(pObj->m_strName);
		/*
		if (pObj->IsKindOf(RUNTIME_CLASS(CAxes))
			// include in component direction?
		*/
	}

	// Cheap: do another UpdateData just to transfer string data member values 
	// into dropdown lists 

	// initialize default values of choice lists if unset from data members
	if (((CVector*)m_pTempObj)->m_strCompOf.IsEmpty())		// body not yet specified
		m_cboCompOf.SetCurSel(0);
	else
		m_cboCompOf.SelectStringExact(((CVector*)m_pTempObj)->m_strCompOf);
	// other choices not yet specified?


	return TRUE;  // return TRUE unless you set the focus to a control
}
	
void CVectorCompDlg::OnOK() 
{
	//Make sure all of dialog completed	
	if (IsEmpty(&m_cboCompOf))
	{
		theApp.DoWarningMessage("Please select a vector", this);
		return;
	}
	if (IsEmpty(&m_cboCompDirList))
	{
		theApp.DoWarningMessage("Please select an axis", this);
		return;
	}

	UpdateTempComp();

	if (!IsValidLabel(m_pTempObj->m_strName)) return;

	if (!m_pTempObj->IsValid())	return;

	if (!CheckDialog())	return;

	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();

}

void CVectorCompDlg::InitObjectDlg()
{
	m_editName.SetWindowText(m_pTempObj->m_strName);
	m_cboCompDirList.SelectStringExact(((CVector*)m_pTempObj)->m_strCompDir);
	m_cboCompOf.SelectStringExact(((CVector*)m_pTempObj)->m_strCompOf);
}

void CVectorCompDlg::UpdateTempComp()
{
	m_editName.GetWindowText(m_pTempObj->m_strName);
	((CVector*)m_pTempObj)->m_strCompDir = GetCurString(&m_cboCompDirList);
	((CVector*)m_pTempObj)->m_strCompOf = GetCurString(&m_cboCompOf);

}
