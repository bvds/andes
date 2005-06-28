// LabRadDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "LabRadDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CRadiusDlg dialog


CRadiusDlg::CRadiusDlg(CDrawObj* pObj,CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CRadiusDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CRadiusDlg)
	//}}AFX_DATA_INIT
	if (m_pTempObj->m_strName.IsEmpty())
		m_pTempObj->m_strName = "r";
}


void CRadiusDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CRadiusDlg)
	DDX_Control(pDX, IDC_LRAD_TEXT, m_stcRad);
	DDX_Control(pDX, IDC_RADNAME_TEXT, m_stcLet);
	DDX_Control(pDX, IDOK, m_btnOK);
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	DDX_Control(pDX, IDC_BODIES, m_cboBodies);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_BODIES, &m_pDocument->m_strObjects);
	DDX_AddCompoundBodies(pDX, IDC_BODIES, &m_pDocument->m_objects);//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);

}

BEGIN_CTL_TBL(CRadiusDlg)
	"name",	IDC_CUSTOM_LABEL,
	"body", IDC_BODIES,
	// to accept old long names for backwards compatibility:
	"radius-name",	IDC_CUSTOM_LABEL,
	"radius-body", IDC_BODIES,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CRadiusDlg)

BEGIN_MESSAGE_MAP(CRadiusDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CRadiusDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRadiusDlg message handlers

BOOL CRadiusDlg::OnInitDialog() 
{
	LogEventf(EV_DLG_RADIUS, "%s |%s|", m_pObj->m_strId, OBJ_NAME(m_pObj));
	
	CDrawObjDlg::OnInitDialog();
	
	if ( !m_pDocument)
		return TRUE;

	// set initial choice values 
	// following reloads combo box values from member vars now that the
	// choices are initialized
	ASSERT(m_pTempObj);//shouldn't happen

 //	UpdateData(FALSE);

	if (m_bSought || m_bProp){
		m_editName.ShowWindow(SW_HIDE);
		m_stcLet.ShowWindow(SW_HIDE);
		Remove(IDC_BOX_LABEL);

		if (m_bSought){
			SetWindowText("Define Sought");
		}
		else{
			SetWindowText("Define Property");
		}
		UpdatePlanStrings(&m_cboBodies);
	}


	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


void CRadiusDlg::OnOK() 
{
	//Make sure all of dialog completed	
	if (IsEmpty(&m_cboBodies))
	{
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}

	//Update info from controls into temp object
	m_editName.GetWindowText(m_pTempObj->m_strName);
	if (m_pObj->IsKindOf(RUNTIME_CLASS(CRadius)))
		UpdateTempRadius();
	else
		UpdateTempVariable();

	if (m_bProp)
	{
		CTableRow* pThisProp = (CTableRow*)m_pTempObj;

		if (pThisProp->m_pTable)
		{
			CTableRow* pProp = pThisProp->m_pTable->GetMatchingProp(pThisProp->m_strDef);
			if (pProp && (pThisProp->m_strId != pProp->m_strId))
			{
				CString str;
				str.Format(IDS_PROP_EXISTS, pThisProp->m_strDef);
				theApp.DoInfoMessage(str);
				return;
			}
		}
	}
	else if (!m_bSought)
	{
		CString str = m_pTempObj->m_strName;
		str.Remove('$');
		
		if (!IsValidLabel(str))	return;

		if (! m_pTempObj->IsValid()) return;

	}

	if (!CheckDialog())	return;

	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();
}


void CRadiusDlg::InitObjectDlg()
{
	CRadius* pRad = (CRadius*)m_pTempObj;
	m_cboBodies.SelectStringExact(pRad->m_strBodies);

}

void CRadiusDlg::InitVariableDlg()
{
	SetWindowText("Variable definition");
	CVariable* pVar = (CVariable*)m_pTempObj;
	m_cboBodies.SelectStringExact(pVar->m_strObject);

}

CLabelRichEdit* CRadiusDlg::GetLabelCtrl()
{
	return &m_editName;
}



void CRadiusDlg::UpdateTempRadius()
{
	m_editName.GetRichEditText(m_pTempObj->m_strName);
	((CRadius*)m_pTempObj)->m_strBodies = GetCurString(&m_cboBodies);
}

void CRadiusDlg::UpdateTempVariable()
{
	m_editName.GetRichEditText(m_pTempObj->m_strName);
	((CVariable*)m_pTempObj)->m_strObject = GetCurString(&m_cboBodies);
	((CVariable*)m_pTempObj)->m_strQuantName = "Radius";

	
	((CVariable*)m_pTempObj)->m_strDef = 
			((CVariable*)m_pTempObj)->m_strQuantName + " of Circular Motion of the " + 
				((CVariable*)m_pTempObj)->m_strObject;


}
