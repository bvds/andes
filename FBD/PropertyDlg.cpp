// PropertyDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "PropertyDlg.h"
#include "GridCtrl.h"
#include "HiLevelVw.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPropertyDlg message handlers

////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CPropertyDlg dialog


CPropertyDlg::CPropertyDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CPropertyDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CPropertyDlg)
	//}}AFX_DATA_INIT

}

void CPropertyDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPropertyDlg)
	DDX_Control(pDX, IDC_STATIC_VALUE, m_stcValue);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_TIME, m_cboTime);
	DDX_Control(pDX, IDC_BODY, m_cboObject);
	DDX_Control(pDX, IDC_PREP2, m_stcPrep2);
	DDX_Control(pDX, IDC_PREP1, m_stcPrep1);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_BODY, &m_pDocument->m_strObjects);
	DDX_FillList(pDX, IDC_TIME, &m_pDocument->m_strObjects);
	
	DDX_CBStringExact(pDX, IDC_STATIC_VALUE, ((CVariable*)m_pTempObj)->m_strValue);
	DDX_CBStringExact(pDX, IDC_BODY, ((CVariable*)m_pTempObj)->m_strObject);
	DDX_CBString(pDX, IDC_TIME, ((CVariable*)m_pTempObj)->m_strTime);
	// if bound to object, update it directly.
	if (m_pObj && pDX->m_bSaveAndValidate)
		UpdateProperty();

}

BEGIN_CTL_TBL(CPropertyDlg)
	"property-value", IDC_VALUE,
	"property-object", IDC_BODY,
	"property-time", IDC_TIME,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CPropertyDlg)


BEGIN_MESSAGE_MAP(CPropertyDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CPropertyDlg)
	ON_LBN_SELCHANGE(IDC_BODY, OnSelchangeBody)
	ON_LBN_SELCHANGE(IDC_TIME, OnSelchangeTime)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CPropertyDlg message handlers

BOOL CPropertyDlg::OnInitDialog() 
{
	LogEventf(EV_PROPERTY_DLG, "%s |%s|",m_pObj->m_strId, OBJ_NAME(m_pObj));

	CDrawObjDlg::OnInitDialog();

	if (!m_pTempObj)
		m_pTempObj = new CVariable();

	
	if (((CVariable*)m_pTempObj)->m_nType == ID_ADDPROP_COEFFRICTION){//friction
		m_stcValue.SetWindowText("Coefficient of Friction");
		m_cboTime.ShowWindow(SW_SHOW);
		m_stcPrep1.SetWindowText("between");
		m_stcPrep2.SetWindowText("and");
	}
	else{//gravitational acceleration
		m_stcValue.SetWindowText("Gravitational Acceleration");
		m_cboTime.ShowWindow(SW_HIDE);
		m_stcPrep1.SetWindowText("of");
		m_stcPrep2.SetWindowText("");
	}
	//first make sure our chosen body is in the list (might not be if compound body)
	//and select it
	UpdatePlanStrings(&m_cboObject);

	//if object body property is already filled (i.e. we are editing)
	//and we can't select, it is a compound body which we must create and select
	//override previous selection
	//some redundacy here
	if (!((CVariable*)m_pTempObj)->m_strObject.IsEmpty() 
		&& (m_cboObject.SelectString(-1, ((CVariable*)m_pTempObj)->m_strObject) == CB_ERR) )
	{
		m_cboObject.InsertString(-1, ((CVariable*)m_pTempObj)->m_strObject);
		m_cboObject.SelectString(-1, ((CVariable*)m_pTempObj)->m_strObject);
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CPropertyDlg::OnSelchangeBody() 
{

	BuildProperty();
}

void CPropertyDlg::OnSelchangeTime() 
{
	BuildProperty();
}


void CPropertyDlg::BuildProperty()
{
	m_stcValue.GetWindowText(((CVariable*)m_pTempObj)->m_strValue);
	m_stcPrep1.GetWindowText(m_strPrep1);
	((CVariable*)m_pTempObj)->m_strObject = GetCurString(&m_cboObject);
	m_stcPrep2.GetWindowText(m_strPrep2);
	((CVariable*)m_pTempObj)->m_strTime = GetCurString(&m_cboTime);
	((CVariable*)m_pTempObj)->m_strDef = ((CVariable*)m_pTempObj)->m_strValue + " " +  m_strPrep1 + " " + 
		((CVariable*)m_pTempObj)->m_strObject + " "  + m_strPrep2 + " " + ((CVariable*)m_pTempObj)->m_strTime;
	m_strProp = ((CVariable*)m_pTempObj)->m_strDef;
//	m_stcProperty.SetWindowText(m_strProp);

}



void CPropertyDlg::UpdateProperty()
{
	if (m_pObj == NULL) return;
	((CVariable*)m_pObj)->m_strDef = ((CVariable*)m_pTempObj)->m_strDef;
	((CVariable*)m_pObj)->m_strValue = ((CVariable*)m_pTempObj)->m_strValue;
	((CVariable*)m_pObj)->m_strObject = ((CVariable*)m_pTempObj)->m_strObject;
	((CVariable*)m_pObj)->m_strTime = ((CVariable*)m_pTempObj)->m_strTime;
	((CVariable*)m_pObj)->m_nType = ((CVariable*)m_pTempObj)->m_nType;


}


void CPropertyDlg::OnOK() 
{
	BuildProperty();
	if (IsEmpty(&m_cboObject))//
	{
		theApp.DoWarningMessage("Please select a body", this);
		return;
	}
	if (IsEmpty(&m_cboTime))
	{
		theApp.DoWarningMessage("Please select a time", this);
		return;
	}
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CTableRow)))
	{
		CTableRow* pThisProp = (CTableRow*)m_pTempObj;
		if (pThisProp->m_pTable && pThisProp->m_pTable->GetMatchingProp(pThisProp->m_strDef)){
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

	CDrawObjDlg::OnOK();
}
