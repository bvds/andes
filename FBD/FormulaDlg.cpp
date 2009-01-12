// FormulaDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FormulaDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CFormulaDlg dialog
extern const formString Formulas[] = {

 "Weight of an object near Earth",	"W = m * g",				"grav-force-law",
 "Hooke's Law of Springs",			"F = -k * x",				"hookes-law",
 "Magnitude of Friction",			"f = $m * N",				"friction-force-law",
 "Pythagorean Theorem",				"a^2 + b^2 = c^2",			"pythag-theorem",
};

const int numForms ARRAY_SIZE(Formulas);

CFormulaDlg::CFormulaDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CFormulaDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CFormulaDlg)
	//}}AFX_DATA_INIT
}


void CFormulaDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CFormulaDlg)
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_FORMULA, m_listFormulaName);
	//}}AFX_DATA_MAP
	DDX_Control(pDX, IDC_DEF, m_editFormula);
}


BEGIN_MESSAGE_MAP(CFormulaDlg, CLogDialog)
	//{{AFX_MSG_MAP(CFormulaDlg)
	ON_LBN_SELCHANGE(IDC_FORMULA, OnSelchangeFormula)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CFormulaDlg message handlers

BOOL CFormulaDlg::OnInitDialog() 
{
	LogEventf(EV_FORMULA_DLG, "");

	CLogDialog::OnInitDialog();
	for (int i = 0;  i < numForms; ++i) 
		m_listFormulaName.AddString(Formulas[i].strName);//make sure in same order as in CExpDlg.h
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CFormulaDlg::OnSelchangeFormula() 
{
	CString eqStr = Formulas[m_listFormulaName.GetCurSel()].strForm;
	m_editFormula.SetRichEditText(eqStr);

}

void CFormulaDlg::OnOK() 
{
	m_editFormula.GetRichEditText(m_strFormula);
	
	CLogDialog::OnOK();
}
