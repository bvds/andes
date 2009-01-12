// ProbDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ProbDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProbDlg dialog


CProbDlg::CProbDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CProbDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CProbDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

BEGIN_CTL_TBL(CProbDlg)
	"name", IDC_CUSTOM_LABEL,
	"event", IDC_EVENT,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
END_CTL_TBL(CProbDlg)

IMPLEMENT_DYNAMIC(CProbDlg, CDrawObjDlg)


void CProbDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CProbDlg)
	DDX_Control(pDX, IDC_STATIC_EQUALS, m_stcEquals);
	DDX_Control(pDX, IDC_GIVEN_BOX, m_stcGiven);
	DDX_Control(pDX, IDC_STATIC_OR, m_stcOr);
	DDX_Control(pDX, IDC_CHECK_UNKNOWN, m_btnUnknown);
	DDX_Control(pDX, IDC_GIVEN_VALUE, m_editValue);
	DDX_Control(pDX, IDOK, m_btnCancel);
	DDX_Control(pDX, IDCANCEL, m_btnOk);
	DDX_Control(pDX, IDC_EVENT, m_editEvent);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_editName);
	//}}AFX_DATA_MAP
	CDrawObjDlg::DoDataExchange(pDX);
}


BEGIN_MESSAGE_MAP(CProbDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CProbDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
	ON_COMMAND_RANGE(IDC_PROB_BTN_FIRST, IDC_PROB_BTN_LAST, OnInsertProbSymbol)
	ON_BN_CLICKED(IDC_CHECK_UNKNOWN, OnCheckUnknown)
	ON_EN_CHANGE(IDC_GIVEN_VALUE, OnChangeGivenValue)
END_MESSAGE_MAP()

// Strings for U and inverted U in Windows Symbol font, using octal escape
static const char* szEmptySet = "\306";     // = "\xC6" = "Æ";
static const char* szIntersection = "\307"; // = "\xC7" = "Ç"
static const char* szUnion = "\310";        // = "\xC8" = "È"



/////////////////////////////////////////////////////////////////////////////
// CProbDlg message handlers
BOOL CProbDlg::OnInitDialog() 
{
	CDrawObjDlg::OnInitDialog();
	
	// create symbol variant of dialog font 
	CFont* pDlgFont = GetFont();
	LOGFONT lf;
    pDlgFont->GetLogFont(&lf);
	strcpy(lf.lfFaceName, "Symbol");        
	VERIFY(m_fontSymbol.CreateFontIndirect(&lf));  // create the font

	// set symbol font on AND and OR buttons
	GetDlgItem(IDC_BTN_AND)->SetFont(&m_fontSymbol);
	GetDlgItem(IDC_BTN_AND)->SetWindowText(szIntersection);
	GetDlgItem(IDC_BTN_OR)->SetFont(&m_fontSymbol);
	GetDlgItem(IDC_BTN_OR)->SetWindowText(szUnion);


	if (m_bSought){
		Remove(IDC_BOX_LABEL);
		SetWindowText("Define Sought");
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CProbDlg::InitVariableDlg()
{
	m_editEvent.SetRichEditText(((CVariable*)m_pTempObj)->m_strObject);
	// Transfer given value/unknown bit from controls to variable
	m_editValue.SetWindowText(((CVariable*)m_pTempObj)->m_strValue);
	OnChangeGivenValue();	// to sync unknown check box with value
}

void CProbDlg::OnOK() 
{
	//Ensure dialog complete
	CString strEvent;
	m_editEvent.GetWindowText(strEvent);
	if (strEvent.IsEmpty())
	{
		theApp.DoWarningMessage("Please specify an event", this);
		return;
	}

	// Update data into temporary object
	UpdateTempVariable();

	if (! m_bSought)
	{
		CString str = m_pTempObj->m_strName;
		str.Remove('$');
		
		if (!IsValidLabel(str))	return;
		
		if (!m_pTempObj->IsValid()) return;
	}

	if (!CheckDialog())	return;
	
	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();
}

void CProbDlg::UpdateTempVariable()
{
	CString strEvent;
	m_editEvent.GetRichEditText(strEvent);
	((CVariable*)m_pTempObj)->m_strObject = strEvent;
	((CVariable*)m_pTempObj)->m_strQuantName = "Probability";
	m_editValue.GetWindowText(((CVariable*)m_pTempObj)->m_strValue);	

	CString str;
	m_editName.GetRichEditText(str);
	m_pTempObj->m_strName = str;
	
	((CVariable*)m_pTempObj)->m_strDef = 
			((CVariable*)m_pTempObj)->m_strQuantName + " of " + 
				((CVariable*)m_pTempObj)->m_strObject;

}

CLabelRichEdit* CProbDlg::GetLabelCtrl()
{
	return &m_editName;
}

void CProbDlg::OnInsertProbSymbol(UINT nID)
{
	// We just get text to insert from button label
	CString strNewSymbol;
	GetDlgItem(nID)->GetWindowText(strNewSymbol);

	// Insert at current cursor location
	if (nID == IDC_BTN_AND || nID == IDC_BTN_OR) {
		m_editEvent.SetCharPlain();
		m_editEvent.ReplaceSel(" ");
		m_editEvent.InsertGreekText(strNewSymbol);
		m_editEvent.SetCharPlain();
		m_editEvent.ReplaceSel(" ");
	}
	else {
		m_editEvent.SetCharPlain();
		m_editEvent.ReplaceSel(strNewSymbol);
	}
}

// convert an event name string in $-tagged format to the format understood by helpsystem.
void CProbDlg::EventNameToHelpFormat(CString& strName)
{
	static const CString strIntersectionTag = CString("$") + szIntersection;
	static const CString strUnionTag = CString("$") + szUnion;

	strName.Replace(strIntersectionTag, "&");
	strName.Replace(strUnionTag, "V");
	strName.Replace("|", "/");
}

// Keep edit control contents and unknown check box in sync:
// blank string <=> unknown checked
void CProbDlg::OnCheckUnknown() 
{
	if (m_btnUnknown.GetCheck()) {
		m_editValue.SetWindowText("");
	} else {
		m_editValue.SetFocus();
	}
}

void CProbDlg::OnChangeGivenValue() 
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
