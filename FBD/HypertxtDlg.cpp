// HypertxtDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "HypertxtDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHypertxtDlg dialog


CHypertxtDlg::CHypertxtDlg(CHyperLnk* pLnk, CWnd* pParent /*=NULL*/)
	: CDialog(CHypertxtDlg::IDD, pParent)
{
	memset(&m_logFont, '\0', sizeof(m_logFont));	
	m_pLnk = pLnk;
	if (m_pLnk) // init dialog data members from object props (string-valued)
	{
		pLnk->m_font.GetLogFont(&m_logFont);
		m_nHyperType = pLnk->m_nType;
		if (m_nHyperType == ID_POPUP)
			m_strDef = pLnk->m_strLink;
		else 
			m_strLink =	pLnk->m_strLink;
		m_strText = pLnk->m_strName;
		return;
	}
	//{{AFX_DATA_INIT(CHypertxtDlg)
	m_nHyperType = -1;
	m_strText = _T("");
	m_strDef = _T("");
	m_strLink = _T("");
	//}}AFX_DATA_INIT

}


void CHypertxtDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHypertxtDlg)
	DDX_Control(pDX, IDC_LINKBTN, m_btnHyperType);
	DDX_Control(pDX, IDC_LINKTEXT, m_editLinkTxt);
	DDX_Control(pDX, IDC_HYPERTEXT, m_editHyperTxt);
	DDX_Control(pDX, IDC_DEFTEXT, m_editDefTxt);
	DDX_Radio(pDX, IDC_LINKBTN, m_nHyperType);
	DDX_Text(pDX, IDC_HYPERTEXT, m_strText);
	DDX_Text(pDX, IDC_DEFTEXT, m_strDef);
	DDX_Text(pDX, IDC_LINKTEXT, m_strLink);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CHypertxtDlg, CDialog)
	//{{AFX_MSG_MAP(CHypertxtDlg)
	ON_BN_CLICKED(IDC_LINKBTN, OnLinkbtn)
	ON_BN_CLICKED(IDC_HELPCALLBTN, OnLinkbtn)
	ON_BN_CLICKED(IDC_DEFBTN, OnDefbtn)
	ON_BN_CLICKED(IDC_CHOOSE_FONT, OnChooseFont)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHypertxtDlg message handlers

void CHypertxtDlg::OnLinkbtn() // also mapped to helpcall btn (link field specifies call)
{
	m_editDefTxt.EnableWindow(FALSE);
	m_editLinkTxt.EnableWindow(TRUE);
	m_editLinkTxt.SetFocus();
	
}

void CHypertxtDlg::OnDefbtn() 
{
	m_editDefTxt.EnableWindow(TRUE);
	m_editDefTxt.SetFocus();
	m_editLinkTxt.EnableWindow(FALSE);
	
}

BOOL CHypertxtDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	if (m_nHyperType >= 0) {
		m_editDefTxt.EnableWindow(m_nHyperType == ID_POPUP);
		m_editLinkTxt.EnableWindow(m_nHyperType != ID_POPUP);
	}
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CHypertxtDlg::OnChooseFont() 
{
	CFontDialog dlg(&m_logFont);
	// initializes FontDialog's CHOOSEFONT struct m_cf so as to contain
	// a pointer to our LOGFONT member.
	if (dlg.DoModal() == IDOK){
			// Change font and invalidate document.
		m_fontText.DeleteObject();
		m_fontText.CreateFontIndirect(&m_logFont);
		m_editHyperTxt.SetFont(&m_fontText, TRUE);
	}
	
}
