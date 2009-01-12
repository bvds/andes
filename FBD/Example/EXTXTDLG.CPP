// EXTxtDlg.cpp : implementation file
//
// $Id: EXTXTDLG.CPP,v 1.1 2005/01/24 16:28:10 bvds Exp $
#include "stdafx.h"
#include "fbd.h"
#include "EXTxtDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEXTextDlg dialog


CEXTextDlg::CEXTextDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CEXTextDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CEXTextDlg)
	m_strMenu = _T("");
	m_strText = _T("");
	m_bVisible = FALSE;
	m_strId = _T("");
	//}}AFX_DATA_INIT
	// ??? on the chance following gives valid default LOGFONT struct
	memset(&m_logFont, '\0', sizeof(m_logFont));	
}


void CEXTextDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CEXTextDlg)
	DDX_Control(pDX, IDC_EXP_MENU, m_cboMenuList);
	DDX_CBStringExact(pDX, IDC_EXP_MENU, m_strMenu);
	DDX_Text(pDX, IDC_LABEL_TEXT, m_strText);
	DDX_Check(pDX, IDC_VISIBLE, m_bVisible);
	DDX_Text(pDX, IDC_EXTEXT_ID, m_strId);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CEXTextDlg, CDialog)
	//{{AFX_MSG_MAP(CEXTextDlg)
	ON_BN_CLICKED(IDC_CHOOSE_FONT, OnChooseFont)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEXTextDlg message handlers

BOOL CEXTextDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

	// Load combo box choices from explain menu
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_EXPLAIN));
	UINT nItems = menu.GetMenuItemCount();
	
	CString strItem;
	for (UINT nSubMenu = 0; nSubMenu < nItems; nSubMenu++) {
		menu.GetMenuString(nSubMenu, strItem, MF_BYPOSITION);
		m_cboMenuList.AddString(strItem);
	}
		
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CEXTextDlg::OnChooseFont() 
{
	CFontDialog dlg(&m_logFont);	// initialize it??
	dlg.DoModal();
}
