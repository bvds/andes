// ViewOptsDlg.cpp : implementation file
// 
// $Id: VwOptDlg.cpp,v 1.1 2005/01/24 16:28:09 bvds Exp $

#include "stdafx.h"
#include "fbd.h"
#include "VwOptDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CViewOptsDlg dialog


CViewOptsDlg::CViewOptsDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CViewOptsDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CViewOptsDlg)
	m_nMaskMode = -1;
	m_nGreyLevel = 0;
	m_bHighlightBox = FALSE;
	m_bScaleToFit = FALSE;
	//}}AFX_DATA_INIT
}


void CViewOptsDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CViewOptsDlg)
	DDX_Control(pDX, IDC_SPIN1, m_spinGreyLevel);
	DDX_Radio(pDX, IDC_MASKMODE, m_nMaskMode);
	DDX_Text(pDX, IDC_GREYLEVEL, m_nGreyLevel);
	DDV_MinMaxInt(pDX, m_nGreyLevel, 0, 255);
	DDX_Check(pDX, IDC_HIGHLIGHT_BOX, m_bHighlightBox);
	DDX_Check(pDX, IDC_SCALETOFIT, m_bScaleToFit);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CViewOptsDlg, CDialog)
	//{{AFX_MSG_MAP(CViewOptsDlg)
	ON_BN_CLICKED(IDC_BOX_COLOR, OnBoxColor)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CViewOptsDlg message handlers

BOOL CViewOptsDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// Set range of spinner
	m_spinGreyLevel.SetRange(0, 255);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CViewOptsDlg::OnBoxColor() 
{
	CColorDialog dlg(m_colorBox);
	if (dlg.DoModal() != IDOK)
		return;
	m_colorBox = dlg.GetColor();	
}
