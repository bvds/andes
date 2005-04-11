// LabelDlg.cpp : implementation file
//
// $Id: LABELDLG.CPP,v 1.2 2005/04/11 18:53:54 anders Exp $

#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "FBDDoc.h"
#include "LabelDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLabelDlg dialog


CLabelDlg::CLabelDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CLabelDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CLabelDlg)
	m_strText = _T("");
	m_bMask = FALSE;
	m_bVisible = FALSE;
	//}}AFX_DATA_INIT
	// ??? on the chance following gives valid default LOGFONT struct
	memset(&m_logFont, '\0', sizeof(m_logFont));	
}


void CLabelDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLabelDlg)
	DDX_Control(pDX, IDC_LABEL_TEXT, m_editText);
	DDX_Text(pDX, IDC_LABEL_TEXT, m_strText);
	DDX_Check(pDX, IDC_MASK, m_bMask);
	DDX_Check(pDX, IDC_VISIBLE, m_bVisible);
	//}}AFX_DATA_MAP
	
}


BEGIN_MESSAGE_MAP(CLabelDlg, CDialog)
	//{{AFX_MSG_MAP(CLabelDlg)
	ON_BN_CLICKED(IDC_CHOOSE_FONT, OnChooseFont)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLabelDlg message handlers

BOOL CLabelDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	
	// This just to log entry into this dialog. 
	// Logging full text of every change could get long.
	LogEventf(EV_DLG_TEXT, "");

	// show "custom mask" checkbox when authoring examples only.
	if (theApp.m_bAuthorMode && theApp.GetDocument()->m_nProblemType == PROB_EXAMPLE) 
	{
		GetDlgItem(IDC_MASK)->ShowWindow(SW_SHOW);
	}
	
	// Caller must initialize m_logFont before running dialog
	// !!! Note that the logical font here should specify height in device units, 
	// not logical units (since the choosefont dialog has no DC for mapping). 
	// (On most systems device=logical so we can get away with error here.)
	UpdateEditFont();

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CLabelDlg::OnChooseFont() 
{
	// initializes FontDialog's CHOOSEFONT struct m_cf so as to contain
	// a pointer to our LOGFONT member m_logFont, modified by dialog.
	CFontDialog dlg(&m_logFont);
	if (dlg.DoModal() == IDOK){
			UpdateEditFont();
	}
	
}

// update font in edit control to match the one specified in m_logFont
void CLabelDlg::UpdateEditFont()
{
	m_fontText.DeleteObject();
	m_fontText.CreateFontIndirect(&m_logFont);
	m_editText.SetFont(&m_fontText, TRUE);
}
