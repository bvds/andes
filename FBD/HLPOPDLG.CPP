// HelpOptsDlg.cpp : implementation file
//
// $Id: HLPOPDLG.CPP,v 1.2 2005/04/11 18:53:54 anders Exp $

#include "stdafx.h"
#include "FBD.h"
#include "HlpOpDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHelpOptsDlg dialog


CHelpOptsDlg::CHelpOptsDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CHelpOptsDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CHelpOptsDlg)
	m_nFeedback = -1;
	m_bCheckEquations = FALSE;
	m_bCheckDiagram = FALSE;
	//}}AFX_DATA_INIT
}


void CHelpOptsDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHelpOptsDlg)
	DDX_Radio(pDX, IDC_WAIT_FEEDBACK, m_nFeedback);
	DDX_Check(pDX, IDC_CHECK_EQUATIONS, m_bCheckEquations);
	DDX_Check(pDX, IDC_CHECK_DIAGRAM, m_bCheckDiagram);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CHelpOptsDlg, CDialog)
	//{{AFX_MSG_MAP(CHelpOptsDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHelpOptsDlg message handlers
