// TraceDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "TraceDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTraceDlg dialog


CTraceDlg::CTraceDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CTraceDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CTraceDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CTraceDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTraceDlg)
	DDX_Control(pDX, IDC_RICHEDIT1, m_edit);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CTraceDlg, CDialog)
	//{{AFX_MSG_MAP(CTraceDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTraceDlg message handlers

void CTraceDlg::AddMsg(LPCTSTR pszMsg)
{
	// no-op if window not created
	if (!::IsWindow(m_hWnd)) return;

	// put the cursor at the end   
	LONG nLength = m_edit.GetTextLength();
	m_edit.SetSel(nLength, nLength);

	// add the msg, plus CRLF
	m_edit.ReplaceSel(pszMsg);
	m_edit.ReplaceSel("\r\n");

	// Should scroll it to ensure latest line is visible (nuisance)
}
