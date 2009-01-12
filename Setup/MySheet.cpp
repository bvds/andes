// MySheet.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"

#include "ExitDlg.h"
#include "MySheet.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMySheet

IMPLEMENT_DYNAMIC(CMySheet, CPropertySheet)

CMySheet::CMySheet(UINT nIDCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(nIDCaption, pParentWnd, iSelectPage)
{
}

CMySheet::CMySheet(LPCTSTR pszCaption, CWnd* pParentWnd, UINT iSelectPage)
	:CPropertySheet(pszCaption, pParentWnd, iSelectPage)
{
}

CMySheet::~CMySheet()
{
}


BEGIN_MESSAGE_MAP(CMySheet, CPropertySheet)
	//{{AFX_MSG_MAP(CMySheet)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDCANCEL, OnCancel)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMySheet message handlers
void CMySheet::OnCancel()
{
	//pops up an exit dialog to make sure user wants to quit
	CExitDlg exDlg;
	if (exDlg.DoModal() == IDOK)
		return;
	
	CPropertySheet::EndDialog(IDCANCEL);
}

