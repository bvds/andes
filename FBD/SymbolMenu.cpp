// SymbolMenu.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "fbddoc.h"
#include "history.h"
#include "EQView.h"
#include "AngleDlg.h"
#include "SymbolMenu.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSymbolMenu dialog

TBBUTTON tbButtons[] = {
	{0, IDM_ALPHA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{1, IDM_BETA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{2, IDM_GAMMA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{3, IDM_DELTA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{4, IDM_EPSILON, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{5, IDM_ZETA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{6, IDM_ETA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{7, IDM_THETA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{8, IDM_IOTA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{9, IDM_KAPPA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{10, IDM_LAMBDA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{11, IDM_MU, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{12, IDM_NU, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{13, IDM_XI, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{14, IDM_OMICRON, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{15, IDM_PI, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{16, IDM_RHO, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{17, IDM_SIGMA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{18, IDM_TAU, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{19, IDM_UPSILON, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{20, IDM_PHI, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{21, IDM_CHI, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{22, IDM_PSI, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0},
	{23, IDM_OMEGA, TBSTATE_ENABLED, TBSTYLE_BUTTON, 0L, 0}
};

#define ARRAY_SIZE(array_name) (sizeof(array_name)/sizeof(array_name[0]))

const int nButtons ARRAY_SIZE(tbButtons);


CSymbolMenu::CSymbolMenu(CWnd* pParent /*=NULL*/)
	: CDialog(CSymbolMenu::IDD, pParent)
{
	//{{AFX_DATA_INIT(CSymbolMenu)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CSymbolMenu::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSymbolMenu)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CSymbolMenu, CDialog)
	//{{AFX_MSG_MAP(CSymbolMenu)
	//}}AFX_MSG_MAP
	ON_NOTIFY_EX( TTN_NEEDTEXT, 0, OnToolTip )
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSymbolMenu message handlers





BOOL CSymbolMenu::OnInitDialog() 
{
	CDialog::OnInitDialog();
	CRect rect(0, 0, 0, 0);
	if (!m_TBar.Create( WS_CHILD | WS_VISIBLE | CCS_TOP 
			|TBSTYLE_TOOLTIPS | TBSTYLE_WRAPABLE |TBSTYLE_FLAT , 
		rect, this,
	   (UINT)IDR_GREEK))
	   TRACE("Toolbar not created!");
	m_TBar.AddBitmap(24, IDB_GREEK);
	m_TBar.AddButtons(24, tbButtons);
	m_TBar.AutoSize();
	//Change tooltip control style so tooltips always 
	//active even if dialog doesn't have the focus
	CToolTipCtrl* pTipCtrl = m_TBar.GetToolTips();
	HWND hWnd = pTipCtrl->GetSafeHwnd();
	DWORD dwStyle = pTipCtrl->GetStyle();
	dwStyle |= TTS_ALWAYSTIP;
	SetWindowLong(hWnd, GWL_STYLE, dwStyle);
	m_TBar.SetToolTips(pTipCtrl);


	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CSymbolMenu::OnToolTip(UINT id, NMHDR * pTTTStruct, LRESULT * pResult )
{
	TOOLTIPTEXT *pTTT = (TOOLTIPTEXT *)pTTTStruct;  
	UINT nID = pTTTStruct->idFrom;
	int i = 0;
	while (i<nButtons){
		if (tbButtons[i].idCommand == (int)nID){	// req is for one of our cmds
			// copied from MFC's Winfrm.cpp
			TCHAR szFullText[256];
			CString strTipText;;

			// don't handle the message if no string resource found
			if (AfxLoadString(nID, szFullText) == 0)
				return;

			// this is the command id, not the button index
			AfxExtractSubString(strTipText, szFullText, 1, '\n');

			lstrcpy(pTTT->szText, strTipText);
			break;
		}
		i++;
	}
}

void CSymbolMenu::EnableButtons(BOOL bEnable)
{
	for (int id = IDM_GREEKLETTER_FIRST; id <= IDM_GREEKLETTER_LAST; id++)
		m_TBar.EnableButton(id, bEnable);
}





