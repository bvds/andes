// AuthDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "AuthDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAuthorDlg dialog


CAuthorDlg::CAuthorDlg(CDrawObj* pObj /*=NULL*/, CWnd* pParent /*=NULL*/)
	: CPropertyPage(CAuthorDlg::IDD)
{
	m_pObj = pObj;
	if (m_pObj != NULL) {
		m_strId = m_pObj->m_strId;
		m_bProblemObj = m_pObj->m_flag == TEACHER_OBJECT;
		m_strName = m_pObj->m_strName;

		CFBDDoc* pDoc = ((CFBDDoc*)theApp.GetDocument());
		if (pDoc->m_nProblemType == PROB_EXAMPLE)
		{
			if (m_pObj->m_pEXInfo->m_relations.GetCount() > 0){
				CDrawObj* pObj = m_pObj->m_pEXInfo->m_relations.GetHead();
				m_linkID1 = pObj->m_strId;
			}
			if (m_pObj->m_pEXInfo->m_relations.GetCount() > 1){
				CDrawObj* pObj = m_pObj->m_pEXInfo->m_relations.GetTail();
				m_linkID2 = pObj->m_strId;
			}
		}

		return;
	}
	//{{AFX_DATA_INIT(CAuthorDlg)
	m_strId = _T("");
	m_bProblemObj = FALSE;
	m_strName = _T("");
	m_linkID2 = _T("");
	m_linkID1 = _T("");
	//}}AFX_DATA_INIT
}

void CAuthorDlg::UpdateObj()
{
	if (m_pObj == NULL) return;

	m_pObj->m_strId = m_strId;
	m_pObj->m_flag = m_bProblemObj ? TEACHER_OBJECT : STUDENT_OBJECT;
	m_pObj->m_strName = m_strName;

	CFBDDoc* pDoc = ((CFBDDoc*)theApp.GetDocument());
	if (pDoc->m_nProblemType == PROB_EXAMPLE)
		m_pObj->GetEXInfo()->m_relations.RemoveAll();
	
	if (m_editID1.IsWindowEnabled() && !m_linkID1.IsEmpty()){
		CDrawObj* pObj1 = ((CFBDDoc*)theApp.GetDocument())->Lookup(m_linkID1);
		if (pObj1 != NULL)
			m_pObj->GetEXInfo()->m_relations.AddTail(pObj1);
	}
	if (m_editID2.IsWindowEnabled() && !m_linkID2.IsEmpty()){
		CDrawObj* pObj2 = ((CFBDDoc*)theApp.GetDocument())->Lookup(m_linkID2);
		if (pObj2 != NULL)
			m_pObj->GetEXInfo()->m_relations.AddTail(pObj2);
	}

	m_pObj->Invalidate();
}

void CAuthorDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAuthorDlg)
	DDX_Control(pDX, IDC_LINKID2, m_editID2);
	DDX_Control(pDX, IDC_LINKID1, m_editID1);
	DDX_Text(pDX, IDC_ITEM_ID, m_strId);
	DDX_Check(pDX, IDC_PROBLEM_OBJ, m_bProblemObj);
	DDX_Text(pDX, IDC_LABEL_EDIT, m_strName);
	DDX_Text(pDX, IDC_LINKID2, m_linkID2);
	DDX_Text(pDX, IDC_LINKID1, m_linkID1);
	//}}AFX_DATA_MAP

	if (m_pObj && pDX->m_bSaveAndValidate)
		UpdateObj();
}


BEGIN_MESSAGE_MAP(CAuthorDlg, CPropertyPage)
	//{{AFX_MSG_MAP(CAuthorDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAuthorDlg message handlers

BOOL CAuthorDlg::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	// Load combo box choices from explain menu
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_EXPLAIN));
	UINT nItems = menu.GetMenuItemCount();

	CString strMenu;
	m_pObj->GetTypeName(strMenu);
	
	CString strItem;
	UINT nSubMenu;
	for (nSubMenu = 0; nSubMenu < nItems; nSubMenu++) {
	menu.GetMenuString(nSubMenu, strItem, MF_BYPOSITION);
		if (strItem == strMenu)
			break;
	}
	if (nSubMenu >= nItems) {	// Didn't find it
		TRACE("Didn't find explain menu |%s|\n", strMenu);
		return TRUE;
	}
	// and find the selected string in the appropriate pop-up menu
	CMenu* pPopup = menu.GetSubMenu(nSubMenu);
	int nCount = pPopup->GetMenuItemCount();
	
	CFBDDoc* pDoc = ((CFBDDoc*)theApp.GetDocument());
	m_editID1.EnableWindow((nCount>=1) && (pDoc->m_nProblemType == PROB_EXAMPLE));
	m_editID2.EnableWindow((nCount>=3) && (pDoc->m_nProblemType == PROB_EXAMPLE));

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
