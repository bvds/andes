// TabView.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"

#include "PlanView.h"
#include "VarView.h"
#include "HiLevelVw.h"
#include "StageObj.h"
#include "TabView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

///////////////////////////////////////////////////////////////////////
// CTabObj
/////////////////////////////////////////////////////////////////////////////
// Constructor
// 
CTabObject::CTabObject()  : m_pView(NULL),    m_bActive(TRUE),
         m_pRunTimeViewClass(NULL),    m_pszTabTitle(NULL)
{}
///////////////////////////////////////////////////////////////////////////
//  Destructor
//
CTabObject::~CTabObject()
{}
/////////////////////////////////////////////////////////////////////////////
// Accessors

void CTabObject::SetView(CWnd* pView)
{
   m_pView = pView;
}

CWnd* CTabObject::GetView()
{
   return (m_pView);
}

void CTabObject::SetRunTime(CRuntimeClass* pViewClass)
{
   m_pRunTimeViewClass = pViewClass;
}

CRuntimeClass* CTabObject::GetRunTime(void)
{
   return (m_pRunTimeViewClass);
}

void CTabObject::SetID(UINT nID)
{
   m_nID = nID;
}

UINT CTabObject::GetID(void)
{
   return (m_nID);
}

void CTabObject::SetTabTitle(LPCSTR pszTabTitle)
{
   m_pszTabTitle = pszTabTitle;
}

LPCSTR CTabObject::GetTabTitle(void)
{
   return (m_pszTabTitle);
}

/////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
void CTabObject::AssertValid() const
{
   CObject::AssertValid();
}

void CTabObject::Dump(CDumpContext& dc) const
{
   CObject::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CTabObjArr implementation 
// Keeps track of the CTabObjects associated with each
// tab built within CTabFormView
/////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
// Constructor

CTabObjArray::CTabObjArray() :	m_nCurTab(-1L)
{}

/////////////////////////////////////////////////////////////////////////////
// Destructor

CTabObjArray::~CTabObjArray()
{
   //delete all tab objects

   CTabObject * pTabObject;

   int nSize = GetSize();

   for ( int nIndex = 0 ; nIndex < nSize; ++nIndex )
   {
      pTabObject = (CTabObject*) GetAt( nIndex ) ;
      delete pTabObject ;
   }

}

/////////////////////////////////////////////////////////////////////////////
// CTabView
////////////////////////////////////////////////////////////////////////////

IMPLEMENT_DYNCREATE(CTabView, CFormView)

CTabView::CTabView()
	: CFormView(CTabView::IDD)
{
	//{{AFX_DATA_INIT(CTabView)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	m_nTabs = 2;		//number of tabs
	m_pCurView = NULL;  //current view
	m_bInitialized = FALSE;	//Has OnInitialUpdate been done?

	m_nBottomFirstTab = 0;
}

CTabView::~CTabView()
{
  
}

void CTabView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTabView)
	DDX_Control(pDX, IDC_TABVIEW, m_ctrlTab);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CTabView, CFormView)
	//{{AFX_MSG_MAP(CTabView)
	ON_NOTIFY(TCN_SELCHANGE, IDC_TABVIEW, OnSelchangeTabview)
	ON_WM_DESTROY()
	ON_WM_SIZE()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTabView diagnostics

#ifdef _DEBUG
void CTabView::AssertValid() const
{
	CFormView::AssertValid();
}

void CTabView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CFBDDoc* CTabView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return (CFBDDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CTabView message handlers

void CTabView::OnInitialUpdate() 
{
	CFormView::OnInitialUpdate();

	AddTabObj(RUNTIME_CLASS(CVarView), GetDocument(), IDC_TABVIEW, "Variables");
	if (GetDocument()->m_bIncludePlan)
		AddTabObj(RUNTIME_CLASS(CHiLevelVw), GetDocument(), IDC_TABVIEW, "High-Level Solution");
	//size the form so as to remove the scroll bars
	CRect rcClient;
	GetClientRect(&rcClient);
	GetParentFrame()->RecalcLayout();

	SetScaleToFitSize(rcClient.Size());


	TC_ITEM tabCtrlItem;
	tabCtrlItem.mask = TCIF_TEXT;
	//get size of tab object array (# of tabs)
	CTabObject* pTabObj;
	int nSize = m_tabObjects.GetSize();
	ASSERT(nSize != 0);
	//insert tab text
	for (int nIndex = 0; nIndex < nSize; nIndex++)
	{
		pTabObj = (CTabObject*)m_tabObjects[nIndex];
		ASSERT_VALID(pTabObj);
		tabCtrlItem.pszText = (char*) pTabObj->GetTabTitle();
		m_ctrlTab.InsertItem(nIndex, &tabCtrlItem);
	}

	CSize sizeTabItem(0,0);
	CSize sizeResult = m_ctrlTab.SetItemSize(sizeTabItem);

	RECT firstTabPos;
	m_ctrlTab.GetItemRect((0), &firstTabPos);
	m_nBottomFirstTab = firstTabPos.bottom;
	//create the two tabbed views (the varview and the planview)
	for (nIndex = 0; nIndex < nSize; nIndex++)
	{
		if (!CreateTabView(nIndex))
			TRACE("Unable to create tab view\n");
	}
	GetParentFrame()->RecalcLayout();
	//default is to the zero indexed based tab
	SwitchTabView(0);
	m_bInitialized = TRUE;


}



void CTabView::OnSelchangeTabview(NMHDR* pNMHDR, LRESULT* pResult) 
{
	TRACE("Switching tabview\n");
	TRACE("code %d hwndFrom %d idFrom %d\n", pNMHDR->code, pNMHDR->hwndFrom, pNMHDR->idFrom);
	int nSelected = m_ctrlTab.GetCurSel();
	
	SwitchTabView(nSelected);

	if (nSelected == 1)//this is a cheat, want minimized equation for plan view (tab 1)
		theApp.GetChildFrame()->HideEQPane();
	else
		theApp.GetChildFrame()->ShowEQPane();
	
	LogEventf(EV_SWITCH_TAB, "%d %d", nSelected, pNMHDR->idFrom);

	*pResult = 0;
}


CView* CTabView::CreateTabView(int nIndex, BOOL bBorder, BOOL bShow)
{
	CRect viewRect;
	CWnd* pParentWnd = GetDlgItem(IDC_TABVIEW);
	FindSizeOfView(viewRect);

	CTabObject* pTabObj = (CTabObject*)m_tabObjects[nIndex];	


	CView* pView = (CView*)pTabObj->GetRunTime()->CreateObject();
//	ASSERT( pView->IsKindOf( RUNTIME_CLASS( CView ) ) );
	ASSERT (pView != NULL);
	CCreateContext context;
	context.m_pNewViewClass = pTabObj->GetRunTime();
	context.m_pCurrentDoc = GetDocument();
	context.m_pNewDocTemplate = NULL;
	context.m_pLastView = NULL;
	context.m_pCurrentFrame = GetParentFrame();

	if (!pView->Create(NULL, NULL, AFX_WS_DEFAULT_VIEW, viewRect,
		pParentWnd, pTabObj->GetID(), &context))
		TRACE("Unable to create tab view\n");


	pTabObj->SetView(pView);

	if (bShow){
		pView->SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_SHOWWINDOW|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);
		GetParentFrame()->SetActiveView(pView);
	}
	else
		pView->SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_HIDEWINDOW|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);

	return pView;




}

void CTabView::FindSizeOfView(LPRECT viewRect)
{
	CWnd* pParentWnd = GetDlgItem(IDC_TABVIEW);
	pParentWnd->GetClientRect(viewRect);
	viewRect->top += m_nBottomFirstTab + 3;
	viewRect->left += 2;
	viewRect->bottom += -2;
	viewRect->right += -2;
}

void CTabView::SwitchTabView(int nTab)
{
	if ((nTab < 0) || (nTab >= m_nTabs)){
		TRACE("Error in tab logic\n");
		return;
	}
	CTabObject* pTabObj = (CTabObject*)m_tabObjects[nTab];

	if (m_pCurView != pTabObj->GetView())
	{
		if (m_pCurView)
			m_pCurView->ShowWindow(SW_HIDE);
		if (pTabObj->GetView()){
			CView* pView = (CView*)pTabObj->GetView();
			pView->ShowWindow(SW_SHOW);
		}
		m_pCurView = (CView*)pTabObj->GetView();
		m_tabObjects.m_nCurTab = nTab;
		InvalidateRect(NULL);
	}
	GetParentFrame()->SetActiveView(m_pCurView);
			
	
}

BOOL CTabView::AddTabObj(CRuntimeClass* pViewClass,	 CDocument * pDoc, 
						 UINT nID, 	 LPCTSTR pszTabTitle)
{
	CTabObject* pTabObj = new CTabObject;
	m_tabObjects.m_nCurTab++;
	pTabObj->SetRunTime(pViewClass);
	pTabObj->SetID(nID);
	pTabObj->SetTabTitle(pszTabTitle);
	m_tabObjects.Add(pTabObj);
	m_nTabs++;
	return TRUE;

}


void CTabView::OnDestroy() 
{
	CFormView::OnDestroy();
	
	CTabObject * pTabObject;

	int nSize = m_tabObjects.GetSize();
	//must destroy the views that exist in the tab control
	for ( int nIndex = 0 ; nIndex < nSize; ++nIndex )
	{
		pTabObject = (CTabObject*) m_tabObjects.GetAt( nIndex ) ;
		pTabObject->GetView()->DestroyWindow() ;
	}
	
}

void CTabView::ResizeAllViews()
{

	CRect viewRect;
	FindSizeOfView(&viewRect);
	CTabObject* pTabObj;
	for (int nIndex = 0; nIndex < m_tabObjects.GetSize(); ++nIndex){
		pTabObj = (CTabObject*)m_tabObjects[nIndex];
		pTabObj->GetView()->MoveWindow(viewRect);
	}
}


void CTabView::OnSize(UINT nType, int cx, int cy) 
{
	CFormView::OnSize(nType, cx, cy);
	
	if (m_bInitialized){
		CRect rect;
		GetClientRect(&rect);
		m_ctrlTab.MoveWindow(rect);
		ResizeAllViews();
	}
	
}

void CTabView::OnPaint() 
{
	CPaintDC dc(this); // device context for painting

	// I added this override of OnPaint to prevent CView's 
	// OnPaint from being called.  CView's OnPaint called
	// CScrollView's OnPrepareDC which would cause a VERIFY
	// failure in CDC's SetWindowExt when SetWindowExtEx was
	// called and the ScrollView was resized to zero.  This 
	// caused a disruption in Andes in Debug mode.  It seemed 
	// to have no effect in Release mode.
	
	// Do not call CFormView::OnPaint() for painting messages
}

void CTabView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	// This was added to prevent the invalidation of the tabview and 
	// hence an erase background everytime UpdateAllViews was called.
	// Without this, CView::OnUpdate is called with Invalidate(TRUE)
	// The invalidation caused both child views of CTabView (CPlanView
	// and CVarView) to flash.
	
}

BOOL CTabView::DispatchEvent(EventID nEvent, LPCTSTR parms)
{
		
	int idFrom;
	int nNewSel;
	switch(nEvent)
	{
	case EV_SWITCH_TAB:{
		if (sscanf(parms, "%d %d", &nNewSel, &idFrom) == 0)
			return FALSE;

		m_ctrlTab.SetCurSel(nNewSel);

		NMHDR nmhdr;
		nmhdr.hwndFrom = m_ctrlTab.m_hWnd;
		nmhdr.idFrom   = idFrom;
		nmhdr.code     = TCN_SELCHANGE;
		TRACE("code %d hwndFrom %d idFrom %d\n", nmhdr.code, nmhdr.hwndFrom, nmhdr.idFrom);

		SendMessage(WM_NOTIFY, nmhdr.idFrom, (LPARAM)&nmhdr);


		}break;


	}
	return TRUE;
}