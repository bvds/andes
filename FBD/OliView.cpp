// OliView.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ProblemSet.h"
#include "OliView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// COliView

IMPLEMENT_DYNCREATE(COliView, CFormView)

COliView::COliView()
	: CFormView(COliView::IDD)
{
	//{{AFX_DATA_INIT(COliView)
	m_strTaskName = _T("");
	//}}AFX_DATA_INIT
	m_bInitialized = FALSE;
}

COliView::~COliView()
{
}

void COliView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(COliView)
	DDX_Control(pDX, IDC_STATUS, m_stcStatus);
	DDX_Text(pDX, IDC_TASK_NAME, m_strTaskName);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(COliView, CFormView)
	//{{AFX_MSG_MAP(COliView)
		ON_BN_CLICKED(ID_OPEN_PROBLEM, OnOpenProblem)
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COliView diagnostics

#ifdef _DEBUG
void COliView::AssertValid() const
{
	CFormView::AssertValid();
}

void COliView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CProblemSet* COliView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CProblemSet)));
	return (CProblemSet*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// COliView message handlers

void COliView::OnInitialUpdate() 
{
	// init m_strTaskName from attached document
	CTask* pTask = GetDocument()->m_tasks.GetHead();
	if (pTask) m_strTaskName = pTask->m_strName;

	CFormView::OnInitialUpdate();	// will do DDX for task name
	m_bInitialized = TRUE;			// flag that now controls exist
	
	// size frame to view
    ResizeParentToFit(FALSE); // FALSE allows resizing; possible it won't all fit.
	
	// Center parent frame within MDI Client area.
	GetParentFrame()->CenterWindow();

	// if we're auto-opening a problem from this set, post message to ourselves to do 
	// the problem open
	if (GetDocument()->m_bOli) {
		// Put message in our queue to actually do it after brief delay, so open
		// starts automatically after we return to framework and it runs the message loop
		// (Need delay for some reasons. Framework will show dialog on first idle point in msg loop.)
		SetTimer(ID_OPEN_PROBLEM, 1000, NULL);
	}
}

void COliView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	if (! m_bInitialized) // windows controls not created yet. 
		return;

	if (lHint == HINT_UPDATE_STATUS_MSG && (pHint != NULL)) 
	{
		LPCTSTR pszText = (LPCTSTR) pHint;

		// Make sure we are visible, e.g. after problem close
		GetParentFrame()->ShowWindow(SW_SHOW);
		ShowWindow(SW_SHOWNORMAL); // have to show this one too?
		m_stcStatus.SetWindowText(pszText);
		UpdateWindow();
	}
}


void COliView::OnOpenProblem() 
{
	// should be only one task in an OLI problem set document
	CTask* pTask = GetDocument()->m_tasks.GetHead();
	ASSERT(pTask);



	// For OLI version, download what we need, then proceed same as if
	// everything was local.
	if (GetDocument()->m_bOli) 
	{
		AfxGetApp()->BeginWaitCursor();
	/* No longer do this since we had errors
		// Fetch the student history file before opening problem
		GetDocument()->GetHistory();
	*/
		// download problem components into local files. This should download
		// solution into standard location if it exists. Otherwise it will
		// delete any local solution to avoid picking up old copies.
		GetDocument()->GetProblemFiles(pTask->m_strName);

		AfxGetApp()->EndWaitCursor();
	}
	
	// See if solution file exists in standard location:
	CString strSolnPath =pTask->GetSolutionPath();
	CString strOpenPath = strSolnPath;
	CFileStatus statSolution;
	if (! CFile::GetStatus(strSolnPath, statSolution))  // doesn't exist
	{
		// Else starting new solution: get fullpath of problem to open in ANDES dir.
		strOpenPath = g_strAndesDir + g_szProblemDir + "\\" + pTask->m_strName + ".fbd";
	}

	// Set help flags appropriately for new problem
	// no flag on problem: ensure defaults restored 
	theApp.m_wHelpFlags = theApp.m_wDefaultHelpFlags;

	// Hide ourselves (the ProblemSet frame) during problem.
	// App will show frame again after problem close (DoTaskSelect).
	GetParentFrame()->ShowWindow(SW_HIDE);

	CDocument* pResult = theApp.OpenDocumentFile(strOpenPath);
	if (pResult != NULL) {
		// success -- change path name to solution path, so obvious in title,
		// and so save turns into save as if it doesn't exist
		pResult->SetPathName(strSolnPath);
		
	} else {// failed to open document
		GetParentFrame()->ShowWindow(SW_SHOW);
	}
}


void COliView::OnTimer(UINT nIDEvent) 
{
	if (nIDEvent == ID_OPEN_PROBLEM) {
		KillTimer(nIDEvent);
		OnOpenProblem();
	}
	
	CFormView::OnTimer(nIDEvent);
}
