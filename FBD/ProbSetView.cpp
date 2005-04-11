// ProbSetView.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ProblemSet.h"
#include "ProbSetView.h"
#include "FBDDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProbSetView

IMPLEMENT_DYNCREATE(CProbSetView, CFormView)

CProbSetView::CProbSetView()
	: CFormView(CProbSetView::IDD)
{
	//{{AFX_DATA_INIT(CProbSetView)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT

	m_pTempDoc = NULL;
	m_bInitialized = FALSE;
	m_wDefaultHelpFlags = wAllHelp;
}
	

CProbSetView::~CProbSetView()
{
	// free storage for preview doc.
	if (m_pTempDoc) {
		m_pTempDoc->DeleteContents();
		delete m_pTempDoc;
	}
}

void CProbSetView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CProbSetView)
	DDX_Control(pDX, IDC_PREVIEW_STATIC, m_stcPreview);
	DDX_Control(pDX, IDC_PROBLEM_LIST, m_lstProblems);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CProbSetView, CFormView)
	//{{AFX_MSG_MAP(CProbSetView)
	ON_LBN_SELCHANGE(IDC_PROBLEM_LIST, OnSelchangeProblemList)
	ON_BN_CLICKED(ID_OPEN_PROBLEM, OnOpenProblem)
	ON_BN_CLICKED(ID_DELETE_SOLUTION, OnDeleteSolution)
	ON_UPDATE_COMMAND_UI(ID_OPEN_PROBLEM, OnUpdateOpenProblem)
	ON_UPDATE_COMMAND_UI(ID_DELETE_SOLUTION, OnUpdateDeleteSolution)
	ON_LBN_DBLCLK(IDC_PROBLEM_LIST, OnOpenProblem)
	//}}AFX_MSG_MAP
	
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CProbSetView diagnostics

#ifdef _DEBUG
void CProbSetView::AssertValid() const
{
	CFormView::AssertValid();
}

void CProbSetView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CProblemSet* CProbSetView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CProblemSet)));
	return (CProblemSet*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CProbSetView message handlers

static const char* szStates[] = {
/*workNone*/ "-----",			
/*workPartial*/	"(Partial)",
/*workCompleted*/ "DONE",
/*workUnknown*/	"????",
};
void CProbSetView::OnInitialUpdate() 
{
	CFormView::OnInitialUpdate();
	m_bInitialized = TRUE;
	
	// size frame to view
    ResizeParentToFit(FALSE); // FALSE allows growing; possible it won't all fit.
	
	// Center parent frame within MDI Client area.
	GetParentFrame()->CenterWindow();

	// Set suitable tabstop for our usage -- should be slightly larger than
	// default (32 dlus) to acommodate longer problem names like Exmomr3a
	m_lstProblems.SetTabStops(40);

	OnUpdate(this, 0, NULL); // works fine for initial update

	// if we're auto-opening, post message to ourselves to do it
	if (GetDocument()->m_bOli)
		PostMessage(WM_COMMAND, MAKELONG(ID_OPEN_PROBLEM, 0), BN_CLICKED);
}

void CProbSetView::OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView) 
{
	// Always set focus to list box on activation so arrow-key navigation is available.
 	m_lstProblems.SetFocus();
	
	CFormView::OnActivateView(bActivate, pActivateView, pDeactiveView);
}

void CProbSetView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	if (! m_bInitialized)	// Controls not created yet.
		return;

	// general update: re-synch problem list w/document
	UpdateProblemList();

	// Set selection: 
	// try to restore last selected prob. NB: may have been deleted!
	if (m_strSelectedProb.IsEmpty()
		|| m_lstProblems.SelectString(-1, m_strSelectedProb) == -1)
	{
		// couldn't set to prev. Try first incomplete problem
		CTask* pNextTask = GetDocument()->GetFirstIncompleteTask();
		if (pNextTask == NULL
			|| m_lstProblems.SelectString(-1, pNextTask->m_strName) == -1)
			// else just select first
			m_lstProblems.SetCurSel(0);
		// update for change in selection
		OnSelchangeProblemList();
	}

	// Update command buttons for new state:
	UpdateDialogControls(this, TRUE);
}

void CProbSetView::UpdateProblemList()
{
	m_lstProblems.ResetContent();

	CProblemSet* pSet = GetDocument();
	POSITION pos = pSet->m_tasks.GetHeadPosition();
	while (pos != NULL) {
		CTask* pTask = pSet->m_tasks.GetNext(pos);
		CString strText = pTask->m_strName + '\t' + szStates[pTask->m_work];
		m_lstProblems.AddString(strText);
	}
}

CString CProbSetView::GetSelectedProb()	// returns problem ID; empty string if none
{
	int nSel = m_lstProblems.GetCurSel();
	if (nSel == LB_ERR) return "";

	// else get problem's options
	CString strText;
	m_lstProblems.GetText(nSel, strText);
	return strText.Left(strText.Find('\t'));
}

void CProbSetView::OnSelchangeProblemList() 
{
	// remember selection in member variable:
	m_strSelectedProb = GetSelectedProb();
	if (m_strSelectedProb.IsEmpty()) return;
	
	// no preview if OLI problem set
	if (! GetDocument()->m_bOli) {
		CTask* pTask = GetDocument()->FindTask(m_strSelectedProb);
		if (pTask && pTask->m_strExt.CompareNoCase(".fbd") == 0) // only preview problems, not videos
			SetPreview(m_strSelectedProb);
		else if (pTask && pTask->IsVideo())
			SetPreview("video");			// video.fbd contains generic video preview text
	}
	// We could subscribe to MFC message WM_IDLEUPDATECMDUI to update our
	// command buttons, but this comes much more often then we need, (seems to
	// cause flickering of buttons). We really only need to update when selected 
	// problem changes, i.e here; also if solution state changes for setting
	// the text (Begin Solution vs. Open Solution);
	UpdateDialogControls(this, TRUE);
}

// Get the full path corresponding to the given problem id.
// This will be *either* the custom made .fbd file in the problems directory 
// (for old problems for which we already made .fbd files) OR, if that does not exist, 
// the .prb file in the problems directory for new method
// whereby .fbd problem data is automatically generated from prb files.
CString CProbSetView::GetProblemFilePath(LPCTSTR pszProblemId)
{
	CString strPathName = g_strAndesDir + g_szProblemDir + "\\" + pszProblemId + ".fbd";
/* we do this in OnOpenDocument instead
	CFileStatus statFile;
	if (!CFile::GetStatus(strPathName, statFile)) {
		// no .fbd file exists -- use the prb file path instead.
		strPathName = g_strAndesDir + g_szProblemDir + "\\" + pszProblemId + ".prb";
	}
*/
	return strPathName;
}

void CProbSetView::SetPreview(LPCTSTR pszProblemId)
{
	// empty any existing doc before overwriting m_pDoc
	if (m_pTempDoc) {
		m_pTempDoc->DeleteContents(); // FBDDoc's don't implement so can't re-use them
		delete m_pTempDoc;
	}
	// create new temp doc, flagging it as file preview doc
	// dyncreate from RUNTIME CLASS, since constructor is protected
	m_pTempDoc = (CFBDDoc*)(RUNTIME_CLASS(CFBDDoc))->CreateObject();
	if (! m_pTempDoc) {
		TRACE("ProbSetView: Couldn't create temp doc for preview\n");
		return;
	}
	// flag open reason as preview mode to doc open/close methods
	m_pTempDoc->m_bFilePreview = TRUE;

	// get fullpath from problem id (!!! what about examples)?
	CString strPathName = GetProblemFilePath(pszProblemId);

	// load doc file into tempdoc
	if (!m_pTempDoc->OnOpenDocument(strPathName)) 
		return;		// m_pTempDoc freed later, on update or close of dialog

	// and set preview static to display it
	m_stcPreview.SetDoc(m_pTempDoc);
	m_stcPreview.Invalidate();
}

void CProbSetView::OnOpenProblem() 
{
	CString strProblemId = GetSelectedProb();
	if (strProblemId.IsEmpty()) return;
	CTask* pTask = GetDocument()->FindTask(strProblemId);
	ASSERT(pTask);

	// if it's a video task, launch the demo video
	if (pTask->IsVideo()) {
		ShowVideo(pTask);
		return;
	}
	// else it's a problem task
	
	// See if solution file already exists in standard location:
	CString strSolnPath =pTask->GetSolutionPath();
	CString strOpenPath = strSolnPath;
	CFileStatus statSolution;
	// For OLI version, download what we need, then proceed same as if
	// everything was local.
	if (GetDocument()->m_bOli) 
	{
		// download problem components into local files. This should download
		// solution if it exists.
		GetDocument()->GetProblemFiles(strProblemId);

		// Note!!!: can be weirdness if no OLI solution found, but
		// a local solution exists (maybe saved locally, then crashed before could
		// upload. Maybe should delete or rename local solution before
		// trying to download? Or change Save to do upload in OLI mode?
	}
	
	if (! CFile::GetStatus(strSolnPath, statSolution))  // check for local solution
	{
		// Else starting new solution: get fullpath of problem to open in ANDES dir.
		/* strOpenPath = g_strAndesDir + 
			(pTask->m_strExt == ".apx" ? g_szExampleDir : g_szProblemDir) 
			+ "\\" + strProblemId + pTask->m_strExt; */
		// No longer handle examples !
		strOpenPath = GetProblemFilePath(strProblemId);
	}


	// Set help flags appropriately for new problem
	// TRACE("Opening problem w/ options: "); pTask->DumpOpts(); TRACE("\n");
	CString strValue;
	if (pTask->m_opts.Lookup("TestMode", strValue)
		&& (strValue != "0")) { // any non-zero value sets
			theApp.m_wHelpFlags = wNoHelp;
		}
	else  if (pTask->m_opts.Lookup("FlagMode", strValue)
		&& (strValue != "0")) { // any non-zero value sets
			theApp.m_wHelpFlags = wFlagMode;
		}
	else { 
		// no flag on problem: ensure defaults restored 
		theApp.m_wHelpFlags = theApp.m_wDefaultHelpFlags;
	}


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

void CProbSetView::OnUpdateOpenProblem(CCmdUI* pCmdUI)
{
	CString strProblemId = GetSelectedProb();
	CTask* pTask = GetDocument()->FindTask(strProblemId);
	pCmdUI->Enable(pTask != NULL);
	if (pTask && pTask->IsVideo())
		pCmdUI->SetText("Watch Video");
	else if (pTask && pTask->m_work == workNone) 
		pCmdUI->SetText("Begin Solution");
	else
		pCmdUI->SetText("Open Solution");
}


void CProbSetView::OnDeleteSolution() 
{
	CString strProblemId = GetSelectedProb();
	CTask* pTask = GetDocument()->FindTask(strProblemId);
	if (! pTask) return;

	// destructive op so request confirmation  here
	CString strSolution = pTask->GetSolutionPath();
	CString strMsg;
	strMsg.Format("Delete your solution %s?", strSolution); 
	if (AfxMessageBox(strMsg, MB_YESNO) == IDNO)
		return;

	try {
		CFile::Remove(pTask->GetSolutionPath());

		// can directly update work state and then our display
		pTask->m_work = workNone;

		// no need to reload problem list, just modify selected item in list box
		int nSel = m_lstProblems.GetCurSel();
		CString strNew = pTask->m_strName + '\t' + szStates[pTask->m_work];
		m_lstProblems.DeleteString(nSel);
		m_lstProblems.InsertString(nSel, strNew);
		// keep it selected
		m_lstProblems.SetCurSel(nSel);

		// Update command buttons for new state
		UpdateDialogControls(this, TRUE);
	} 
	catch (CFileException* pEx) {
		TCHAR szCause[512];
		if (! pEx->GetErrorMessage(szCause, 512))
			szCause[0] = '\0';

		CString strMsg;
		strMsg.Format("Couldn't delete %s: %s", pEx->m_strFileName, szCause);
		AfxMessageBox(strMsg);

		pEx->Delete();
	}
}

void CProbSetView::OnUpdateDeleteSolution(CCmdUI* pCmdUI)
{
	CString strProblemId = GetSelectedProb();
	CTask* pTask = GetDocument()->FindTask(strProblemId);
	if (pTask && pTask->IsVideo())
		pCmdUI->Enable(FALSE);
	else
		pCmdUI->Enable(pTask && pTask->m_work != workNone);
}


// launch a video from a problem set task
void CProbSetView::ShowVideo(CTask *pTask)
{
	// Displayed names must be of form kt1a-DEMO or kt1a-VIDEO, with problem name left
	// of first hyphen. Could also put name in task parameter, but this is simpler
	CString strProblem = pTask->m_strName;  //default if no hyphen
	int iHyphen = pTask->m_strName.Find("-");
	if (iHyphen) strProblem = pTask->m_strName.Left(iHyphen);

	// Show the wrapper page, which may be installed locally or on Web.

	// File is html wrapper file. May itself point to web or to 
	theApp.ShowVideoPage(strProblem + ".html");

	// alternative: launch video directly in user's default viewer:
	// CString strFile = g_strAndesDir + "Review/Videos" + strProblem + ".wmv";
	// HINSTANCE hInst = ShellExecute(NULL, "open", strFile, NULL, NULL, SW_SHOWNORMAL); 
	// if (hInst < 32) {
	//   ... failed
	//

	
}

