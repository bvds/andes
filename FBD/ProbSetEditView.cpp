// ProbSetEditView.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ProblemSet.h"
#include "ProbSetEditView.h"
#include "PicCtrl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProbSetEditView

IMPLEMENT_DYNCREATE(CProbSetEditView, CFormView)

CProbSetEditView::CProbSetEditView()
	: CFormView(CProbSetEditView::IDD)
{
	//{{AFX_DATA_INIT(CProbSetEditView)
	//}}AFX_DATA_INIT
}

CProbSetEditView::~CProbSetEditView()
{
}

void CProbSetEditView::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CProbSetEditView)
	DDX_Control(pDX, IDC_STC_SELECTED_OPT, m_stcSelected);
	DDX_Control(pDX, IDC_OPT_VALUE, m_cboValue);
	DDX_Control(pDX, IDC_PROBLEM_LIST, m_lstProblems);
	DDX_Control(pDX, IDC_OPTION_LIST, m_lstOptions);
	//}}AFX_DATA_MAP
}

void CProbSetEditView::OnInitialUpdate() 
{
	CFormView::OnInitialUpdate();

	// size frame to view
	GetParentFrame()->RecalcLayout(); // ensures our size is set before resize
    ResizeParentToFit(); 

	// Populate list boxes: first problems list + <default>
	FillProblemList();
	m_lstProblems.SetCurSel(0);	// set intial selection to DEFAULT

	// then options list: will change when problem is selected 
	CProblemSet* pSet = GetDocument();
	FillOptionList(&pSet->m_opts);
}

static const char szDefault[] = "<PROBSET>"; // name for default options for probset

void CProbSetEditView::FillProblemList() 
{
	m_lstProblems.ResetContent();
	
	m_lstProblems.AddString(szDefault);

	CProblemSet* pSet = GetDocument();
	POSITION pos = pSet->m_tasks.GetHeadPosition();
	while (pos != NULL) {
		CTask* pTask = pSet->m_tasks.GetNext(pos);
		m_lstProblems.AddString(pTask->m_strName);
	}
}

// Data for table-driven option handling (for easy extensibility).
// !!! should really go in doc, not view.
// Currently only use BOOL and one ENUM type
enum optType { optBOOL, optSTR, optENUM, optINT };
static const struct OptInfo {
	char* szName;
	optType type;
	// values (as strings) for each of designated modes:
	char*  szDefault;
	char*  szCoached;
	char*  szIntro;
	char*  szSolo;
} Options[] = {
	// Name             Type     Def	Coach	Intro	Solo
	"HelpAvailable",	optBOOL, "1",	"1",	"1",	"0",
	"FbdRequired",		optBOOL, "0",	"0",	"1",	"0",
	"FixedOrder",		optBOOL, "0",	"0",	"1",	"0",
	"CompletionTest",	optBOOL, "1",	"1",	"1",	"0",
"MandatoryCorrection",	optBOOL, "1",	"0",	"1",	"0",
	"VariableEQs",		optBOOL, "0",	"0",	"1",	"0",
	"EquationTool",		optBOOL, "0",	"0",	"1",	"0",
	"PrincipleWindow",	optBOOL, "1",	"0",	"1",	"0",
	"AlgebraWindow",	optBOOL, "0",	"1",	"0",	"0",
	"VariableTips",		optBOOL, "1",	"1",	"1",	"0",
	// default for this?
	"CurriculumStage",	optENUM, "Forces", "Forces", "Forces", "Forces",
};
#define NOPTIONS ARRAY_SIZE(Options)

static const char* StageValues[] ={ 
	"Kinematics", "Newton's Laws", "Forces", 
	"Cons of energy", "Cons of momentum", 
};

static const struct EnumInfo { // info for enum types
	char* szName;
	int   nValues;
	const char** pszValues;
} EnumTbl[] = {
	"CurriculumStage", ARRAY_SIZE(StageValues), StageValues,
};
#define NENUMS ARRAY_SIZE(EnumTbl)

static const OptInfo* FindOption(LPCTSTR szName) {
	for (int i = 0; i < NOPTIONS; i++) {
		if (strcmp(Options[i].szName, szName) == 0) {
			return &Options[i];
		}
	}
	return NULL;
}
static const EnumInfo* GetEnumVals(LPCTSTR szName) {
	for (int i = 0; i < NOPTIONS; i++) {
		if (strcmp(EnumTbl[i].szName, szName) == 0) {
			return &EnumTbl[i];
		}
	}
	return NULL;
}

// codes for where option value may come from: 
#define SET_IN_PROBLEM 0
#define SET_IN_PROBSET 1
#define USE_DEFAULT    2
#define NOT_SET        3	// i.e. error
// pWhere not yet implemented.
CString CProbSetEditView::GetOptionVal(COptionSet* pOpts, CString strName, int *pWhere)
{
	CString strValue;
	if (pOpts->Lookup(strName, strValue)) 
		return strValue;
	else if (pOpts != &GetDocument()->m_opts // this is per-problem set,
			&& GetDocument()->m_opts.Lookup(strName, strValue) ) // set in problem
		return strValue;
	const OptInfo* pInfo = FindOption(strName);
	if (pInfo)
		return pInfo->szDefault;

	return strValue;		
}

void CProbSetEditView::FillOptionList(COptionSet* pOptionSet)
{
	// save current selection (index OK for now).
	int nSel = m_lstOptions.GetCurSel();

	m_lstOptions.ResetContent();
	if (! pOptionSet) return;

	// Show values of all possible options on our list.
	// !!! Won't show any keys in OptionSet not on our list (could happen if change them).
	for (int i = 0; i < NOPTIONS; i++) {
		CString strName = Options[i].szName;
		CString strValue;
		CString strLine;
		if (pOptionSet->Lookup(strName, strValue)) 
			strLine.Format("%s \t= %s", strName, strValue);
		else if (pOptionSet != &GetDocument()->m_opts // this is per-problem set,
				&& GetDocument()->m_opts.Lookup(strName, strValue) ) // set in problem
			strLine.Format("%s \t= PROBSET ( %s )", strName, strValue);// show problem default
		else // show compiled-in default
			strLine.Format("%s \t= DEFAULT [ %s ]", strName, Options[i].szDefault);

		m_lstOptions.AddString(strLine);
	}

	// restore prev selection
	m_lstOptions.SetCurSel(nSel);
}

BEGIN_MESSAGE_MAP(CProbSetEditView, CFormView)
	//{{AFX_MSG_MAP(CProbSetEditView)
	ON_BN_CLICKED(ID_PROBSET_ADD_OPT, OnAddOption)
	ON_BN_CLICKED(ID_PROBSET_ADD_PROB, OnAddProblem)
	ON_LBN_SELCHANGE(IDC_PROBLEM_LIST, OnSelchangeProblemList)
	ON_COMMAND(ID_PROBSET_DELETE_PROB, OnProbsetDeleteProb)
	ON_COMMAND(ID_PROBSET_DELETE_OPTION, OnProbsetDeleteOption)
	ON_LBN_SELCHANGE(IDC_OPTION_LIST, OnSelchangeOptionList)
	ON_BN_CLICKED(IDC_SCAFFOLD_COACHED, OnScaffoldCoached)
	ON_BN_CLICKED(IDC_SCAFFOLD_INTRO, OnScaffoldIntro)
	ON_BN_CLICKED(IDC_SCAFFOLD_SOLO, OnScaffoldSolo)
	ON_BN_CLICKED(ID_PROBSET_MOVEDOWN, OnProbsetMovedown)
	ON_BN_CLICKED(ID_PROBSET_MOVEUP, OnProbsetMoveup)
	ON_UPDATE_COMMAND_UI(ID_PROBSET_MOVEDOWN, OnUpdateMovedown)
	ON_UPDATE_COMMAND_UI(ID_PROBSET_MOVEUP, OnUpdateMoveup)
	//}}AFX_MSG_MAP
	// Participate in idle-time command ui updating
	ON_MESSAGE(WM_IDLEUPDATECMDUI, OnIdleUpdateCmdUI)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProbSetEditView diagnostics

#ifdef _DEBUG
void CProbSetEditView::AssertValid() const
{
	CFormView::AssertValid();
}

void CProbSetEditView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CProblemSet* CProbSetEditView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CProblemSet)));
	return (CProblemSet*)m_pDocument;
}
#endif //_DEBUG


/////////////////////////////////////////////////////////////////////////////
// CProbSetEditView message handlers

// get pointer to the option list for the selected problem, or "default".
COptionSet* CProbSetEditView::GetSelectedOptions()
{
	int nSel = m_lstProblems.GetCurSel();
	if (nSel == LB_ERR) return NULL;

	if (nSel == 0) 
		return &(GetDocument()->m_opts);

	// else get problem's options
	CString strProblemName;
	m_lstProblems.GetText(nSel, strProblemName);
	CTask* pTask = GetDocument()->FindTask(strProblemName);
	if (pTask)
		return &(pTask->m_opts);

	return NULL; // !!! problem's list not found!
}

void CProbSetEditView::OnSelchangeProblemList() 
{
	FillOptionList(GetSelectedOptions());
	
}

// comparison function for sorting file list array with qsort
// args should be *pointers* to CStrings
static int compare(const void *arg1, const void *arg2)
{
   CString* pStr1 = (CString*) arg1;
   CString* pStr2 = (CString*) arg2;

   return (*pStr1).CompareNoCase(*pStr2);
}

void CProbSetEditView::OnAddProblem() 
{
	// Run File Open dlg.
	static const char szFilter[] = "All problem files|*.fbd;*.prb|Workbench files (*.fbd)|*.fbd|Problem files (*.prb)|*.prb| Examples (*.apx)|*.apx||";
	CPicCtrl dlg(TRUE, "fbd", "*.fbd", OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		          szFilter);
	dlg.m_ofn.Flags |= OFN_ENABLESIZING;	  // let them resize [doesn't work?]
	dlg.m_ofn.Flags |= OFN_ALLOWMULTISELECT;  // to easily add multiple problems
	char szFiles[1024] = "";				  // buffer for multiple filename list
	dlg.m_ofn.lpstrFile = szFiles;
	dlg.m_ofn.nMaxFile = 1024;

    // Start the dialog in Problems directory.
	CString strDir = g_strAndesDir + g_szProblemDir;
	dlg.m_ofn.lpstrInitialDir = strDir;
    if (dlg.DoModal() != IDOK)
    	return;
    	
	// extract filenames into an array and sort it alphabetically for adding 
	// (the dialog iterator seems to return names in a funny order)
	// Note what we take is the relevant file title, not full path. (better to get 
	// internal ID?) It will have to be in the ANDES directory when student opens it.
	CStringArray arrFileNames;
	POSITION pos = dlg.GetStartPosition();
	while (pos != NULL) {
		CString strPath = dlg.GetNextPathName(pos);
		// only need filename (with extension)
		int iFileName = strPath.ReverseFind('\\') + 1;
		CString strFileName = strPath.Mid(iFileName);

		arrFileNames.Add(strFileName);
	}

	// sort the array data in place with C Library func
	qsort((void*) arrFileNames.GetData(), arrFileNames.GetSize(), sizeof(CString), 
		   compare);
	
	// now add each one in the array in order
	for (int i = 0; i < arrFileNames.GetSize(); i++) {
		CString strFileName = arrFileNames.GetAt(i);
		// split filename and extension
		int iExt	= strFileName.ReverseFind('.');
		ASSERT(iExt != -1);
		CString strFileTitle = strFileName.Left(iExt);
		CString strExt = strFileName.Mid(iExt);
		// map .prb into .fbd, in case old code requires this
		if (strExt.CompareNoCase(".prb") == 0)
			strExt = ".fbd";

		// update document now
		GetDocument()->AddTask(strFileTitle, strExt);
	}
	
	// (could send UpdateAllViews, but following is simpler)
	FillProblemList();// update display in problem listbox 
}

void CProbSetEditView::OnProbsetDeleteProb() 
{
	int nSel = m_lstProblems.GetCurSel();
	if (nSel == LB_ERR || nSel == 0) return; // can't delete "default"
	
	// else get problem's name
	CString strDeleted;
	m_lstProblems.GetText(nSel, strDeleted);

	// update document
	GetDocument()->RemoveTask(strDeleted);
	// (could send UpdateAllViews, but following is simpler)
	// update display: just remove it from listbox
	m_lstProblems.DeleteString(nSel);
	// should trigger update options now, since selected prob has gone away.
}

void CProbSetEditView::OnAddOption() 
{
	// make sure there's a current option set
	COptionSet* pOpts = GetSelectedOptions();
	if (! pOpts) return;

	// make sure we have two arguments
	CString strName, strValue;
	if (! GetSelectedOption(strName))
		return;

	m_cboValue.GetWindowText(strValue);
	if (strName.IsEmpty() || strValue.IsEmpty()) {
		AfxMessageBox("Missing name or value");
		return;
	}

	// update relevant optset in document
	pOpts->SetAt(strName, strValue);
	GetDocument()->SetModifiedFlag();
	// (could send UpdateAllViews, but following is simpler)
	// update display in option listbox. 
	FillOptionList(pOpts);
}

BOOL CProbSetEditView::GetSelectedOption(CString& strName)
{
	// get selected "name = val" string
	int nSel = m_lstOptions.GetCurSel();
	if (nSel == LB_ERR) return FALSE; 
	CString strNameVal;
	m_lstOptions.GetText(nSel, strNameVal);

	// Split out option key from string
	strName = strNameVal.Left(strNameVal.Find('='));
	strName.TrimRight();
	return TRUE;
}

void CProbSetEditView::OnSelchangeOptionList() 
{
	CString strName;
	if (! GetSelectedOption(strName) ) return;
	
	// Update name arg
	m_stcSelected.SetWindowText(strName);

	// Update value choices combo
	const OptInfo* pInfo = FindOption(strName);
	if (pInfo == NULL) return;
	if (pInfo->type == optBOOL) {
		m_cboValue.ResetContent();
		m_cboValue.AddString("0");
		m_cboValue.AddString("1");
	
	} else if (pInfo->type == optENUM) {
		const EnumInfo* pEnum = GetEnumVals(strName);
		if (pEnum == NULL) return;

		m_cboValue.ResetContent();
		for (int i = 0; i < pEnum->nValues; i++) {
			m_cboValue.AddString(pEnum->pszValues[i]);
		}
	} // interface for other types (string, integer) not implemented

	// Set to current value
	COptionSet* pOpts = GetSelectedOptions();
	if (! pOpts) return;
	CString strValue = GetOptionVal(pOpts, strName);
	m_cboValue.SelectString(-1, strValue);
}

void CProbSetEditView::OnProbsetDeleteOption() 
{
	// make sure there's a current option set
	COptionSet* pOpts = GetSelectedOptions();
	if (! pOpts) return;

	// get selected option name
	CString strName; 
	if (! GetSelectedOption(strName) ) return;

	// remove setting from relevant optset in document
	pOpts->RemoveKey(strName);
	GetDocument()->SetModifiedFlag();
	// (could send UpdateAllViews, but following is simpler)
	// update display in option listbox. 
	FillOptionList(pOpts);
}

// Scaffold-level buttons set all options appropriate to that level
void CProbSetEditView::OnScaffoldCoached() 
{
	COptionSet* pOpts = GetSelectedOptions();
	if (! pOpts) return;

	for (int i = 0; i < NOPTIONS; i++) {
		pOpts->SetAt(Options[i].szName, Options[i].szCoached);
	}
	// (could send UpdateAllViews, but following is simpler)
	// update display in option listbox. 
	FillOptionList(pOpts);
}

void CProbSetEditView::OnScaffoldIntro() 
{
	COptionSet* pOpts = GetSelectedOptions();
	if (! pOpts) return;

	for (int i = 0; i < NOPTIONS; i++) {
		pOpts->SetAt(Options[i].szName, Options[i].szIntro);
	}
	// (could send UpdateAllViews, but following is simpler)
	// update display in option listbox. 
	FillOptionList(pOpts);
}

void CProbSetEditView::OnScaffoldSolo() 
{
	COptionSet* pOpts = GetSelectedOptions();
	if (! pOpts) return;

	for (int i = 0; i < NOPTIONS; i++) {
		pOpts->SetAt(Options[i].szName, Options[i].szSolo);
	}
	// (could send UpdateAllViews, but following is simpler)
	// update display in option listbox. 
	FillOptionList(pOpts);
}

CString CProbSetEditView::GetSelectedProb()
{
	int nSel = m_lstProblems.GetCurSel();
	if (nSel == LB_ERR) return "";

	// else get problem's options
	CString strText;
	m_lstProblems.GetText(nSel, strText);
	return strText;
}

void CProbSetEditView::OnProbsetMovedown() 
{
	CString strProblem = GetSelectedProb();
	CTask* pTask = GetDocument()->FindTask(strProblem);
	if (pTask == NULL) return;

	CTaskList& tasks = GetDocument()->m_tasks;
	POSITION pos = tasks.Find(pTask);
	ASSERT(pos != NULL);
	if (pos != tasks.GetTailPosition())
	{
		POSITION posNext = pos;
		tasks.GetNext(posNext);
		tasks.RemoveAt(pos);
		tasks.InsertAfter(posNext, pTask);

		GetDocument()->SetModifiedFlag();
	}

	// (could send UpdateAllViews, but following is simpler)
	FillProblemList();	// update display in problem listbox 

	// Keep same problem selected  (allows easy multiple moves)
	int nIndex = m_lstProblems.FindStringExact(-1, strProblem);
	if (nIndex >= 0)
		m_lstProblems.SetCurSel(nIndex); 
}

void CProbSetEditView::OnProbsetMoveup() 
{
	CString strProblem = GetSelectedProb();
	CTask* pTask = GetDocument()->FindTask(strProblem);
	if (pTask == NULL) return;

	CTaskList& tasks = GetDocument()->m_tasks;
	POSITION pos = tasks.Find(pTask);
	ASSERT(pos != NULL);
	if (pos != tasks.GetHeadPosition())
	{
		POSITION posPrev = pos;
		tasks.GetPrev(posPrev);
		tasks.RemoveAt(pos);
		tasks.InsertBefore(posPrev, pTask);

		GetDocument()->SetModifiedFlag();
	}
	// (could send UpdateAllViews, but following is simpler)
	FillProblemList();	// update display in problem listbox 
	
	// Keep same problem selected  (allows easy multiple moves)
	int nIndex = m_lstProblems.FindStringExact(-1, strProblem);
	if (nIndex >= 0)
		m_lstProblems.SetCurSel(nIndex); 
}

LRESULT CProbSetEditView::OnIdleUpdateCmdUI(WPARAM wParam, LPARAM lParam)
{
	UpdateDialogControls(this, TRUE);
	return 0L;
}

void CProbSetEditView::OnUpdateMovedown(CCmdUI* pCmdUI)
{
	int nSel = m_lstProblems.GetCurSel();
	int nItems = m_lstProblems.GetCount();
	pCmdUI->Enable(nSel != LB_ERR && nSel != 0 && nSel < nItems - 1);
}

void CProbSetEditView::OnUpdateMoveup(CCmdUI* pCmdUI)
{
	int nSel = m_lstProblems.GetCurSel();
	pCmdUI->Enable(nSel != LB_ERR && nSel > 1);
}


