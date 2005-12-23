// FBDDoc.cpp : implementation of the CFBDDoc class
//
// 
    
#include "stdafx.h"
#include <math.h>
    
#include "FBD.h"
#include "history.h"
#include "MainFrm.h"
#include "FBDDoc.h"
#include "FBDObj.h"		// CAxes used in IsAxesDrawn
#include "FBDView.h"
#include "EQView.h"
#include "EXView.h"
#include "EXPlanVw.h"
//#include "HiLevelVw.h"
#include "HelpIfc.h"
#include "DocPages.h"
#include "ChildFrm.h"
#include "VariableDlg.h"
#include "TaskDlg.h"
#include "HintDlg.h"
#include "EXHintDg.h"
#include "HelpFilesPage.h"
#include "ProblemSet.h"
#include "LispReader.h"
    
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
    static char THIS_FILE[] = __FILE__;
#endif
  

/////////////////////////////////////////////////////////////////////////////
// CFBDDoc
    
IMPLEMENT_DYNCREATE(CFBDDoc, COleDocument)
    
BEGIN_MESSAGE_MAP(CFBDDoc, COleDocument)
   	//{{AFX_MSG_MAP(CFBDDoc)
	ON_COMMAND(ID_FILE_SAVE_AS, OnFileSaveAs)
	ON_UPDATE_COMMAND_UI(ID_FILE_SAVE_AS, OnUpdateFileSaveAs)
	//}}AFX_MSG_MAP
   	ON_UPDATE_COMMAND_UI(ID_OLE_VERB_FIRST, COleDocument::OnUpdateObjectVerbMenu)
END_MESSAGE_MAP()
    
/////////////////////////////////////////////////////////////////////////////
// CFBDDoc construction/destruction
    
#define DOC_VERSION 24		//latest document format version number
    
CFBDDoc::CFBDDoc()
{
   	// Parameters:
   	m_nProblemType = PROB_QUANT;
   	m_nVersion = DOC_VERSION;
	m_nKBType = KB_CLIPS;			// new problems get newer KB as initial default.
	m_nAssessor = ASSESSOR_EXACT;	// assume can use exact algorithm unless told 
	m_bIncludePlan = FALSE;
	// m_bSolution = FALSE;			// starts out as master problem.
	m_wConcept = (ID_PROB_KINEMATICS | ID_PROB_FORCE);
    
   	// Drawing objects:  list empty by default
   	nNextID = 1;	// init doc's id counter

	//Stage Objects:
	nNextStageID = 0; // initialize doc's id counter
    
   	// Equation arrays: each string empty by default.
   	// init equation statuses 
   	for (int i = 0; i < NEQS; i++)
   		m_statusEq[i] = statusUnknown;
   
   	// Per-problem Student statistics: Note not persistent across sessions.
   	m_nStartTime = 0;
   	m_nHelpReqs = 0;
    
	// File open reason flags:
	m_bFilePreview = FALSE;
	m_bPrintOnly = FALSE;
	m_bDemoProblem = FALSE;
   	
	m_bPlanWnd = FALSE;

	// solution state
	m_workState = workNone;

	m_bSaveAsCmd = FALSE;
}
    
CFBDDoc::~CFBDDoc()
{
   	// Empty drawing object list and free each. 
   	while (!m_objects.IsEmpty())
   		m_objects.RemoveHead()->Delete();
   
   	// Empty variable list and free each
   	while (!m_Variables.IsEmpty()){
   		CVariable* pVar=m_Variables.RemoveHead();
   		delete pVar;
   	}
	if (!m_bPlanWnd)//plan window deletes own list
		DeletePlanItems();

	//we no longer need to do this.  The principles object removes and
	//deletes its associated stage
	// Empty stage list and free each
//	while (!m_stages.IsEmpty()){
//		CStageObj* pStage = m_stages.RemoveHead();
//		delete pStage;
//	}

	// Empty principles list and free each
	while (!m_principles.IsEmpty()){
		CPrincObj* pPrinc = m_principles.RemoveHead();
		delete pPrinc;
	}
}
    
/////////////////////////////////////////////////////////////////////////////
// CFBDDoc serialization
//
// Note the framework calls the document serialization function directly to
// store document data into archive. That means it does *not* prefix
// any header information to the file, in particular it does not put the
// class-descriptor info that is automatically prefixed when storing serializable 
// objects to an archive through the >> operator for the class. The upshot is that 
// the "VERSIONABLE_SCHEMA" tag is of no use at the document level since the
// framework doesn't put out the class version info for the document. However we are
// free to put put and get header data, including version info, in any format we
// wish in this routine. (See Technical Note on Serialization.)
///////////////////////////////////////////////////////////////////////////// 
void CFBDDoc::Serialize(CArchive& ar)
{
	// First use common code to save/load file "header" info (includes work state).
	// This is broken out since problem set display only needs this from disk.
	SerializeHeader(ar);

   	if (ar.IsStoring())
   	{
#ifdef _DEBUG // sometimes useful when debugging:
		// TRACE("Writing Document:\n");
		// Dump(afxDump); // dump whole document's data on loading.
#endif _DEBUG	
	
   		// Serialize problem parameters
   		ar << m_bIncludePlan;			// added version 8
		ar << m_nKBType;				// added version 10
		ar << m_nAssessor;				// added version 18
		ar << m_wConcept;				// added version 19
		
		// added version 22: feature string & source info for
		// autolayout (in case author modifies problem master).
		//   version 23: changed strFeatures to CStringList
		// ar << m_strFeatures;		// version 22 code
		m_strFeatures.Serialize(ar);
		ar << m_strStatement;				
		ar << m_strGraphicFile;	
		
		// item lists
   		m_strObjects.Serialize(ar);
   		m_strTimes.Serialize(ar);
   		// ar << m_strEarth;			// taken out version 22
    	m_strPositions.Serialize(ar);	// added version 13
		m_strBranches.Serialize(ar);	// added version 21

   		//  Serialize drawing objects
		
		// update answer boxes from controls -- needed for correct Greek until bug found.
		//CFBDView * pFBDView = theApp.GetFBDView();
   		//if (pFBDView != NULL) pFBDView->UpdateDoc();
    
   		m_objects.Serialize(ar);
   		// Added version 3: store ID generation counter
   		if (m_nVersion >= 3)	
   			ar << (LONG) nNextID;
    
   		//  Serialize equations and their status:
   
   		// First find EQ view and update strings from controls
   		CEQView * pEQView = theApp.GetEQView();
   		if (pEQView != NULL)
   			pEQView->UpdateDoc();
   		// Just ignore if didn't find an EQView, strings should be empty
    			
   		// put status values array
   		for (int i = 0; i < NEQS; ++i) { 
   			ar << (WORD) m_statusEq[i];
   		}
   		// put string array
   		for (i = 0; i < NEQS; ++i) {
   			ar <<  m_strEq[i];
   		}
   
   		// Serialize plan:
   
   		//Find first plan view and update itemList from TreeControl
   		CPlanView* pPlanView = (CPlanView*)theApp.FindView(RUNTIME_CLASS(CPlanView));
		if (pPlanView != NULL)
   			pPlanView->UpdateDoc();//new planobj and plan items created
   		
    	m_plan.Serialize(ar);

		// Added version 5: now an OleDoc, let base class store its embedded item list
		if (m_nVersion >= 5) {
			COleDocument::Serialize(ar);
		}
		m_Variables.Serialize(ar);
   		m_strVarNames.Serialize(ar);
		m_principles.Serialize(ar);
		ar << (LONG) nNextStageID;
		m_stages.Serialize(ar);
	
	}
	else // Loading
	{
		CString strTemp;
		// Problem parameters
		// Added in version 8:  BOOLEAN should we include the High-level Solution Window
    	if (m_nVersion >= 8)
			ar >> m_bIncludePlan;
		else
			m_bIncludePlan = FALSE;
		
		if (m_nVersion >= 10)		// Added in version 10: KBType
			ar >> m_nKBType;
		else
			m_nKBType = KB_ATMS;	// older problems used older ATMS knowledge base

		if (m_nVersion >= 18)		// added v18: assessor flags
			ar >> m_nAssessor;		// else stick with default from constructor

		if (m_nVersion >= 19)
			ar >> m_wConcept;
		
		if (m_nVersion >= 22)		// add v22: feature list, statement, graphic
		{
			if (m_nVersion >= 23)	// changee to string list
				m_strFeatures.Serialize(ar);
			else // consume v22 value (was unused, just empty string)
				ar >> strTemp; 
			ar >> m_strStatement;				
			ar >> m_strGraphicFile;
		}
		if (m_strFeatures.IsEmpty()) 
			AddConceptsToFeatures();	// init from m_wConcept
		
		if (m_nVersion < 17)	// old version stored in delimited strings
		{
			ar >> strTemp;
			SplitStr(strTemp, m_strObjects);
			ar >> strTemp;
			SplitStr(strTemp, m_strTimes, "\n\r");
		}
		else // >= 17: stored as stringlists
		{
			m_strObjects.Serialize(ar);
			m_strTimes.Serialize(ar);
		}
		// New practice: ensure always include at least one default time spec
	//	if (m_strTimes.IsEmpty())
	//		m_strTimes.AddHead("T0 = the instant depicted");
	
		// ar >> m_strEarth			// taken out AT version 22
		if (m_nVersion < 22)		// consume item in earlier versions
			ar >> strTemp;	
	
		if (m_nVersion >= 13)		// Added in version 13: positions of bodies at time
		{
			if (m_nVersion < 17) {	// stored in delimited string
				ar >> strTemp;
				SplitStr(strTemp, m_strPositions, "\n\r");
			} else // stored as CStringList
				m_strPositions.Serialize(ar);
		}

		if (m_nVersion >= 21)		// added v21: branches for circuit problems
			m_strBranches.Serialize(ar);
    
		// load list of drawing objects
		TRACE("CFBDDoc::Serialize Loading drawing objects\n");
		m_objects.Serialize(ar);
		// Added version 3: load unique ID generation counter
		if (m_nVersion >= 3) 
			ar >> (LONG&) nNextID;
    
		// load statuses and equations
		TRACE("Loading equations\n");
		try {
			for (int i = 0; i < NEQS; ++i) {
				WORD wTemp;
				ar >> wTemp;  m_statusEq[i] = (Status) wTemp;
			}
			for (i = 0; i < NEQS; ++i) {
				ar >> m_strEq[i];
			}
		} 
		catch (CArchiveException* e)
		{
			// try to recover from end of file exception -- will get it here 
			// for earlier format without equation pane state in document
			if (e->m_cause == CArchiveException::endOfFile) {
				theApp.DoWarningMessage("Unexpected end of file while trying to read equation information.\nAssuming old file format.\nSave document to convert to newer format.");
				e->Delete();
			} else
				throw;
		}
    		
		TRACE("Loading Plan object\n");
		m_plan.Serialize(ar);
    
		// Added version 5: now COleDocument so let base load list of OLE items
		if (m_nVersion >= 5) {
			TRACE("Loading OLE embedded items\n");
			COleDocument::Serialize(ar);
		}
    
		if (m_nVersion >=7){
			TRACE("Loading Variables\n");
			m_Variables.Serialize(ar);
		}
		if (m_nVersion >= 15)
		{
			TRACE("initializing variable strings\n");
			m_strVarNames.Serialize(ar);
		}

		if (m_nVersion >= 14)
		{
			TRACE("Loading principles\n");
			m_principles.Serialize(ar);
		}

			
		if (m_nVersion >= 12)
			ar >> (LONG&) nNextStageID;
		
		if (m_nVersion >= 11){		// Added in version 11: Hilevel soln window
			TRACE("Loading stages for hi-level plan\n");
			m_stages.Serialize(ar);
		}

		// After doc is all loaded: Update doc's version field to the latest one,
		// so it will be used in any subsequent storing of these objects. 
    	m_nVersion = DOC_VERSION;

		// Calc workState if it wasn't saved in doc.
		if (m_workState == workUnknown)
			UpdateWorkState();

#ifdef _DEBUG // sometimes useful when debugging:
		Dump(afxDump); // dump whole document's data on loading.
#endif _DEBUG	
	} // end Loading document data
}// end Serialize

// Read/Write document file header from start of an archive.
// Broken out of main serialzation code to allow use by itself for quickly 
// peeking at header alone to get info about files on disk (mainly workstate).
void CFBDDoc::SerializeHeader(CArchive &ar)
{
	if (ar.IsStoring()) {
   		// ensure writing object using latest file version:
   		m_nVersion = DOC_VERSION;
   
   		// Header contains problem id, type/version, and workstate
		// Our first version documents did not serialize a version number.
   		// For backwards compatibility we now pack the version number into
   		// the unused high word of the 32-bit problem type field when storing, 
   		// and unpack when loading. Earlier documents thus wind up with a version
   		// number of zero
   		ar << m_strProblemId;
   		ar << (LONG) (MAKELONG(m_nProblemType, m_nVersion));
		// ar << m_bSolution;			// Added v16: Flag if solution
		UpdateWorkState();			// make sure up to date before saving
		ar << (WORD) m_workState;	// Added v16: workstate (at top for easy access)

		// Added version 20: stamp file with creator, to inhibit other students from
		// copying saved solutions. This should be set on first save of a solution in 
		// saving management code. We do that so don't set it on problem masters.
		ar << m_strCreator;

		// Added version 24: saved score statistics string. Make sure up to date
		// NB: if saving in author mode, assume this is problem master so save
		// blank stats
		if (theApp.m_bAuthorMode) 
			ar << CString("");
		else {
			UpdateSavedStats();
			ar << m_strSavedStats;
		}
	} else {
		ar >> m_strProblemId;
		// Unpack stored version number and problem type:
		// m_nVersion is set to the external file format during the load and
		// updated to the latest version when done, to affect subsequent saves
		LONG lTemp;
		ar >> lTemp;
		m_nProblemType = LOWORD(lTemp);
		m_nVersion = HIWORD(lTemp);
		// Added version 16: solution file flag
		// if (m_nVersion >= 16)
		//		ar >> m_bSolution;
		// else
		//		// unknown, clear if file in ANDES Problem directory or has entries
		// Added version 16: workstate (at top for easy access)
		if (m_nVersion >= 16)
			ar >> (WORD&) m_workState;
		else
			m_workState = workUnknown;	// update it from contents after load
		
		// Added version 20: id of creator for solution files.
		if (m_nVersion >= 20)
			ar >> m_strCreator;
		
		// added version 24:
		if (m_nVersion >= 24)
			ar >> m_strSavedStats;
	}
}

// Read work state from file on disk. 
// Returns workState recorded in header on last save.
// if doesn't exist => workNone (non-existent solution has no work).
// return of workUnkown means does exist but can't determine (error or old file).
WorkState CFBDDoc::LoadWorkState(LPCTSTR pszPathName)
{
	// make sure it exists first.
	CFileStatus statFile;
	if (!CFile::GetStatus(pszPathName, statFile))
		return workNone;
	 
	// use temp doc to do the serialization
	CFBDDoc* pDoc = new CFBDDoc();
	pDoc->m_bAutoDelete = FALSE;
	BOOL bLoadedOK = TRUE;	// until failure

	// Following based on CDocument::OnOpenDocument.
	// open file and make an archive for this filename;
	CFileException fe;
	CFile* pFile = pDoc->GetFile(pszPathName, CFile::modeRead|CFile::shareDenyWrite, &fe);
	if (pFile == NULL) {
		// ReportSaveLoadException(lpszPathName, &fe, FALSE, AFX_IDP_FAILED_TO_OPEN_DOC);
		return workUnknown;
	}
	CArchive loadArchive(pFile, CArchive::load | CArchive::bNoFlushOnDelete);
	loadArchive.m_pDocument = pDoc;
	loadArchive.m_bForceFlat = FALSE; //don't flatten OLE item data in compound files
	TRY {
		// CWaitCursor wait;
		if (pFile->GetLength() != 0)
			pDoc->SerializeHeader(loadArchive);   // only need to load header info
		loadArchive.Close();
		pDoc->ReleaseFile(pFile, FALSE);
	}
	CATCH_ALL(e) {
		pDoc->ReleaseFile(pFile, TRUE);
		pDoc->DeleteContents();   // remove failed contents // ?? no-op for our docs
		// No error messages
		// TRY {
		//		ReportSaveLoadException(lpszPathName, e, FALSE, AFX_IDP_FAILED_TO_OPEN_DOC);
		// }
		// END_TRY
		// DELETE_EXCEPTION(e); // only for *inside* MFC, not client code using MFC macros
		bLoadedOK = FALSE;
	}
	END_CATCH_ALL

	WorkState workState;	// save return value over temp document delete.
	if (bLoadedOK)
		workState = pDoc->m_workState;
	else
		workState = workUnknown;
	// !!!if loaded but workState unknown (old file header), could load whole doc to calc.
	
	// Free the temp doc.
	// Following is necessary to stop MFC's automatic closing of App from COleDoc
	// destructor if OLE object count goes to zero and app has not yet been marked
	// under user control in MFC state. (Happened on shell DDE open of problem set, 
	// when problem set view is looking up the solution states, before Open is complete
	// and main window shown, so that UserControl flag is not set yet by framework.)
	BOOL bUserCtrl = AfxOleGetUserCtrl();
	AfxOleSetUserCtrl(TRUE);
	delete pDoc;
	AfxOleSetUserCtrl(bUserCtrl);

	return workState;
}

// For simplicity of the authoring interface, original document format stored 
// some lists of ids in single delimited strings. Following utility is used to
// unpack old format into the newer CStringList representation on loading
// (though it may be generally useful).
//
// Split delimited list of tokens out of a single CString into StringList
// Sequences from delimiter set are effectively coalesced, so no empty tokens returned
void SplitStr(CString& str, CStringList& list, LPCTSTR seps /*=" \t\n\r,"*/)
{
	// Copy into temp buffer
	char* buf = (char*) malloc(strlen(str) + 1);	
	strcpy(buf, str);

	// Non re-entrant strtok tokenizes in place by inserting NULs into buf
	char *token = strtok( buf, seps ); 
	while( token != NULL )
	{
		list.AddTail(token);
		token = strtok( NULL, seps );	//	returns ptr to next token in buf
	}

	free((void*)buf);
}

// same as above, but splits into a CStringArray
void SplitStr(CString& str, CStringArray& strs, LPCTSTR seps /*=" \t\n\r,"*/)
{
	// Copy into temp buffer
	char* buf = (char*) malloc(strlen(str) + 1);	
	strcpy(buf, str);

	// Non re-entrant strtok tokenizes in place by inserting NULs into buf
	int i = 0;
	char *token = strtok( buf, seps ); 
	while( token != NULL )
	{
		strs[i++] = token;
		token = strtok( NULL, seps );	//	returns ptr to next token in buf
	}

	free((void*)buf);
}

CStringList* CFBDDoc::GetChoiceList(const CString& strName)
{
	// for now, just have fixed set of choices. Later will make extensible.
	if (strName.CompareNoCase("bodies") == 0)
		return &m_strObjects;
	else if (strName.CompareNoCase("positions") == 0)
		return &m_strPositions;
	else if (strName.CompareNoCase("times") == 0)
		return &m_strTimes;
	else if (strName.CompareNoCase("branches") == 0)
		return &m_strBranches;

	return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////
//
// Processing of Open and Close Problem events on behalf of the application:
//
////////////////////////////////////////////////////////////////////////////////////////
    
/////////////////////////////////////////////////////////////////////////////
// CFBDDocTemplate -- special doc template to customize framework's default launch
// sequence to load document before creating frame and views.
    
IMPLEMENT_DYNAMIC(CFBDDocTemplate, CMultiDocTemplate)
    
BEGIN_MESSAGE_MAP(CFBDDocTemplate, CMultiDocTemplate)
	//{{AFX_MSG_MAP(CFBDDocTemplate)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
    
// "pass-through" constructor: just to pass parms up to base class ctor
CFBDDocTemplate::CFBDDocTemplate(UINT nIDResource, CRuntimeClass* pDocClass,
								CRuntimeClass* pFrameClass, CRuntimeClass* pViewClass)
	: CMultiDocTemplate(nIDResource, pDocClass, pFrameClass, pViewClass)
{}
    
//
// Template's OpenDocumentFile does the work of setting up doc/frame/view set,
// for existing or new documents (NULL pathname => create new). This code 
// copied from the MFC default so that we can re-order the process. The default
// constructs a new empty document object, then uses CreateNewFrame to construct a
// Framewindow. That in turn calls the frame's OnCreateClient to create the views in its
// client area. Then it serializes the document's data into the document.
// 
// That was a problem for us because we want to set up the splitter pane views based
// on data in the document (problem type), but the splitter must be set up before it
// is read. Rather then create a different document type for each possible setup, we 
// use this subclass to override the default and re-order the code to read in the 
// document before creating frame and view. Will see if there are any problems with this.
// **Note: in MFC default order, doc cleanup/removal from template's list on failure to 
// load (e.g due to serialization error) is accomplished as normal side-effect of 
// destroying the frame and hence last view on document. We need explicit cleanup here.
// 
CDocument* CFBDDocTemplate::OpenDocumentFile
		(LPCTSTR lpszPathName, BOOL bMakeVisible /* = TRUE */) 
{
	// Create the doc. Note this does AddDocument to add to template's list
	CDocument* pDocument = CreateNewDocument();
	if (pDocument == NULL)
	{
		TRACE0("CDocTemplate::CreateNewDocument returned NULL.\n");
		AfxMessageBox(AFX_IDP_FAILED_TO_CREATE_DOC);
		return NULL;
	}
	ASSERT_VALID(pDocument);
    
/* Moved up from below: load the document data before creating frame: */
	if (lpszPathName == NULL)
	{
		// create a new document - with default document name
		SetDefaultTitle(pDocument);

		// avoid creating temporary compound file when starting up invisible
		if (!bMakeVisible)
			pDocument->m_bEmbedded = TRUE;

		if (!pDocument->OnNewDocument())
		{
			// user has been alerted to what failed in OnNewDocument
			TRACE0("CDocument::OnNewDocument returned FALSE.\n");
			/* pFrame->DestroyWindow(); */  // taken out: frame not created yet 
			delete pDocument;	/* added: explicit cleanup since no frame cleanup */
			return NULL;
		}
    
		// it worked, now bump untitled count
		m_nUntitledCount++;
	}
	else
	{
		// open an existing document
		CWaitCursor wait;
		if (!pDocument->OnOpenDocument(lpszPathName))
		{
			// user has been alerted to what failed in OnOpenDocument
			TRACE0("CDocument::OnOpenDocument returned FALSE.\n");
			/* pFrame->DestroyWindow();  */ // taken out: frame not created yet
			delete pDocument;	/* added: explicit cleanup since no frame cleanup */
			return NULL;
		}
		pDocument->SetPathName(lpszPathName);
	}
    
/* Moved down from above: Now create and update the frame */
	BOOL bAutoDelete = pDocument->m_bAutoDelete;
	pDocument->m_bAutoDelete = FALSE;   // don't destroy if something goes wrong
	CFrameWnd* pFrame = CreateNewFrame(pDocument, NULL);
	pDocument->m_bAutoDelete = bAutoDelete;
	if (pFrame == NULL)
	{
		AfxMessageBox(AFX_IDP_FAILED_TO_CREATE_DOC);
		delete pDocument;   // explicit delete on error
		return NULL;
	}
	ASSERT_VALID(pFrame);


	InitialUpdateFrame(pFrame, pDocument, bMakeVisible);
	return pDocument;
}

    
//
// We trap opening of document and notify the help system to initialize state
// for the new problem. Also to reset global flags for the new problem. 
// Currently updates feedback flag.
//
    
// helper derives problem ID from pathname if not specified
void CFBDDoc::EnsureProblemId(LPCTSTR lpszPathName)
{
	// see if name specified in problem file:
	if (! m_strProblemId.IsEmpty()) 
		return;
	// else not specified in document: use base of filename as problem name
	// Note GetTitle doesn't work since title not set till open completes

	// Copy file base name out of pathname into basename
	char basename[_MAX_PATH] = "";
	// following point into tail of pathname string:
	char * last_dotp = strrchr(lpszPathName, '.');
	char * last_backslashp = strrchr(lpszPathName, '\\');
	ASSERT(last_dotp != NULL);
	ASSERT(last_backslashp != NULL);
	char * basep = last_backslashp + 1;
	int baselen = last_dotp - basep;
	strncpy(basename, basep, baselen);
	basename[baselen] = '\0';
    
	// copy into member var
	m_strProblemId =  basename;
}
    
// Called by the framework to load data into document from file
// Although we still normally specify problems as .fbd files, we can obviate
// the need for .fbd files as follows: if the specified .fbd file does
// not exist, we try to import the problem from a .prb file.
// Note that since there is no .fbd file, one can't choose such a
// problem from a File Open dialog on .fbd files -- but that's OK,
// we can open it from a problem set that has only problem id, or
// other operations that construct a .fbd file path and try to open it.
// 
// We also allow for the explicit opening of prb files. File extension
// will be used to detect this case.
BOOL CFBDDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
	
	// Path type may specify either real fbd, "pseudo" fbd for which
	// we fall back to load from prb, or explicit prb to load from.
	CString strPathName(lpszPathName);
	CString strExt = strPathName.Mid(strPathName.ReverseFind('.'));
	
	// if .fbd file specified and it exists, load it
	CFileStatus statFile;
	if (strExt.CompareNoCase(".fbd") == 0 && CFile::GetStatus(strPathName, statFile)) 
	{
		// Call base class first to actually load the document via
		// standard MFC serialization. 
		if (!COleDocument::OnOpenDocument(lpszPathName))
			return FALSE;		// failed to open	
	}
	else // either non-existent fbd or explicit prb  
	{	 // try to import from prb file instead.

		// adjust extension if non-prb specified:
		if (strExt.CompareNoCase(".prb") != 0)
			strPathName.Replace(strExt, ".prb");
		BOOL bOK = LoadFromPrb(strPathName);
		if (! bOK) {
			// !!! need to show reason, e.g. if file doesn't exist or
			// has old format without info we need. For OLI, this may mean .fbd file
			// was not downloaded successfully.
#ifdef OLI
			theApp.DoWarningMessage("Failed to load problem. A required problem file may not have downloaded successfully. Close ANDES and try again another time.");
#else ! OLI
			theApp.DoWarningMessage("Failed to load problem from " +  strPathName);
#endif !OLI
			return FALSE;
		}
	}
	
	// Ensure we have problem id (derives from file name if unset).
	EnsureProblemId(lpszPathName);
	
	// OK, Problem data now successfully loaded.
	
	// if doc only opened to preview or print we are now done
	if (m_bFilePreview || theApp.m_bShellPrintOnly) {
		// save flag for use in on close document
		m_bPrintOnly = theApp.m_bShellPrintOnly;
		// reset global flag now that it has been consumed
		theApp.m_bShellPrintOnly = FALSE;
		return TRUE;
	}

	// if opening in demo mode, flag it as demo problem for post-demo cleanup
	m_bDemoProblem = theApp.m_bDemoMode;

	// Log the file open, using op with Andes-dir-relative arg if possible
	LogOpen(lpszPathName);

	// remember the problem start time (seconds)
	m_nStartTime = HistTime();

	// if opening for working, try to ensure help system is connected
	theApp.EnsureHelpSysInit(); // proceed w/o help even if failed

	// just to be safe: ensure we are not in TutorMode to start (e.g. from prev problem).
	theApp.SetTutorMode(FALSE);

	// Reset global feedback flag for new problem. We turn it off if help system not connected
	// (necessary? simply avoids help system calls that will fail anyway. The flag is here just 
	// in case we want to make this a settable parameter, e.g. for experimenting.
	// Note we must keep it set in case running from log file and simulating help system
	if (! HelpSystemIsConnected() && ! LogPlayerInPlayback()) {
		theApp.m_bFeedback = FALSE;
		return TRUE;			// If feedback off then no more to do
	} else
		theApp.m_bFeedback = TRUE;	// May be turned off if following fails
    
	// If get here, we are giving feedback: Tell help system to load problem info. 
	// It returns NIL if it was unsuccessful. "NO-HINTS" return means it can't give
	// hints for this problem. 
	LPCTSTR pszResult;
	if (m_nProblemType == PROB_EXAMPLE)
		// pszResult = HelpSystemExecf("(read-example-info \"%s\")", m_strProblemId);
		pszResult = HelpSystemExecf("(read-example-info \"%s\" %d)", 
								m_strProblemId, m_nAssessor);	
	else {
#ifndef OLI // for Min-Chi experiment: set long timeout on open for large problems.
		HelpIfcSetCallParms(10*60*1000, "Waiting for tutor to load problem");
#endif 
		pszResult = HelpSystemExecf("(read-problem-info \"%s\" %d %d)", 
								LISPSTR(m_strProblemId), m_nKBType, m_nAssessor);
	}
	// NB: NIL return means help system does not get a "close-problem" notification on 
	// finish, since it failed to open the problem (correct behavior?)
	if (! pszResult || (strcmp(pszResult, "NIL") == 0) ){
		theApp.DoWarningMessage( 
"Help System couldn't load problem information for this problem\nFeedback will not be available" 
		);
		theApp.m_bFeedback = FALSE;
	} else if (strcmp(pszResult, "NO-HINTS") == 0) {
		theApp.m_bNoHints = TRUE;
	} else	// any other non-NIL return:
		theApp.m_bNoHints = FALSE;
	
    // If newly opened doc has any student entries, must check them all with 
    // help system  to restore help system state. We log this as an event so
	// we can tell why these help system events are occurring.
	LogEventf(EV_CHECK_ENTRIES, "1");	
	if (theApp.m_bFeedback) // don't send cmds if helpsys didn't load!
	{
		HelpSystemSendf("(Check-Entries T)"); // notify helpsys of mode
		CheckInitEntries();
		HelpSystemSendf("(Check-Entries NIL)"); // notify helpsys end of mode
	}
	LogEventf(EV_CHECK_ENTRIES, "0");

	// After rechecking, log initial state of all entries in solution
	if (m_workState != workNone)
		LogInitEntries();
    // Log other initial state of document (id generation counter);
	LogEventf(EV_NEXT_ID, "%d", nNextID);

	// Now reset persistent scores in the help system
	if (theApp.m_bFeedback) 
		RestoreSavedScores();

    return TRUE;
}

// Log open converting arg to Andes-dir-relative filename if possible
void CFBDDoc::LogOpen(LPCTSTR lpszPathName)	
{
	// Log open by problem ID, adding full path as well so can tell if it's a
	// problem set solution file. Also in case loaded from out of problem dir.
	CString strPath = lpszPathName;	// default
	int iSubDir;					// indexes start of subdirectory in full path
	if (m_nProblemType == PROB_EXAMPLE) 
	{
		if ((iSubDir = strPath.Find(g_szExampleDir)) != -1)
			strPath = strPath.Mid(iSubDir + strlen(g_szExampleDir) + 1);

		LogEventf(EV_OPEN_EXAMPLE, "%s %s", m_strProblemId, strPath);
	} 
	else // PROBLEM
	{ 
		if ((iSubDir = strPath.Find(g_szProblemDir)) != -1)
			strPath = strPath.Mid(iSubDir + strlen(g_szProblemDir) + 1);

		LogEventf(EV_OPEN_PROBLEM, "%s %s", m_strProblemId, strPath);
	}
}

// This is used just before serializing document to fetch the latest saved
// scores from the help system, if it is available. Does nothing if helpsys
// not active on current problem. This is not done in OnCloseDocument because
// the file saving occurs before close of document, triggered by the SaveAllModified
// buffers mechanism called by the doc or app window close command handler.
void CFBDDoc::UpdateSavedStats()
{
	if (! (HelpSystemIsConnected() && theApp.m_bFeedback))
		return;

	LPCTSTR pszResult = HelpSystemExecf("(get-stats persist)");
	if (pszResult) 
		m_strSavedStats = pszResult;
}

//
// Trap close of document to notify help system. Can lead to long wait
// while Bayes net updates. Note this may be executed invisibly as part
// of app shutdown, if there were open documents when initiated.
//
void CFBDDoc::OnCloseDocument() 
{
	// if only opened for previewing or printing just do the close and return.
	if (m_bFilePreview || m_bPrintOnly){
		COleDocument::OnCloseDocument();
		return;
	}

	LogEventf(EV_CLOSE, ""); // Means close current document, should be clear in context.

	// For examples: check w/help system if permitted to close
	CMainFrame* pFrame = (CMainFrame*)AfxGetMainWnd();
	if ((!pFrame->m_bClosing)  && (m_nProblemType == PROB_EXAMPLE)) {
 		if (!theApp.m_bAuthorMode){
			if(!HelpAllowClose())
   				return;
   		}
 	}

	// If this is from a problem set, notify it that we are being closed, so it can update
	CString strProblemId = m_strProblemId;	// save for below
	CProblemSet* pSet = theApp.GetProblemSet();
	CTask* pTask = pSet ? pSet->FindTask(m_strProblemId) : NULL;
	if (pTask) { 
		pSet->PreCloseProblem(this);
	}

	// First do the base-class close so there is a quick visible response      
	// to the user's "close" command when problem disappears from screen. 
	// We suppress the framework's autodelete feature, which deletes the
	// doc object by default on close, so we can continue to use it while we finish.
	m_bAutoDelete = FALSE;
	COleDocument::OnCloseDocument();
	// Make a sound to aid synchronizing playback with external recording
	// MessageBeep(MB_ICONEXCLAMATION);

	// Now notify help system of problem close: //
	// Note: helpsys close call will reset score to zero. Save actual problem score
	// over this set, so score is available post-close for OLI updating. (Could also
	// set flag to suppress helpsys setscore handler from updating this doc).
	CString strScore = m_strScore; // save score over automatic reset on close

	// Make sure help system had problem open for feedback. Whether read-problem-info 
	// succeeded is recorded in the state of the m_bFeedback flag.
	if (HelpSystemIsConnected() && theApp.m_bFeedback)
	{

/*		// Set a long timeout and show custom message since old Bayes net
		// can take a while to update to completion on problem close
		const UINT nMsecsTimeout = 10 * 60 * 1000;		// 10 min better be enough */
		HelpIfcSetCallParms(HELPIFC_TIMEOUT, "Waiting for Andes to update its records");
		(void) HelpSystemExecf( "(close-%s \"%s\")", 
						m_nProblemType == PROB_EXAMPLE ? "example" : "problem", 
						LISPSTR(m_strProblemId) );
		// result of this call is unused, but must block till RPC returns:
	}

	m_strScore = strScore;  //restore score so PostClose handler has it

	// if this is in a problem set, notify it after problem close completed.
	if (pTask) { 
		pSet->PostCloseProblem(this);
	}

	// now we are really finished with the document so free it.
	// !!! Don't access methods or data beyond this point!!!
	delete this;	// suicide. 

	// Done close of this problem, have to decide what app should do next:
	// For students, we popup the task selection dialog again.
	// But don't do if the app is in process of shutting down. 
	// Also don't do if closing a problem in demo mode -- in which case we want to pop
	// to previous open problem -- or inside log playback, in which case we skip the task
	// dialog and recreate its effects -- or if player control is up (user may play more).
	// Note: DoTaskSelect checks for OLI mode and just closes set and exits in this case
	if (! (theApp.m_bAuthorMode || pFrame->m_bClosing 
		   || theApp.m_bDemoMode || LogPlayerInPlayback() || theApp.IsRemoteViewer()
		   || ::IsWindowVisible(pFrame->GetPlayerControl()->GetSafeHwnd()) )) 
	{
		// Documented MFC bug can occur when showing dialog here: By default, MFC sets 
		// dialog parent to frame that routed current command. In this case that can be 
		// the MDI child frame that has now gone away. We now take care of this in 
		// CChildFrame destructor (Bug is fixed in VC6 SP3).
		theApp.DoTaskSelect();
	}
}
    
    
// 
// CheckInitEntries: resubmit all initial entries on load to restore help system state
//
// Forward references are a problem. That is, some definitions reference other objects 
// and will be marked incorrect if the dependencies have not yet been defined to the 
// help system. Order in doc's object list is not necessarily order of dependency. E.g. 
// can draw system B, define vector on B, delete B, then draw B again. More simply, move
// objects to front or back. So we must deal with the possibility that definitions later 
// in object list are needed to vindicate correctness of objects occurring earlier.
// 
// We could topologically sort the dependency graph so that the defs are linearly
// ordered. For now, just make two passes; first to ensure all objects defined, second 
// to really check the objects. 
// Note no need to apply this to equations or answer fields, which are just checked once.


// for iterating through all "CheckedObj's" in a document = 
// student diagram entries and variable entries.
class CChkObjIter 
{
	CFBDDoc* m_pDoc;			// document in which to find entries
	BOOL     m_bInDiagramList;  // T=> in m_objects, else in m_variables
	POSITION pos;				// MFC list position in relevant list
public:
	CChkObjIter(CFBDDoc* pDoc) : m_pDoc(pDoc) { Reset(); };
	void	Reset() 
	{
		m_bInDiagramList = TRUE; 
		pos = m_pDoc->m_objects.GetHeadPosition();
	}
		 
	CCheckedObj*  Next()
	{
		if (m_bInDiagramList) { // try next diagram entry
			CDrawObj* pObj = NULL;
			while (pos != NULL) {
				CDrawObj* pObj = m_pDoc->m_objects.GetNext(pos);
				if (pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)) &&
					pObj->m_flag == STUDENT_OBJECT)
					return (CCheckedObj*)pObj;
			}
			// else no more diagram entries, move on to var list & fall thru
			m_bInDiagramList = FALSE;
			pos = m_pDoc->m_Variables.GetHeadPosition();
		}
		// in var list: return next var
		if (pos == NULL)		// no more variables
			return NULL;
		return m_pDoc->m_Variables.GetNext(pos);
	}
};

// Log all entries so they can be recreated on playback of solution file	
void CFBDDoc::LogInitEntries()
{
	// log all diagram objects and defined variables
	CChkObjIter chkObjs(this);
	while (CCheckedObj* pChkObj = chkObjs.Next()) {
		pChkObj->LogEntry();
	}
	
	// log equation entries with status
	for (int iEq = 0; iEq < NEQS; ++iEq) {	
		CString strEq =   m_strEq[iEq];
		strEq.TrimLeft();	// strip leading whitespace & see if anything left
		if (!strEq.IsEmpty()) 
			LogEventf(EV_EQ_ENTRY, "%d %d %s", iEq, m_statusEq[iEq], m_strEq[iEq]);
	}

	// log answer entries with status
	POSITION pos = GetObjects()->GetHeadPosition();	
	while (pos != NULL) {
		CDrawObj* pObj = GetObjects()->GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) &&
			((CDrawRect*)pObj)->IsAnswerBox() &&
			! pObj->m_strName.IsEmpty() ) 
			// !!! also do for done buttons !!!
			LogEventf(EV_ANSWER_ENTRY, "%s %d %s", pObj->m_strId, pObj->m_status,
										pObj->m_strName);

		// !!! Need to log multiple choice answer state
		if (CFBDView::IsChoiceGroup(pObj)) {
			// loop through items looking for correct choice check
			BOOL bCorrect = FALSE; // true if find correct answer checked
			for (POSITION pos1 = ((CGroup*)pObj)->m_objects.GetHeadPosition(); pos1; ) {
				CDrawObj* pItem = ((CGroup*)pObj)->m_objects.GetNext(pos1);
				if ( pItem->IsKindOf(RUNTIME_CLASS(CChoice)) )  { // hit a choice	 
					if (((CChoice*) pItem)->m_bChosen /*&& pItem->m_status == statusCorrect*/) {
						// !!! Log that this choice was chosen in way that it can be replayed:
						// EV_CHOICE_CLICKED - low-level event, logs by windows child ctrl id which is assigned 
						// to button control within preset range by FBDView when creating buttons. Should stay the
						// same if problem choices don't change. (Can it change if choice-representing drawobjs
						// are reordered within the display list?)
						// This is what is currently replayed by the log player. Also have higher level event:
						// EV_CHOICE_SELECT - logs by group id and ordinal of choice. But don't have code to
						// replay it yet.
						break;
					}
				}
			}
		} // if choice group
	}

	// TODO: principles, qualitative stuff
}

extern int split(const CString& str, const CString& Sep, CStringArray& result);
	
void CFBDDoc::CheckInitEntries()
{
	CStringList listErrors;				// result parameter unused in this context

	// Ensure feedback sounds off during all these submisions
	BOOL bOldSoundFlag = theApp.m_bPlaySounds;	// save old state
	theApp.m_bPlaySounds = FALSE;			

	// Views have not done their initial updates at this point, so edit controls for
	// answers, eqs, don't exist yet. So we only update objects' status in document. 
	// Note this means we ignore any piggybacked commands in help system returns.

	for (int nPass = 1; nPass <= 2; nPass++ ) // do twice
	{
		// submit all checkable drawn objects and variables
		CChkObjIter chkObjs(this);
		while (CCheckedObj* pChkObj = chkObjs.Next()) {
			pChkObj->CheckObject();
		}
	}

	// submit all solution plan principles.  These might
	// depend on defined compound bodies, so do it *after* bodies.
	// Note don't check "done" checkmarks until after have submitted all entries
	POSITION pos = m_principles.GetHeadPosition();
	while (pos != NULL) {
		CPrincObj* pPrinc = m_principles.GetNext(pos);
		pPrinc->CheckObject();
	}

	// Now submit all the equations. All variables used must have been defined above
	for (int nEq = 0; nEq < NEQS; ++nEq) {
		CString strEq =   m_strEq[nEq];
		strEq.TrimLeft();	// strip leading whitespace, process if anything left
		// don't re-check if saved status unknown, let them remain as unsubmitted
		if (!strEq.IsEmpty() && m_statusEq[nEq] != statusUnknown) 
		{
			// urgh, no subroutine to check an equation outside of view
			// strEq.Replace("\"", "\\\"");  // escape any embedded quotes for Lisp read
			LPCTSTR pszResult = HelpSystemExecf("(lookup-eqn-string \"%s\" %d)", LISPSTR(strEq), nEq);
			CCheckedObj::ApplyStatus(pszResult, m_statusEq[nEq], listErrors);
		}
	}

	// now submit contents of any answer boxes in the problem.
	pos = m_objects.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = m_objects.GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) && ((CDrawRect*)pObj)->IsAnswerBox() ) {
			CString strAnswer = pObj->m_strName;
			strAnswer.TrimLeft();  // strip leading whitespace, process if anything left
			// don't recheck if saved status unknown, let it remain as unsubmitted
			if (! strAnswer.IsEmpty() && pObj->m_status != statusUnknown)
			{	
				LPCTSTR pszResult = HelpSystemExecf("(check-answer \"%s\" %s)", LISPSTR(strAnswer), pObj->m_strId);
				CCheckedObj::ApplyStatus(pszResult, pObj->m_status, listErrors);
			}
		}
		// also do for "I'm done" check boxes. Identify by label Answer-1, etc like answer boxes.
		if (pObj->IsKindOf(RUNTIME_CLASS(CChoice)) &&
			((CDrawRect*)pObj)->IsAnswerBox()) { // dangerous but OK, refs CDrawObj::m_strId only
			CChoice* pChoice = (CChoice*) pObj;
			if (pChoice->m_bChosen) // no need to notify if unchecked
			{
				LPCTSTR pszResult =  HelpSystemExecf("(lookup-mc-answer %s 1)", pChoice->m_strId);
				CCheckedObj::ApplyStatus(pszResult, pChoice->m_status, listErrors);
			}
		}
		// also do for multiple choice question answers
		if (CFBDView::IsChoiceGroup(pObj)) {
			// loop through group's items, counting choices until hit the checked one
			int nChoice = 0;    // to be inc'd for each choice found
			for (POSITION pos1 = ((CGroup*)pObj)->m_objects.GetHeadPosition(); pos1; ) {
				CDrawObj* pItem = ((CGroup*)pObj)->m_objects.GetNext(pos1);
				if ( pItem->IsKindOf(RUNTIME_CLASS(CChoice)) )  { // hit a choice	 
					 nChoice += 1;
					 if (((CChoice*) pItem)->m_bChosen) {         // it's the chosen one
						// N.B: must send containing Group (=pObj) id as question-id, not choice item's
						LPCTSTR pszResult =  HelpSystemExecf("(lookup-mc-answer %s %d)", pObj->m_strId, nChoice);
						CCheckedObj::ApplyStatus(pszResult, pItem->m_status, listErrors);
					 }
				}
			}
		}
	}
	
	// after all work, submit all "done" checkmarks on principle applications.
	pos = m_principles.GetHeadPosition();
	while (pos != NULL) {
		CPrincObj* pPrinc = m_principles.GetNext(pos);
		// skip erroneous principles, can never be done
		if (pPrinc->m_status == statusError) 
			continue;
		if (pPrinc->HasSubItems()) {
			// check donenesss of all substeps. Currently only one level to check
			POSITION posStep = pPrinc->m_subItems.GetHeadPosition();
			while (posStep != NULL) {
				CPrincStep* pStep = (CPrincStep*) pPrinc->m_subItems.GetNext(posStep);
				// check done marking on substep
				if (pStep->m_bChecked) {
					CString strStageId = pStep->GetStageId();
					CString strStepId = pStep->GetStepId();
					LPCSTR pszResult = HelpSystemExecf("(substep-finished %s %s)",
									STR2ARG(strStepId), strStageId);
					CCheckedObj::ApplyStatus(pszResult, pStep->m_checkStatus, listErrors);
				}
			}
		} else if (pPrinc->m_bChecked) {	// check done marking on principle
			CString strStageId = pPrinc->GetStageId();
			LPCSTR pszResult = HelpSystemExecf("(substep-finished NIL %s)", strStageId);
			CCheckedObj::ApplyStatus(pszResult, pPrinc->m_checkStatus, listErrors);
		}
	}
}

// send saved persistent scores back to help system
void CFBDDoc::RestoreSavedScores()
{
	if (m_strSavedStats.IsEmpty()) 
		return;

	// urgh, need to parse score string and convert into help sys format.
	// make list of score-value pairs. 
	CString strArgList;
	CStringArray strStats, strFields;
	int nStats = split(m_strSavedStats, ";", strStats);
	for (int iStat = 0; iStat < nStats; iStat++)   // for each stat
	{
			// split fields from "name weight value" triple
			int nFields = split(strStats[iStat], " ", strFields);
			ASSERT(nFields == 3);
			CString strName = strFields[0];
			CString strScore = strFields[2];
			CString strArg;	int nNum, nDenom;
			if ((sscanf(strScore, "%d/%d", &nNum, &nDenom)) == 2)
				strArg.Format(" (%s (%d %d))", strName, nNum, nDenom);
			else
				strArg.Format(" (%s %s)", strName, strScore);
			strArgList += strArg;
	}

	// call help sys API. We can't do anything with a failure result, so
	// just post the callx and barge on. 
	HelpSystemSendf("(set-stats %s)", strArgList);

}

// Check for completion, updating work state in doc
WorkState CFBDDoc::UpdateWorkState()
{
	// problem is complete if all answer boxes filled in w/status correct
	BOOL bCompleted = TRUE; // until find bad/missing answer
	POSITION pos = GetObjects()->GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = GetObjects()->GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) &&
			((CDrawRect*)pObj)->IsAnswerBox() &&
			((CDrawRect*)pObj)->m_status != statusCorrect) {
				bCompleted = FALSE;	
				break;
		}
		// New: check "I'm done" buttons as well. "filled in" = checked
		if (pObj->IsKindOf(RUNTIME_CLASS(CChoice)) &&
			((CDrawRect*)pObj)->IsAnswerBox()) { // dangerous!! OK, refs CDrawObj::m_strId only
			CChoice* pChoice = (CChoice*) pObj;
			// if not both checked and correct => not done 
			if (! (pChoice->m_bChosen && pChoice->m_status == statusCorrect)) {
				bCompleted = FALSE;	  
				break;
			}
		}
		// Also: check mc-choice buttons as well.
		if (CFBDView::IsChoiceGroup(pObj)) {
			// loop through items looking for correct choice check
			BOOL bCorrect = FALSE; // true if find correct answer checked
			for (POSITION pos1 = ((CGroup*)pObj)->m_objects.GetHeadPosition(); pos1; ) {
				CDrawObj* pItem = ((CGroup*)pObj)->m_objects.GetNext(pos1);
				if ( pItem->IsKindOf(RUNTIME_CLASS(CChoice)) )  { // hit a choice	 
					if (((CChoice*) pItem)->m_bChosen && pItem->m_status == statusCorrect) {
						bCorrect = TRUE;
						break;
					}
				}
			}
			if (! bCorrect) {
				bCompleted = FALSE;
				break;
			}
		}
	}
	if (bCompleted) 
		return (m_workState = workCompleted);

	// problem has partial work if has at least one entry
	
	// look for principle (solution plan) entries
	if (m_principles.GetHeadPosition() != NULL) 	// at least one on list
			return (m_workState = workPartial);

	// look for diagram entries
	pos = m_objects.GetHeadPosition();	
	while (pos != NULL) {
		CDrawObj* pObj = m_objects.GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)) &&
			pObj->m_flag == STUDENT_OBJECT) 
				return (m_workState = workPartial);
	}

	// look for variable entries
	if (m_Variables.GetHeadPosition() != NULL) 	// at least one on list
		return (m_workState = workPartial);

	// look for equation entries
	for (int nEq = 0; nEq < NEQS; ++nEq) {	
		CString strEq =   m_strEq[nEq];
		strEq.TrimLeft();	// strip leading whitespace & see if anything left
		if (!strEq.IsEmpty()) 
			return (m_workState = workPartial);
	}

	// look for answer entries
	pos = GetObjects()->GetHeadPosition();	
	while (pos != NULL) {
		CDrawObj* pObj = GetObjects()->GetNext(pos);
		if (pObj->IsKindOf(RUNTIME_CLASS(CDrawRect)) &&
			((CDrawRect*)pObj)->IsAnswerBox() &&
			! pObj->m_strName.IsEmpty() ) 
			return (m_workState = workPartial);

		// look for multiple choice question selections
		if (CFBDView::IsChoiceGroup(pObj)) {
			for (POSITION pos1 = ((CGroup*)pObj)->m_objects.GetHeadPosition(); pos1; ) {
				CDrawObj* pItem = ((CGroup*)pObj)->m_objects.GetNext(pos1);
				if ( pItem->IsKindOf(RUNTIME_CLASS(CChoice)) &&
					((CChoice*) pItem)->m_bChosen )   // hit a checked choice	 
					return (m_workState = workPartial);
			}
		}
		
		// check done buttons for qual problems? ignore for now
	}
	

	// get here => no entries found:
	return (m_workState = workNone);
}  

// Get string for OLI score
CString CFBDDoc::GetWorkStateStr()
{
	UpdateWorkState();
	static const char* szStates[] = {
      /*workNone*/      "Visited",			
      /*workPartial*/	"Partial",
      /*workCompleted*/ "DONE",
      /*workUnknown*/	"????",
    };
	return CString(szStates[m_workState]);
}

//
// Check with help system if can close problem
// This is used on examples by the example study coach 
// to prompt items to study more
//
BOOL CFBDDoc::HelpAllowClose()
{
	// make sure example study coach is enabled
	if (!(theApp.m_wHelpFlags & fExample))
		return TRUE;

	LPCTSTR pszResult = HelpSystemExecf("(approve-close)");
	if (pszResult && (strcmp(pszResult, "T") != 0)){
	
	// Display result in hint dialog, which knows how to parse it.
	CEXHintDlg dlg;
	dlg.m_pszHintSpec = pszResult;
   	int response = dlg.DoModal();
	CMainFrame* pFrame = (CMainFrame*)AfxGetMainWnd();
	if (response == IDOK) {
			CEXPlanVw* ppView = theApp.GetEXPlanVw();
			if (ppView->IsWindowEnabled())
				ppView->OnDone(FALSE);
			CEXView* pView = theApp.GetEXView();
  			pView->m_bCoachMode = TRUE;
  			pView->HighlightHint(dlg.m_strHints);
			pFrame->m_bClosing = FALSE;
		   	return FALSE;
		}
	}
	return TRUE;
}

////////////////////////////////////////////////////////////////////////////////////////
//
// File saving customizations
//
////////////////////////////////////////////////////////////////////////////////////////

// This is kind of hairy since we copied the default MFC file saving code and injected
// our customizations to  do what we want.  Normal path is Save cmd -> DoFileSave 
// worker routine which implements the Save command. If file doesn't exist (i.e. on 
// first save), DoFileSave calls the multi-use worker routine DoSave(NULL)
// to effect a SaveAs, setting doc buffer to reference new filename.
//
// We want solutions to problems from a problem set to be saved in a standard 
// location so the program can always find them when student reopens the problem 
// set document, to can display student's work status in the probset view. 
// So we copied DoSave and modified the NULL case to silently save into standard problem 
// set solution location without prompting for a filename in this, the most common
// case. In the case of "loose" problems not opened from a problem set (which we may
// be disallowing in the future), we let it do the normal prompting behavior, though 
// we suggest a standard name in the student's solution directory so multiple students 
// can use the same installation.
//
// However, we also have to handle the case where come into DoSave because of a 
// true user-initiated SaveAs command. To detect that, we just set a flag
// on receiving the SaveAs command so our DoSave additions can distinguish it.
//
// Also, in case of a problem set solution, we take SaveAs to function as
// SaveCopyAs: it saves a copy wherever the user wants it but 
// doesn't change the buffer so that future saves go into that copy. This is 
// because we currently would have no easy way for student to get back to saving the 
// solution into the standard location (students need not ever know where it is).
//
// Note SaveAs functionality is not important to our students; we expect them to let 
// ANDES manage all their saved solutions automatically, as a mail client manages 
// messages in folders. It's mainly for use by developers to save file snapshots of 
// some state for some reason. Students might find it useful to make copies of
// their finished solutions as well.

void CFBDDoc::OnFileSaveAs() 
{
	// Flag that save is user-initiated SaveAs for DoSave, which resets
	m_bSaveAsCmd = TRUE;

	// For problem sets, change it to SaveCopyAs via our own worker routine
	CProblemSet* pSet = theApp.GetProblemSet();
	CTask* pTask = pSet ? pSet->FindTask(m_strProblemId) : NULL;
	if (pTask)
		DoSaveCopyAs();
	else
		COleDocument::OnFileSaveAs(); // CDoc method just calls DoSave(NULL)
}

void CFBDDoc::OnUpdateFileSaveAs(CCmdUI* pCmdUI) 
{
	CProblemSet* pSet = theApp.GetProblemSet();
	CTask* pTask = pSet ? pSet->FindTask(m_strProblemId) : NULL;
	// If doc is in current problem set and student mode,
	// change "SaveAs" text to Save Copy As" (but don't change command ID).
	if (pTask && ! theApp.m_bAuthorMode)
		pCmdUI->SetText("Save Copy As");
	else 
		pCmdUI->SetText("Save As");
}

BOOL CFBDDoc::DoSaveCopyAs()
{
	// we don't want saving a copy to change modified flag for current buffer,
	// so we have to save and restore it around OnSaveDocument, which clears it 
	// by default on every save. 
	BOOL bModified = IsModified();
	BOOL bSuccess = DoSave(NULL, FALSE);
	SetModifiedFlag(bModified);
	return bSuccess;
}
   
BOOL CFBDDoc::DoSave(LPCTSTR lpszPathName, BOOL bReplace)
	// Save the document data to a file
	// lpszPathName = path name where to save document file
	// if lpszPathName is NULL then the user will be prompted (SaveAs)
	// note: lpszPathName can be different than 'm_strPathName'
	// if 'bReplace' is TRUE will change file name if successful (SaveAs)
	// if 'bReplace' is FALSE will not change path name (SaveCopyAs)
{
	CString newName = lpszPathName;
	if (newName.IsEmpty())
	{
		CDocTemplate* pTemplate = GetDocTemplate();
		ASSERT(pTemplate != NULL);

		newName = m_strPathName;
		if (bReplace && newName.IsEmpty())
		{
			newName = m_strTitle;
			// check for dubious filename
			int iBad = newName.FindOneOf(_T(" #%;/\\"));
			if (iBad != -1)
				newName.ReleaseBuffer(iBad);

			// append the default suffix if there is one
			CString strExt;
			if (pTemplate->GetDocString(strExt, CDocTemplate::filterExt) &&
			  !strExt.IsEmpty())
			{
				ASSERT(strExt[0] == '.');
				newName += strExt;
			}
		}
		BOOL bPrompt = TRUE;	// default is to prompt for new name	
		if (!theApp.m_bAuthorMode)	// Saving a student solution: 
		{
			// Stamp solution file with current student name on first save. Name will
			// persist on all subsequent saves. This shows up on printout to deter 
			// students opening another student's solution then printing as his own.
			// We do this here because we don't want the creator id stamped into a
			// problem master file when it is saved.
			if (m_strCreator.IsEmpty())
				m_strCreator = theApp.m_strUserName;

			// if a problem set solution: we derive standard name and
			// save silently into solution folder (no prompting for save name)
			CProblemSet* pSet = theApp.GetProblemSet();
			if (pSet && ! m_bSaveAsCmd) {	// but don't do for SAVE_AS cmd
				CString strPath;
				CTask* pTask = pSet->FindTask(m_strProblemId);
				if (pTask)
					strPath = pTask->GetSolutionPath(TRUE); // makes dir if nec.
				if (! strPath.IsEmpty()) { 
					newName = strPath;
					bPrompt = FALSE;	
				}
			}
			if (bPrompt) {	// no probset or doing a save as 
				// get prompt name from title using old method (+ added student id)
				int iDot = newName.Find('.');
				CString strExt = newName.Mid(iDot);
				
				// For now, prompt name just ignores any existing Title, which may
				// be anything if opened after earlier SaveAs's. 
				// Try to suggest "PROBNAME-Solution" in all cases.
				// and try to save it in the student's subdir if possible
				CString strSolnDir = g_strAndesDir + g_szProblemDir + "\\" + g_szSolutionDir;		
				CString strDir = strSolnDir + "\\" +  theApp.m_strUserName;
				CFileStatus statDir;
				if (! theApp.m_strUserName.IsEmpty() &&
					! CFile::GetStatus(strDir, statDir) &&
					! ::CreateDirectory(strDir, NULL)) { 
					// failed to find student dir, so insert student's name into filename
					// so it isn't confused with other student's solutions.
					newName = strSolnDir + "\\" +  m_strProblemId +  
						      "-" + theApp.m_strUserName + "-Solution" + strExt;
				} else {
					newName = strDir + "\\" + m_strProblemId + "-Solution" + strExt;
				}
				
			}
		}
		if (m_bSaveAsCmd) {	// in case of SAVE_AS cmd
			bPrompt = TRUE;			// always prompt for filename
			m_bSaveAsCmd = FALSE;	// reset flag for next time
		}
		if (bPrompt && !AfxGetApp()->DoPromptFileName(newName,
		  bReplace ? AFX_IDS_SAVEFILE : AFX_IDS_SAVEFILECOPY,
		  OFN_HIDEREADONLY | OFN_PATHMUSTEXIST, FALSE, pTemplate))
			return FALSE;       // don't even attempt to save
	}

	CWaitCursor wait;

	if (!OnSaveDocument(newName))
	{
		if (lpszPathName == NULL)
		{
			// be sure to delete the file
			TRY
			{
				CFile::Remove(newName);
			}
			CATCH_ALL(e)
			{
				TRACE0("Warning: failed to delete file after failed SaveAs.\n");
				e->Delete();
			}
			END_CATCH_ALL
		}
		return FALSE;
	}

	// reset the title and change the document name
	if (bReplace)
		SetPathName(newName);

	return TRUE;        // success
}  

// This is called by the framework to Save a Modifed buffer. We modify to
// do a silent save into the solution directory without prompting, in OLI
// case (could do in all!)
BOOL CFBDDoc::SaveModified() 
{
	CProblemSet* pSet = theApp.GetProblemSet();
	if (! (pSet && pSet->m_bOli)) {
		return COleDocument::SaveModified();
	}
	// Else we're running an OLI problem:

	// AW: Following copied from COleDocument::SaveModified:

	// sometimes items change without a notification, so we have to
	//  update the document's modified flag before calling
	//  CDocument::SaveModified.
	UpdateModifiedFlag();
	// Didn't copy bit worrying about in-place active objects (won't happen for students)

	// AW: Following copied from CDocument::SaveModified. Might want to save even if
	// not modified (otherwise on first open solution then quit, no solution will exist).
/*
	if (!IsModified())
		return TRUE;        // ok to continue
*/
	// get name/title of document	// AW -- maybe unnecessary for us
	CString name;
	if (m_strPathName.IsEmpty())
	{
		// get name based on caption
		name = m_strTitle;
		if (name.IsEmpty())
			VERIFY(name.LoadString(AFX_IDS_UNTITLED));
	}
	else
	{
		// get name based on file title of path name
		name = m_strPathName;
		// if (afxData.bMarked4) //
		{
			extern UINT AFXAPI AfxGetFileTitle(LPCTSTR lpszPathName, LPTSTR lpszTitle, UINT nMax);
			AfxGetFileTitle(m_strPathName, name.GetBuffer(_MAX_PATH), _MAX_PATH);
			name.ReleaseBuffer();
		}
	}

	// Here is where CDocument::SaveModified prompts to Save. We do the YES action always.
	// If so, either Save or Update, as appropriate
	if (!DoFileSave()) {
		int nResult = AfxMessageBox("Failed to save solution file! Continue closing anyway?", 
			           MB_YESNO|MB_ICONEXCLAMATION|MB_DEFBUTTON2);
		return nResult == IDYES ? TRUE: FALSE;       
	}
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////////
//
// Methods for Managing list of drawing objects
//
/////////////////////////////////////////////////////////////////////////////////
    
//
// Doc::Draw: Render all objects into given DC.
// Optional view arg is used to provide info on selection and whether view is active
// 
void CFBDDoc::Draw(CDC* pDC, CFBDView* pView /*=NULL*/)
{
	POSITION pos = m_objects.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = m_objects.GetNext(pos);
		pObj->Draw(pDC);
		/* Simple version to draw selected objects specially
		if (!pDC->IsPrinting() && pView && pView->IsSelected(pObj))
			pObj->DrawSelectState(pDC, CDrawObj::selected); */

		// More subtle logic from Drawcli: don't show selection in non-active views
		// also don't show when printing or rendering into metafile (for clipboard) 
		if (! pDC->IsPrinting() && pDC->m_hDC == pDC->m_hAttribDC // -> not metafile 
			&& pView && pView->m_bActive && pView->IsSelected(pObj))
			pObj->DrawSelectState(pDC, CDrawObj::selected);
	}
}
    
// DrawMasked: render whole document into DC in masked (example study) mode
// applies DrawMasked to each object in turn
void CFBDDoc::DrawMasked(CDC* pDC)
{
	POSITION pos = m_objects.GetHeadPosition();
	while (pos != NULL)
	{	
		CDrawObj* pObj = m_objects.GetNext(pos);
		pObj->DrawMasked(pDC);
	}
}
    
//
// Generate and set drawobj id, ensuring unique within this document
//
void CFBDDoc::GenerateId(CDrawObj* pObj)
{
	// For readable ID we combine class name after leading "C" and id counter
	char szID[255];
	sprintf(szID, "%s-%d", pObj->GetRuntimeClass()->m_lpszClassName + 1, nNextID++);
	pObj->m_strId = szID;
    
}
    
void CFBDDoc::GenerateId(CVariable* pVar)
{
	// For readable ID we combine abbreviated classname and id counter
	char szID[255];
	sprintf(szID, "%s-%d", "Var", nNextID++);
	pVar->m_strId = szID;
}

void CFBDDoc::GenerateId(CStageObj* pStage)
{
	//just a precautionary measure, should never get this high
	//id can't be 10, because will be out of control range functions
	if (nNextStageID >= 10)
		pStage->m_nId = GetUnusedId();	// should be able to recycle earlier one.
	else
		pStage->m_nId = nNextStageID++;
}


int CFBDDoc::GetUnusedId()
{
	for (int id=0; id<10; id++)
	{
		BOOL bExists = FALSE;
		POSITION pos = m_stages.GetHeadPosition();
		while (pos != NULL) 
		{
			CStageObj* pStage = m_stages.GetNext(pos);
			if (pStage->m_nId == id){
				bExists = TRUE;
				break;
			}
		}
		if (!bExists)
			return id;
	}
	return -1;
}

    
// Add -- Add new drawing object to document.
// 
// Appends to the end of the object list so it will be added on top or earlier ones
// Marks document modified but leaves it to caller to control invalidating/repaint.
//
// Normally this routine generates a document-unique id for object. That
// is appropriate if it is the first time this object is linked into document
// (We don't generate id on object creation because at that time the object is not 
// part of any document, and our unique ids are persistent and document-relative). 
// The optional flag argument can be set to FALSE to suppress id generation. This
// can be used when moving existing objects around in the list or hierarchy, as for 
// grouping or ungrouping. (Though unclear exactly what is right in that case -- could
// treat group/ungroup sequence as destroying of old objects and adding some
// new ones, in which case new ids *should* be generated.) The ids are mainly 
// for use with student solution entries to communicate with help system and in log 
// file. Read-only graphic objects get ids too, but they are not that important.
//
void CFBDDoc::Add(CDrawObj* pObj, BOOL bGenID /* = TRUE */)
{
	m_objects.AddTail(pObj);
	pObj->m_pDocument = this;
	if (bGenID)
		GenerateId(pObj);
	SetModifiedFlag();
}

CDrawObj* CFBDDoc::AddClone(CDrawObj* pOrigObj, BOOL bOffset /* = TRUE*/)
{
	CDrawObj* pClone = pOrigObj->Clone();

	if (!pClone)
		return NULL;
	
	if (bOffset)
		// offset position slightly so duplicate is visible
		pClone->MoveTo(pClone->m_position + CSize(10, 10));
   	
   	// and add to document
   	Add(pClone);
   	pClone->Invalidate();
	return pClone;
}
    
// 
// Remove: Delete a given drawing object from the object list.
// 
// Propagates a callback to all FBDViews' RemoveFromSel method
// Marks document modified but leaves it to caller to control invalidating/repaint.
// Note this just unlinks from list, doesn't free object storage
//
void CFBDDoc::Remove(CDrawObj* pObj)
{
	// Find and remove from document
	POSITION pos = m_objects.Find(pObj);
	if (pos != NULL)
		m_objects.RemoveAt(pos);
	SetModifiedFlag();

	//From DRAWCLI:  Notify each FBDView so that the view can remove from selection
	pos = GetFirstViewPosition();
	while (pos != NULL){
		CView* pView = GetNextView(pos);
		if (pView->IsKindOf(RUNTIME_CLASS(CFBDView)))
			((CFBDView*)pView)->RemoveFromSel(pObj);
	}
}

// DeleteObjlist -- Remove a list of drawn objects from the document and delete.
// This is actually only used to free set of dependent angle objects
// whenever user deletes a vector or axis. Diagram view uses different method
// when deleting a multiple selection (deletes them one by one). 
void CFBDDoc::DeleteObjList(CDrawObjList* pObjList)
{
	// Notify view to redraw whole set of objects. 
	UpdateAllViews(NULL,  HINT_DELETE_SELECTION , pObjList);

	POSITION pos = pObjList->GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = pObjList->GetNext(pos);
		// Unlink from display list
		Remove(pObj);
		// Notify helpsys this object is no more
		((CCheckedObj*)pObj)->NotifyDelete();
		// and free it 
		pObj->Delete();
	}
}
    
//
// ObjectAt: Find object at a given page location (logical coords) in doc. 
// Optional filter function should return true for objects to ignore in test
// We use this mainly to ignore author-drawn problem objects in student mode.
//
CDrawObj* CFBDDoc::ObjectAt(const CPoint& point, BOOL (*pfnbIgnore)(CDrawObj* pObj) )
// point is in logical coordinates
{
	/* CRect rect(point, CSize(1, 1)); */
	POSITION pos = m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = m_objects.GetPrev(pos);
		if ( pfnbIgnore != NULL && (*pfnbIgnore)(pObj) )
			continue;
		
		if 	(pObj->HitTest(point, NULL, FALSE))
			return pObj;

	}

	return NULL;
}
 
//
// ItemAt: Find outline item at a given display location (logical coords) in 
//         Hi-level solution plan. 
//
COutlineItem* CFBDDoc::ItemAt(const CPoint& point)
// point is in logical coordinates
{
	POSITION pos = m_stages.GetTailPosition();
	while (pos != NULL)
	{
		CStageObj* pStage = m_stages.GetPrev(pos);
		for (int j=pStage->m_items.GetSize()-1; j>=0;  j--)
		{
			COutlineItem* pItem = pStage->m_items[j];
			if 	(pItem->HitTest(point))
				return pItem;
		}

	}

	return NULL;
}
   
// Find drawn object with given id, return NULL if not found
CDrawObj* CFBDDoc::Lookup(LPCTSTR pszId)
{
	POSITION pos = m_objects.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = m_objects.GetNext(pos);
		if (pObj->m_strId == pszId)
			return pObj;
	}
	return NULL;
}

// Same as above but searches by user's label
CDrawObj* CFBDDoc::FindByName(LPCTSTR pszName)
{
	POSITION pos = m_objects.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = m_objects.GetNext(pos);
		if (pObj->m_strName == pszName)
			return pObj;
	}
	return NULL;
} 

// Find drawn object with same def or name as candidate.
// Used when checking new submissions for duplication.
CDrawObj* CFBDDoc::GetMatchingObj(CDrawObj* pObj, BOOL bMatchName)
{
	POSITION pos = m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pListObj = m_objects.GetPrev(pos);
		//Skip teacher drawn problem objects
		if (pListObj->m_flag == TEACHER_OBJECT)
			continue;
		if (bMatchName)
		{
			if (pObj->HasSameName(pListObj))
				return pListObj;
		}
		else
		{
			if (pObj->HasSameDef(pListObj))
				return pListObj;
		}
	}
	return NULL;

}

// Find variable def with same def or name as candidate.
// Used when checking new submissions for duplication.
CVariable* CFBDDoc::GetMatchingVar(CDrawObj* pObj, BOOL bMatchName)
{
	POSITION pos = m_Variables.GetTailPosition();
	while (pos != NULL)
	{
		CVariable* pVar = m_Variables.GetPrev(pos);
		if (bMatchName)
		{
			if (pObj->HasSameName(pVar))
				return pVar;
		}
		else// Match Definition
		{
			if (pObj->HasSameDef(pVar))
				return pVar;
		}
		
	}
	return NULL;
}

// Find matching predefined variable 
// Currently only used for predifined time point variables, which are "virtual"
// definitions only (they're not on the defined variable list).
// RETURNS existing definition (for use in error diagnostic); 
//         empty string if none
CString CFBDDoc::GetMatchingPredef(CString& strName)
{
	CString strDef;	// return result

#ifndef ANDES401_COMPAT	// define to build player for ANDES401 logs that fail new check
	// look for matching time names
	POSITION pos = m_strTimes.GetHeadPosition();
	while (pos != NULL)
	{
		CString strTime = m_strTimes.GetNext(pos);
		// split name from string of form "T0 = block begins moving" 
		int eqPos = strTime.Find('=');
		if (eqPos < 0) continue;
		CString strTimeName = strTime.Left(eqPos);
		strTimeName.TrimRight();		// strip space before =

		// Changed for Andes2 to be case sensitive, since new helpsys is
		if (strTimeName.Compare/*NoCase*/(strName) == 0) {	// found one
			strDef = strTime.Mid(eqPos + 1);
			strDef.TrimLeft();			// strip space after =
			return "the time " + strDef;
		}
	}
#endif
	// not found:
	return strDef;
}

// 
// Check if two time names define the same time.
//
// Handles case where one is student-defined time interval variable name and
// the other is one of our predefined time interval names like "TO to T1".
// Shouldn't be possible for both names to be time interval variables, since
// we don't allow two variables for the same quantity to exist, and this
// routine is intended for use when comparing a candidate def with an existing one.
// 
BOOL CFBDDoc::IsMatchingTime(CString strTime1, CString strTime2)
{
	// check for exact match of time names
	if (strcmp(strTime1, strTime2) == 0) 
		return TRUE;

	// Search variable list in case one of the candidates is a time 
	// interval variable. If so, check if other is matching interval name.
	int posStartTime = -1;
	int posEndTime = -1;
	POSITION pos = theApp.GetDocument()->m_Variables.GetHeadPosition();
	while (pos != NULL)
	{
		CVariable* pVar = theApp.GetDocument()->m_Variables.GetNext(pos);
		// must be time interval var matching one of the names.
		if (pVar->m_nType != ID_VARIABLE_ADDTIME)
			continue;
		if ( (strcmp(strTime1, pVar->m_strName) != 0) && 
				(strcmp(strTime2, pVar->m_strName) != 0) )
			continue;
		
		// found one:
		// Test checks if variable's start and end times occur in order 
		// in other time name (our interval names look like "T0 to T1"). 
		if (strcmp(strTime1, pVar->m_strName) == 0)
		{
			posStartTime = strTime2.Find(pVar->m_strObject);
			posEndTime = strTime2.Find(pVar->m_strTime);
		}
		else if (strcmp(strTime2, pVar->m_strName) == 0)
		{
			posStartTime = strTime1.Find(pVar->m_strObject);
			posEndTime = strTime1.Find(pVar->m_strTime);
		}
		return (posStartTime != -1 && posEndTime != -1 && posStartTime < posEndTime);
	}
	
	return FALSE;
}

// return current max extent of drawing objects on page (approx).
CSize CFBDDoc::GetSize()
{
	CSize sizeMax(0, 0);
	// approximate calc of max x and max y in LUs,
    // doesn't account for width of lines on non-INSIDEFRAME objects
	POSITION pos = m_objects.GetHeadPosition();
    while (pos != NULL) {
		CDrawObj* pObj = m_objects.GetNext(pos);
		if (pObj == NULL) continue;
		CRect rcBounds = pObj->GetBoundingBox();
		rcBounds.NormalizeRect();
		if (rcBounds.right > sizeMax.cx)
			sizeMax.cx = rcBounds.right;
		if (rcBounds.bottom > sizeMax.cy)
            sizeMax.cy = rcBounds.bottom;
		
   }
   return sizeMax;
} 

// return size of hi-level plan display
CSize CFBDDoc::GetHiLevelPlanSize()
{
	CSize size = CSize(0, 0);
	POSITION pos = m_stages.GetHeadPosition();
	while (pos != NULL)
	{
		CStageObj* pStage = m_stages.GetNext(pos);
		CRect rect = pStage->GetPosition();

		if (size.cx < rect.right)
			size.cx = rect.right;
		if (size.cy < rect.bottom)
			size.cy = rect.bottom;
	}
	return size;

}
    
// Move given object to top or bottom of list. (Invalidates)
void CFBDDoc::MoveToBack(CDrawObj* pObj)
{
	CDrawObjList* pObjects = GetObjects();
	POSITION pos = pObjects->Find(pObj);
	ASSERT(pos != NULL);
	pObjects->RemoveAt(pos);
	pObjects->AddHead(pObj);

	pObj->Invalidate();
	SetModifiedFlag();
}
    
void CFBDDoc::MoveToFront(CDrawObj* pObj)
{
	CDrawObjList* pObjects = GetObjects();
	POSITION pos = pObjects->Find(pObj);
	ASSERT(pos != NULL);
	pObjects->RemoveAt(pos);
	pObjects->AddTail(pObj);

	pObj->Invalidate();
	SetModifiedFlag();
}

// move obj to bottom of student drawn objects, but above any problem objects.
// Use this to keep student-drawn object from being moved behind problem graphics.
// !!! If student is ever allowed to move any student-drawn object 
// behind problem graphics via MoveBack or MoveToBack command (currently true),
// then this operation also moves behind problem graphics. Need to control use
// of all ordering commands in student mode to maintain strict layering.
void CFBDDoc::MoveToBackStudent(CDrawObj* pObj)
{
	// unlink given object
	CDrawObjList* pObjects = GetObjects();
	POSITION pos = pObjects->Find(pObj);
	ASSERT(pos != NULL);
	pObjects->RemoveAt(pos);
	
	// Search forward to find pos of rearmost (first) student-drawn obj
	// in display list, NULL if none
	POSITION posRearmostStudentObj = NULL;
	pos = pObjects->GetHeadPosition();
	while (pos != NULL) {
		// NB: Don't use GetNext since that advances pos
		CDrawObj* pTemp = pObjects->GetAt(pos);
		if (pTemp->m_flag == STUDENT_OBJECT) {
			// found it:
			posRearmostStudentObj = pos;
			break; 
		} 
		// else advance pos to next object
		(void) pObjects->GetNext(pos);
	} 
	
	// insert given object just before rearmost obj, if exists
	if (posRearmostStudentObj) {
		pObjects->InsertBefore(posRearmostStudentObj, pObj);
	} else { // no student objects, just add at tail
		pObjects->AddTail(pObj);
	}
	//TRACE("\n\nDoc after MoveToBackStudent\n");
	//Dump(afxDump);

	pObj->Invalidate();
	SetModifiedFlag();
}

// misc helper functions:
    
int CFBDDoc::GetAxesDirection()
{
	POSITION posDoc = GetObjects()->GetTailPosition();
	while (posDoc != NULL){
		CDrawObj* pObj = GetObjects()->GetPrev(posDoc);
		if (pObj->m_flag == TEACHER_OBJECT)
			continue;
		if (pObj->IsKindOf(RUNTIME_CLASS(CAxes))){
			if (pObj->m_status != statusError)
				return ((CAxes*)pObj)->m_nDirection;
		}
	}
	return -1;
}
    
BOOL CFBDDoc::IsAxesDrawn()
{
	POSITION posDoc = GetObjects()->GetTailPosition();
	while (posDoc != NULL){
		CDrawObj* pObj = GetObjects()->GetPrev(posDoc);
		if (pObj->m_flag == TEACHER_OBJECT)
			continue;
		if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
			return TRUE;
	}
	return FALSE;
}

// return next unused Axes index, zero for first axis.
// always returns lowest unused index; ie. 
// if add 3 axes x, x1, and x2 then delete x1, x2 retains its index but
// 1 is used on next axes to be drawn.
int CFBDDoc::GetNextAxesIndex()
{
	// check each index in turn. Loop bound is just defense against infinite loop.
	for (int nIndex = 0; nIndex < 15; nIndex++) 
	{
		// search to see if index is in use by any student-drawn axes
		BOOL bIndexInUse = FALSE;
		POSITION posDoc = GetObjects()->GetTailPosition();
		while (posDoc != NULL){
			CDrawObj* pObj = GetObjects()->GetPrev(posDoc);
			if (pObj->m_flag != TEACHER_OBJECT &&
				pObj->IsKindOf(RUNTIME_CLASS(CAxes)) &&
				((CAxes*)pObj)->m_nIndex == nIndex) {
					bIndexInUse = TRUE;
					break;
			}		
		}

		if (!bIndexInUse)
			return nIndex;
	}

	ASSERT("Failed to find unused axes index");
	return -1;
}  

// Fill in caller's LOGFONT struct with spec of default text font
void CFBDDoc::GetDefaultFont(LOGFONT* plf)
{
	memset(plf, '\0', sizeof(*plf));
	// Choose 8 point Arial plain for Examples, 10 point Arial bold for problems
	// Note height in logfont is specified not in points, but in LUs
	strcpy(plf->lfFaceName, "Arial");
	plf->lfPitchAndFamily = VARIABLE_PITCH | FF_SWISS;

	// default can depend on the current document type
	CFBDDoc* pDoc = theApp.GetDocument();
	if (pDoc && pDoc->m_nProblemType == PROB_EXAMPLE) 
	{
		plf->lfWeight = FW_DONTCARE;
		plf->lfHeight = -MulDiv(8 /*pts */, nLUsPerInch, 72 /*PtsPerInch*/);
	}
	else{
		plf->lfWeight = FW_BOLD;
		plf->lfHeight = -MulDiv(10 /*pts */, nLUsPerInch, 72 /*PtsPerInch*/); 
	}
}

// Set up mapping mode on given DC to use document's logical unit coordinates for 
// drawing or metric information. Static, so don't need a document pointer to use.
void CFBDDoc::SetLogicalUnits(CDC *pDC)
{
	// Now set up to map logical units to this device:
	pDC->SetMapMode(MM_ANISOTROPIC);
	pDC->SetViewportExt(pDC->GetDeviceCaps(LOGPIXELSX),
						pDC->GetDeviceCaps(LOGPIXELSY));
	pDC->SetWindowExt(nLUsPerInch, nLUsPerInch);
}


/////////////////////////////////////////////////////////////////////////////
// CFBDDoc diagnostics
    
#ifdef _DEBUG
void CFBDDoc::AssertValid() const
{
	COleDocument::AssertValid();
    	// !!! Check each object for validity
}
    
void CFBDDoc::Dump(CDumpContext& dc) const
{
	COleDocument::Dump(dc);
	// Dump parameters:
	dc << "Problem Id: " << m_strProblemId << "\n"
		<< "Problem Type: " << m_nProblemType << "\n"
		<< "File Version: " << m_nVersion << "\n"
		<< "Objects: " << m_strObjects << "\n"
		<< "Times: " << m_strTimes << "\n" 
		;
	// Dump objects:
	dc << m_objects.GetCount() << " Objects:\n";
	POSITION pos = m_objects.GetHeadPosition();
	int nObj = 0;
	while (pos != NULL)
	{
		CDrawObj* pObj = m_objects.GetNext(pos);
		dc << "\n[" << ++nObj << "] ";
		if (pObj == NULL)
			dc << "NULL item in object list!!!\n" ;
		else 
			pObj->Dump(dc);
	}
	// Dump non-empty equations, if any
	for (int i = 0; i < NEQS; i++)
		if (! m_strEq[i].IsEmpty() )
		dc << "[EQ" << i << "] " << m_strEq[i] 
			<< " Status: " << (int) m_statusEq[i] << "\n";
}
#endif //_DEBUG
    
/////////////////////////////////////////////////////////////////////////////
// CFBDDoc commands
    
//////////////////////////////////////////////////////////////////////////////
// Problem authoring support
//////////////////////////////////////////////////////////////////////////////

BOOL CFBDDoc::OnNewDocument()
{
   	// Make sure base class can do its initialization
	if (!COleDocument::OnNewDocument())
   		return FALSE;
   
	// For new problem authors: run the new problem wizard
	// to collect specification and layout problem
	if (theApp.m_bAuthorMode) {
		ImportProblem();
		// OnEditProperties();
		// LayoutProblem();
	}
	return TRUE;
}

//-------------------------------------------------------------------------------
// Support for automatically laying out the problem page from a specification
// that includes the problem statement and graphic file. 
//-------------------------------------------------------------------------------
//
// The problem statement string is a series of lines with the positions of answer
// boxes indicated by brackets. Ex. statement spec:
//
//       A cannonball is shot at an angle of ....\r\n
//       When are the x and y components when it hits?\r\n
//           x component: [           ]  y:component [         ]\r\n
//       What is its horizontal range?\r\n
//           Answer: [            ]
// 
// We convert these to text pieces and answer box marker rectangles. 
// The problem graphic is imported from a specified file.
// We call the graphic items generated here "autolayout" items.  We identify
// them by distinctive item IDs, Stmt-N, Graphic-N and Answer-N.

// String utility function for perl-style split into fields:

// split a string into fields at separator string. Return includes empty fields
int split(const CString& str, const CString& Sep, CStringArray& result)
{
	// Empty any contents from result
	result.RemoveAll();

	CString strRest(str);			// remainder to be processed
	int iSep;						// position of next separator start
	int lenSep = Sep.GetLength();	// length of separator (constant)
	while ((iSep = strRest.Find(Sep)) != -1) 
	{
		result.Add(strRest.Mid(0, iSep));
		// if str ends at this separator, include trailing empty field
		if ((iSep + lenSep) == strRest.GetLength()) {
			result.Add("");
		}
		strRest = strRest.Mid(iSep + lenSep); 
	}
	if (strRest.GetLength() > 0) 
		{ result.Add(strRest); }

	return result.GetSize();
}

// Remove any existing autolayout items.
// Does nothing if problem not created by autolayout.
void CFBDDoc::RemoveLayoutItems()
{
	BOOL bHaveAutoLayout = FALSE;
	for (POSITION pos=m_objects.GetHeadPosition(); pos;) {
		CDrawObj* pObj = m_objects.GetNext(pos);
		if (pObj->m_strId.Left(strlen("Stmt-")) == "Stmt-"
			|| pObj->m_strId.Left(strlen("Graphic")) == "Graphic") {
			pObj->Invalidate();
			Remove(pObj);
			pObj->Delete();
			bHaveAutoLayout = TRUE;
		}
		else if ((pObj->m_strId.Left(strlen("Answer-")) == "Answer-") 
			&& bHaveAutoLayout) {
			pObj->Invalidate();
			Remove(pObj);
			pObj->Delete();
		}
		
	}
}

// Layout the problem statement items from given statement spec.
// returns bounding box of problem statement.
// returns NULL rectangle for a bad problem.
CRect CFBDDoc::LayoutStatement(const CString& strStatement)
{
	// split source into array of lines:
	CStringArray strLines;
	int nLines = split(strStatement, "\r\n", strLines);  

	// starting pos for layout with small margin:
	const int xLeftMargin = 6;
	const int yTopMargin = 6;

	// running coords for next item to layout:
	int xPos = xLeftMargin; int yPos = yTopMargin;
	int xMax = xLeftMargin;   // remember max width
	// counter for answer boxes and text pieces
	int nAnswers = 0;
	int nText = 0;
	int nChoices = 0;
	// start and end position of answer box markers:
	int iAnsStart, iAnsEnd;

	// Use screen DC to get height of a line of text
	TEXTMETRIC tm;
	CClientDC dc(NULL);
	SetLogicalUnits(&dc);
	// need to use the document font for metrics
	LOGFONT lfText;
	GetDefaultFont(&lfText);
	CFont fontText;
	fontText.CreateFontIndirect(&lfText);
	CFont* pOldFont = dc.SelectObject(&fontText);
	dc.GetTextMetrics(&tm);
	int nHeight = tm.tmHeight + tm.tmExternalLeading;
	
	// do for each source line in statement spec:
	for (int i = 0; i < nLines; i++) 
	{
		// if an empty lines, just advance y position
		if (strLines[i].IsEmpty()) {
			yPos += nHeight;
			continue;
		}

		// if it has no answer boxes, just lay out the line:
		if (strLines[i].Find("[") == -1) {
			 // Plain line: no answer box
			// Create a new text piece and add it to document
			CLabel* pText = new CLabel(CPoint(xPos, yPos), strLines[i]);
			pText->m_strId.Format("Stmt-%d", ++nText);
			Add(pText, FALSE); pText->m_flag = TEACHER_OBJECT;
			pText->Invalidate();
			xPos += pText->m_position.Width();
			if (xPos > xMax) xMax = xPos;
			
			// and we're done with this source line
			xPos = xLeftMargin;
			yPos += nHeight;
			continue;
		}

		// else process line containing one or more answer boxes:
		CGroup* pGroup = NULL;
		int nGroups = 0;
		
		//  skip a line to accomodate box height, unless preceding line is already blank:
		if (! (i>0 && strLines[i-1].IsEmpty())) // !!could have spaces, should test for printing char
				yPos += nHeight;

		// do for each answer box found remaining in line:
		// if no closing bracket found later on line, it doesn't count as answer box.
		CString strRemain = strLines[i];
		while ((iAnsStart=strRemain.Find("[")) != -1
			   && (iAnsEnd=strRemain.Find("]", iAnsStart)) != -1)
		{
			CString strPrefix = strRemain.Left(iAnsStart);
			// Extract answer box marker text string. Its size will determine size of box
			CString strAnswerSize = strRemain.Mid(iAnsStart, iAnsEnd-iAnsStart);
				
			// if see a "{" before answer box, we start a multiple-choice question group. 
			// note can span multiple lines. 
			if (strPrefix.Find("{") != -1) {
				pGroup = new CGroup();
				pGroup->m_strId.Format("MC-%d", ++nAnswers); // whole group is one answer
				Add(pGroup, FALSE); pGroup->m_flag = TEACHER_OBJECT;
				strPrefix.Delete(strPrefix.Find("{"));
			}
			// if see a "}" before answer box, finish current choice group, if there is one
			if (pGroup && strPrefix.Find("}") != -1) {
				pGroup = NULL;
				strPrefix.Delete(strPrefix.Find("}"));
			}

			// Create text piece for prefix. We use Stmt-N id so we can find and
			// remove autolayout items if author edits statement spec later
			CLabel *pText = new CLabel(CPoint(xPos, yPos), strPrefix);
			pText->m_strId.Format("Stmt-%d", ++nText);
			Add(pText, /*bGenId*/FALSE); pText->m_flag = TEACHER_OBJECT;
			pText->Invalidate();
			// update current x position for next layout item
			xPos += pText->m_position.Width();
		
			// Figure out width of answer box from text width. Height adjusted
			// to extend above and below text line (nice values gotten by experiment)
			const int cyAnswerAbove = 6; const int cyAnswerBelow = 8;
			CSize sizeAnswer = dc.GetTextExtent(strAnswerSize);
			sizeAnswer.cy += cyAnswerAbove + cyAnswerBelow;

			// Create an answer box marker to replace this text. 
			// Offset top up by by a half a line
			CRect rcAnswer(CPoint(xPos, yPos - cyAnswerAbove), sizeAnswer); 
	
			// If marker text begins with [__ create a multiple choice button
			if (strAnswerSize[1] == '_') {
				CChoice* pChoice = new CChoice(rcAnswer);
				strAnswerSize.Replace("[__", "");
				pChoice->m_strName = strAnswerSize;
				pChoice->m_flag = TEACHER_OBJECT;
				// if there's a group, add as choice under that group
				if (pGroup) {
					pChoice->m_strId.Format("Choice-%d", ++nChoices);
					pGroup->AddObj(pChoice);			
				} else { // add a free-standing answer button (probably DONE button)
					pChoice->m_strId.Format("Answer-%d", ++nAnswers);
					Add(pChoice, /*bGenId*/ FALSE); 
				}
				pChoice->Invalidate();
				xPos+= pChoice->m_position.Width();
			} else { // create an answer box rectangle
				CDrawRect* pRect = new CDrawRect(CDrawRect::rectangle, rcAnswer);
				pRect->m_bBrush = FALSE;  // no brush -> unfilled 
				pRect->m_strId.Format("Answer-%d", ++nAnswers);		
				Add(pRect, /*bGenId*/ FALSE); pRect->m_flag = TEACHER_OBJECT;
				pRect->Invalidate();
				xPos+= pRect->m_position.Width();
			}

			// update remainder, leaving empty string if no more
			if ((iAnsEnd + 1) < strRemain.GetLength()) // not at end of string
				strRemain = strRemain.Mid(iAnsEnd + 1);
			else strRemain.Empty();
		}
		
		// no more answer boxes left on this line:
		// but may be remaining text piece after last answer box

		// trailing text may just close a group
		if (pGroup && strRemain.Find("}") != -1) {
				pGroup = NULL;
				strRemain.Delete(strRemain.Find("}"));
		}
		// add any trailing text piece
		if (!strRemain.IsEmpty()) {
			CLabel* pText = new CLabel(CPoint(xPos, yPos), strRemain);
			pText->m_strId.Format("Stmt-%d", ++nText);
			Add(pText, FALSE); pText->m_flag = TEACHER_OBJECT;
			pText->Invalidate();
			xPos += pText->m_position.Width();
		}

		// Ok, finished pieces of answer box line: reset for next line in outer loop
		if (xPos > xMax) xMax = xPos;
		xPos = xLeftMargin;
		yPos += nHeight;
		// skip a line after answer line, unless we have a subsequent blank line 
		if (! (i+1 < nLines && strLines[i+1].IsEmpty()) )
			yPos += nHeight;
	}

	// add a 1-line border before graphic. We most likely skipped a line after answer box,
	// but not necessarily, some boxes followed by hints like "Ignore friction."
	yPos += 1*nHeight;

	// don't leave font selected in DC.
	dc.SelectObject(pOldFont);

	// If problem doesn't have answers, return failure code (NULL rectangle).
	// probably need an .fbd file for this problem
	if (nAnswers == 0)
		return CRect(0,0,0,0);
	// else:
	// return bounding rect of statement (including lower border).
	return CRect(xLeftMargin, yTopMargin, xMax, yPos);
}

// Layout the problem graphic from file with respect to given statement bounds.
// Parameters are full path name to graphic file and rect occupied by problem stmt.
// Reports any exception importing the graphic via CException::ReportError 
void CFBDDoc::LayoutGraphic(const CString &strPathName, const CRect &rcStmt)
{
	CDrawPicture* pPic = NULL;
	// we want to scale big images to ensure it doesn't extend past maxWidth
	// a little over half the page on our printout
	const int maxWidth = 480; // 5 inch * 96 LUsPerInch

	try {
		pPic = CDrawPicture::CreateFromFile(strPathName);
	} catch (CException* pEx) {
		pEx->ReportError();
		pEx->Delete();
		return;
	}
	if (pPic == NULL) return;  // failure that didn't throw exception?

	// Picture must be flagged as read-only problem object -- doesn't happen by default
	// because not added in author mode.
	pPic->m_flag = TEACHER_OBJECT;

	// new picture's m_position should be (0,0,width,height)
	ASSERT(pPic->m_position.left == 0);
	ASSERT(pPic->m_position.top == 0);

	// if it's less wide than the statement, just center it beneath statement.
	// This doesn't ensure the statement fits within maxWidth, just assume statement lines
	// have been constructed at a reasonable width
	if (pPic->m_position.Width() <= rcStmt.Width()) {
		pPic->m_position += CSize((rcStmt.Width() - pPic->m_position.Width())/2, rcStmt.Height());
	}
	else // else wider than statement: center within maxwidth
	{ 	
		// scale uniformly if needed
		if (pPic->m_position.Width() > maxWidth) {
			pPic->m_position.right =  maxWidth;
			pPic->m_position.bottom = (maxWidth/pPic->m_position.Width())*pPic->m_position.bottom;
		}
		pPic->m_position += CSize((maxWidth - pPic->m_position.Width())/2, rcStmt.Height());
	}

	// Give it distinguished ID so we can recognize that it's an autolayout item
	pPic->m_strId = "Graphic"; // only one graphic, so no number needed in ID
	Add(pPic, /*bGenId=*/FALSE);
	pPic->Invalidate();
}

// Main routine to layout a problem from the spec info in member variables.
BOOL CFBDDoc::LayoutProblem()
{
	// Clear existing layout first, in case we are doing it again.
	RemoveLayoutItems();

	// Layout the statement and verify it was well-formed
	CRect rcStmt = LayoutStatement(m_strStatement);
	if (rcStmt.IsRectNull())
		return FALSE;

	// Layout the graphic if an external graphic file is specified
	if (!m_strGraphicFile.IsEmpty()) 
	{
		// if running in OLI mode, must download the graphic file. Problem set knows how
		// to do this.
		CProblemSet* pSet = theApp.GetProblemSet();
		if (pSet && pSet->m_bOli) {
			pSet->GetProblemGraphic(m_strGraphicFile);
		}
		// Form full path. We expect to find graphic file in Andes problem directory
		CString strPathName = g_strAndesDir + g_szProblemDir + "\\" + m_strGraphicFile;
		LayoutGraphic(strPathName, rcStmt);
	}
	return TRUE;
}


    
//
// Edit Document properties with property sheet. 
// Authormode command to define problem properties.
//
void CFBDDoc::OnEditProperties()
{
	// Assemble the doc property sheet from component pages
	CPropertySheet sheet( _T("Problem properties") );
	CGeneralPage pageGeneral;
	CStatementPage pageStatement;
	CObjectsPage pageObjects;
	CTimesPage	 pageTimes;
	CPositionsPage pagePositions;	// holds points for circuits
	CBranchesPage pageBranches;		// holds branches for circuits
	// CHelpFilesPage pageHelpFiles;

	sheet.AddPage(&pageGeneral);
	sheet.AddPage(&pageStatement);
	sheet.AddPage(&pageObjects);
	sheet.AddPage(&pageTimes);
	sheet.AddPage(&pagePositions);
	sheet.AddPage(&pageBranches);
	// sheet.AddPage(&pageHelpFiles);

	// Initial simple value properties from document. 
	// List-valued properties are handled in the property page classes using
	// custom DDX functions for relevant lists.
	pageGeneral.m_strProblemId = m_strProblemId;
	pageGeneral.m_nProblemType = m_nProblemType;
	pageGeneral.m_nKBType = m_nKBType;
	pageGeneral.m_nAssessor = m_nAssessor;
	pageGeneral.m_bIncludePlan = m_bIncludePlan;
	// info for new problem wizard:
	pageStatement.m_strStatement = m_strStatement;
	pageStatement.m_strGraphicFile = m_strGraphicFile;

	// run the sheet
	// sheet.SetWizardMode();	// means always runs as a wizard
	if (sheet.DoModal() != IDOK)
		return;
	SetModifiedFlag();

	// Save new simple value properties
	// List valued properties were updated by the property page classes
	// using custom DDX functions for relevant lists.
	m_strProblemId = pageGeneral.m_strProblemId;
	m_nProblemType = pageGeneral.m_nProblemType;
	m_nKBType = pageGeneral.m_nKBType;
	m_nAssessor = pageGeneral.m_nAssessor;
	m_bIncludePlan = pageGeneral.m_bIncludePlan;
	// spec info for new problem wizard
	m_strStatement =	pageStatement.m_strStatement;
	m_strGraphicFile = pageStatement.m_strGraphicFile;
	// Need to sync features list with possibly-updated concept flags
	// set while editing document. A pain because might have some feature-sets from an initial
	// .prb import that aren't in the set shown on property page, so we shouldn't just reset
	// the list to zero and add. Following routine only adds features, doesn't take them out if
	// they've been removed. Should change everything to use a single set representation throughout
	// to clean this up.
	AddConceptsToFeatures();

	// re-layout problem from spec
	LayoutProblem(); 
}

//
// Set problem attributes from a Lisp-format .prb file
// Note this is a kind of constructor.
//

void CFBDDoc::ImportProblem()
{
	// Prompt for a .prb filename
	CFileDialog dlg(TRUE, "prb", NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "Andes Physics Problem (.prb)|*.prb||");
    // Start the dialog in Problems directory.
	CString strDir = g_strAndesDir + g_szProblemDir;
	dlg.m_ofn.lpstrInitialDir = strDir;
    if (dlg.DoModal() != IDOK) 
	{
		// if they cancelled, let them enter properties by hand:
		OnEditProperties();
    	return;
	}
    
	// else import properties from prb file
    CString strPathName = dlg.GetPathName();
	BOOL bReadOK = LoadFromPrb(strPathName);
	if (! bReadOK) {
		theApp.DoWarningMessage("Failed to load " + strPathName);
	}
}

// Following table maps kb problem features to the workbench
// concept flags they entail. These features already in use, they
// don't  exactly match up with the friendly concept flag names shown 
// in the feature list of the problem properties general page ("concepts" table
// near CGeneralPage code).
const struct fInfo {
	char* szName;		// help system feature name
	int   wConcepts;   // concept flags set for these (could be or'd)
}
featureMap[] = 
{
	// For inverse mapping from flag to string, only first string is used
	"vector-grid",ID_PROB_VECTOR,
	"kinematics", ID_PROB_KINEMATICS,
	"dynamics",   ID_PROB_FORCE,
	"statics",    ID_PROB_FORCE,
	"circular",   ID_PROB_CIRCMOTION,
	"gravitation",ID_PROB_GRAVITATION, 
	"energy",     ID_PROB_ENERGY,
	"work",       ID_PROB_WORK,	
	"linmom",     ID_PROB_MOMENTUM,
	"rotkin",     ID_PROB_ROTKINEMATICS,
	"angmom",     ID_PROB_ROTKINEMATICS,
	"torque",     ID_PROB_ROTKINEMATICS,
	"fluids",     ID_PROB_FLUIDS,
	"circuits",   ID_PROB_CIRCUITS,
	"E&M",        ID_PROB_EM,
	"optics",     ID_PROB_OPTICS,
	"relvel",     ID_PROB_RELVEL,
	"probability", ID_PROB_PROBABILITY,
	"changing-voltage", ID_PROB_CHANGING_VOLTAGE,
};
const int nFeatures ARRAY_SIZE(featureMap);

// Add features from (old) wConcepts feature bit vector to (new) feature stringlist
// To be used on old-style fbd-based problems where bit vector was set in 
// property dialog. Need to also set new stringlist representation (used for .prb-based
// problems) since variable menu construction code now uses string-list sets.
// NB: feature bit vector continues to be used, and is also set from .prb files.
void CFBDDoc::AddConceptsToFeatures()
{
	int wConcepts = m_wConcept; // temp copy
	for (int i = 0; i < nFeatures; i++) {
		if (wConcepts & featureMap[i].wConcepts) {
			// only add string if it's not already there. (Might it be there
			// with a different case, say from .prb import?)
			if (! m_strFeatures.Find(featureMap[i].szName))
				m_strFeatures.AddTail(featureMap[i].szName);
			// remove this flag so don't add a second string for it
			wConcepts &= ~(featureMap[i].wConcepts);
		}
	}
}

// Set problem attributes from given .prb file (full path).
// Returns NULL on any error -- doesn't throw exceptions or report!
BOOL CFBDDoc::LoadFromPrb(LPCSTR pszFileName)
{
   // First read the attributes we need from beginning of file
	FILE* fp = fopen(pszFileName, "r");
	if (! fp) return NULL;
	/// !!! All return FALSES don't close file !!

	// Use the Lisp tokenizer
	CLispReader lr(fp);
	// read the header
	CString strToken = lr.GetToken();
	if (strToken != "<Andes2Problem>") {
		// throw error
		return FALSE;
	}
	// now read sequence of tag-value pairs, picking out the attributes we need
	// until we have them all or we know we are past them.
	for (;;) {
		CLispReader::Obj* pTag, *pValue;
		pTag = lr.GetObject();
		pValue = lr.GetObject();
		
		if (!pTag->IsAtom()) return FALSE;
		CString strTag = ((CLispReader::Atom*)pTag)->m_strValue;
		if (strTag.CompareNoCase("Name") == 0) {
			// set problem name
			m_strProblemId = ((CLispReader::Atom*)pValue)->m_strValue;
		}
		else if (strTag.CompareNoCase("Statement") == 0) 
		{
			// set statement from list. It's a list of strings
			if (! pValue->IsList()) return FALSE;
			CLispReader::List* pList = (CLispReader::List*) pValue;
			// run through list to set statement.
			POSITION pos = pList->m_objects.GetHeadPosition();
			while (pos) {
				CLispReader::Obj* pStr = pList->m_objects.GetNext(pos);
				if (!pStr->IsAtom()) return FALSE;
				
				// append current string to statement
				if (! m_strStatement.IsEmpty())
					m_strStatement += "\r\n";
				m_strStatement += ((CLispReader::Atom*)pStr)->m_strValue;
			}
		}
		else if (strTag.CompareNoCase("Times") == 0) 
		{
			// check for NONE => no times
			if (pValue->IsAtom() && ! ((CLispReader::Atom*)pValue)->m_strValue.CompareNoCase("NONE")) 
				continue;

			// check for NIL => single default time
			if (pValue->IsAtom() && ! ((CLispReader::Atom*)pValue)->m_strValue.CompareNoCase("NIL")) {
				m_strTimes.AddHead("T0 = the instant depicted");
				continue;
			}
			
			// else expect a list of form 
			// ((1 "elevator at 10 m/s") (2 "elevator at a stop") (during 1 2))
			if (! pValue->IsList()) return FALSE;
			CLispReader::List* pList = (CLispReader::List*) pValue;
			POSITION pos = pList->m_objects.GetHeadPosition();
			while (pos) 
			{
				CLispReader::Obj* pItem = pList->m_objects.GetNext(pos);
				if (!pItem->IsList()) return FALSE;
				CLispReader::List* pPair = (CLispReader::List*) pItem;
				// pull first part
				POSITION posFirst = pPair->m_objects.FindIndex(0);
				CLispReader::Obj* pAtom = pPair->m_objects.GetAt(posFirst);
				if (! pAtom->IsAtom()) return FALSE;
				CString strFirst = ((CLispReader::Atom*)pAtom)->m_strValue;
				// pull second part. 
			    POSITION posSecond = pPair->m_objects.FindIndex(1);
			    pAtom = pPair->m_objects.GetAt(posSecond);
				CString strSecond = ((CLispReader::Atom*) pAtom)->m_strValue;

				// NB. our times start at T0; help times normally start at 1
				CString strSpec;
				if (strFirst.CompareNoCase("DURING") != 0) { // time point
					int nFirst = atoi(strFirst); 
					strSpec.Format("T%d = %s", nFirst-1, strSecond);
				} else { // time interval -- use second and third
					POSITION posThird = pPair->m_objects.FindIndex(2);
					pAtom = pPair->m_objects.GetAt(posThird);
					CString strThird = ((CLispReader::Atom*) pAtom)->m_strValue;
					int nSecond = atoi(strSecond); int nThird = atoi(strThird);
					strSpec.Format ("T%d to T%d", nSecond-1, nThird-1);
					// check for optional description of interval:
					POSITION posFourth = pPair->m_objects.FindIndex(3);
					if (posFourth) {
						pAtom = pPair->m_objects.GetAt(posFourth);
						CString strFourth = ((CLispReader::Atom*) pAtom)->m_strValue;
						strSpec += " = " + strFourth;
					}
				}
				m_strTimes.AddTail(strSpec);
			}
			continue;
		}
		else if (strTag.CompareNoCase("Choices") == 0) 
		{
			// ex: ((BODIES (PACKAGE EARTH STRING))
			//      (BRANCHES (BR1 BR2 BR3))
			// Can be NIL if no choices (qual question):
			if (pValue->IsAtom() && ! ((CLispReader::Atom*)pValue)->m_strValue.CompareNoCase("NIL")) {
				continue;
			}
			if (! pValue->IsList()) return FALSE;
			CLispReader::List* pList = (CLispReader::List*) pValue;
			POSITION pos = pList->m_objects.GetHeadPosition();
			while (pos) 
			{
				CLispReader::Obj* pItem = pList->m_objects.GetNext(pos);
				if (!pItem->IsList()) return FALSE;
				CLispReader::List* pPair = (CLispReader::List*) pItem;
				
				// pull string from first part (atom)
				POSITION posFirst = pPair->m_objects.FindIndex(0);
				CLispReader::Obj* pAtom = pPair->m_objects.GetAt(posFirst);
				if (! pAtom->IsAtom()) return FALSE;
				CString strFirst = ((CLispReader::Atom*)pAtom)->m_strValue;
				
				// pull second part as list
			    POSITION posSecond = pPair->m_objects.FindIndex(1);
			    pItem = pPair->m_objects.GetAt(posSecond);
				if (!pItem->IsList()) return FALSE;
				CLispReader::List* pChoiceList = ((CLispReader::List*) pItem);
				// add choices from list
				POSITION pos1 = pChoiceList->m_objects.GetHeadPosition();
				while (pos1) {
					CLispReader::Obj* pChoiceObj = pChoiceList->m_objects.GetNext(pos1);
					if (! pChoiceObj->IsAtom()) return FALSE;
					CString strChoice = ((CLispReader::Atom*)pChoiceObj)->m_strValue;

					// add it to the choice we need
					if (strFirst.CompareNoCase("Bodies") == 0) {
						// undo LISP print's upper casing. But not for R1, C1, etc.
						if (! (strChoice.GetLength() == 2 && isdigit(strChoice[1]))) // cheap heuristic
							strChoice.MakeLower();
						m_strObjects.AddTail(strChoice);
					}
					if (strFirst.CompareNoCase("Branches") == 0) {
						m_strBranches.AddTail(strChoice);
					}
					if (strFirst.CompareNoCase("Positions") == 0) {
						// undo LISP print's upper casing
						strChoice.MakeLower();
						m_strPositions.AddTail(strChoice);
					}
				}
            }
			continue;
		}
		else if (strTag.CompareNoCase("Graphic") == 0) 
		{
			// spec just has base name, not full path
			// Layout problem expects to find it in Problems Folder
			// check for NIL => no graphic
			if (pValue->IsAtom() && ((CLispReader::Atom*)pValue)->m_strValue.CompareNoCase("NIL") != 0) {	
				m_strGraphicFile = ((CLispReader::Atom*)pValue)->m_strValue;
			}
			continue;
		}
		else if (strTag.CompareNoCase("Features") == 0) 
		{
			// clobber default (kinematics|force) if problem specifies features.
			m_wConcept = 0x00;
			// copy feature list items into m_strFeatures member
			if (! pValue->IsList()) return FALSE;  // !! could be NIL
			CLispReader::List* pList = (CLispReader::List*) pValue;
			POSITION pos = pList->m_objects.GetHeadPosition();
			while (pos) {
				CLispReader::Obj* pStr = pList->m_objects.GetNext(pos);
				if (!pStr->IsAtom()) return FALSE;
				CString strFeature = ((CLispReader::Atom*)pStr)->m_strValue;
				m_strFeatures.AddTail(strFeature);

				// use map to set concept bit flags for known features 
				for (int i = 0; i < nFeatures; i++) {
					if (strFeature.CompareNoCase(featureMap[i].szName) == 0)
						m_wConcept |= featureMap[i].wConcepts;
				}

				// check for purely qualitative problem.
				if (strFeature.CompareNoCase("no-quant") == 0)
					m_nProblemType = PROB_QUAL;
			}
		}
		if (strTag.CompareNoCase("Soughts") == 0 ||
			strTag.CompareNoCase("Givens") == 0 ||
			strTag.CompareNoCase("WorkingMemory") == 0) {
			// we've gone beyond what we need in the attribute list
			break;
		}

	}

	// make sure we have all we need.
	fclose(fp);

	// Layout the Problem from the spec, verify it is well-formed:
	if (! LayoutProblem())
		return FALSE;

	// The dirty bit will have been set on adding graphic to file. In student mode, clear it,
	// so no prompt to save if no further changes. For authors, leave it, they have created a new
	// fbd file from a prb and may want to save it (can always cancel anyway).
	if (!theApp.m_bAuthorMode)
		SetModifiedFlag(FALSE);
		
	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////////
//
// Methods for Managing list of principles for hi level solution
//
/////////////////////////////////////////////////////////////////////////////////
void CFBDDoc::AddPrinciple(CPrincObj* pPrinc) 
{
	// First add the stage to generate the stage number
	AddStage(pPrinc->m_pStageObj);

	// Principle's id can be derived from the stage number as needed (GetStageId)
	// so no need to generate one.

	//Add to principle list
	m_principles.AddTail(pPrinc);
	pPrinc->m_pDocument = this;
	
	SetModifiedFlag();
    return;
}

void CFBDDoc::RemovePrinciple(CPrincObj* pPrinc)
{
	POSITION pos = m_principles.Find(pPrinc);
	m_principles.RemoveAt(pos);//Delete from list of variables
	RemoveStage(pPrinc->m_pStageObj);
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////////
//
// Methods for Managing list of stages for high level solution
//
/////////////////////////////////////////////////////////////////////////////////
void CFBDDoc::AddStage(CStageObj* pStage) 
{
	GenerateId(pStage);//generate unique ID

	//Add new plan stage to  list, setting back pointer.
	m_stages.AddTail(pStage);
	pStage->m_pDocument = this;

	// Fill in initial blank template for stage. Note must do *after* 
	// set back pointer, so elements can get it from stage.
	pStage->AddTemplate();

	SetModifiedFlag();
    	
	return;
}

void CFBDDoc::RemoveStage(CStageObj* pStage)
{
	POSITION pos = m_stages.Find(pStage);
	m_stages.RemoveAt(pos);//Delete from list of variables
	SetModifiedFlag(TRUE);
}

/////////////////////////////////////////////////////////////////////////////////
//
// Methods for Managing list of variable definitions
//
/////////////////////////////////////////////////////////////////////////////////
void CFBDDoc::AddVariable(CVariable* pVar) 
{
	GenerateId(pVar);//generate unique ID
	m_Variables.AddTail(pVar);//Add new variable to variable list
	pVar->m_pDocument = this;
	SetModifiedFlag();
    	
	return;
}

void CFBDDoc::RemoveVariable(CVariable* pVar)
{
	POSITION pos = m_Variables.Find(pVar);
	m_Variables.RemoveAt(pos);//Delete from list of variables
	SetModifiedFlag(TRUE);
}


/////////////////////////////////////////////////////////////////////////
// Help system data files optionally embedded in problem document file. 
// We put these as custom DocItem derivatives on the doc's DocItem list.
// (That list also holds embedded OLE objects in COleClientItems. but we
// are not using OLE for this.)
// CHelpData object is just a header wrapping a block of opaque data.
/////////////////////////////////////////////////////////////////////////

IMPLEMENT_SERIAL(CHelpData, CDocItem, VERSIONABLE_SCHEMA | 2)

CHelpData::CHelpData()
	:CDocItem()
{
	// create as empty item
	m_cbLength = 0;
	m_pData = NULL;
}

// Framework will not serialize if blank.
CHelpData::IsBlank()
{
	return (m_pData == NULL) || (m_cbLength == 0); 
}

void CHelpData::FreeData()
{
	if (m_pData) { 		// safe if unset.
		free (m_pData);
		m_pData = NULL;
		m_cbLength = 0;
	}
}

CHelpData::~CHelpData() 
{
	FreeData();
	m_pDocument = NULL;  // unlink from doc. Required for ~CDocItem.
}

void CHelpData::Serialize(CArchive& ar)
{
	UINT nVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;

	CDocItem::Serialize(ar); // sets document ptr on loading
    
	if (ar.IsStoring()) 
	{  
		ar << m_strFileName;
		ar << m_cbLength;
		// added version two: mod time
		ar << m_timeModified;
		
		// write the data from memory
		if (m_cbLength > 0)   // should always be true if IsBlank is used
		{
			ASSERT(m_pData);
			ar.Write (m_pData, m_cbLength);
		}
	} 
	else // Loading
	{ 
		ar >> m_strFileName;
		ar >> m_cbLength;
		if (nVersion >= 2)
			ar >> m_timeModified;
		else
			m_timeModified = 0; // doesn't matter, version 1 soon obsolete.

		// allocate block and gulp data into it
		if (m_cbLength > 0) { // should always be true since blank items not saved
			FreeData();
			m_pData = malloc(m_cbLength);
			int nRead = ar.Read(m_pData, m_cbLength);
			ASSERT(nRead == m_cbLength);
		
			// On loading, when help system is connected, we copy data out into file in
			// the help system's run-time PData directory. Note this overwrites every time,
			// so authors may need to ensure src directory is different.
			if (HelpSystemIsConnected())
				ExportFile();

			// Although we don't use it in any way, we keep data block in memory for 
			// simplicity when saving doc back out. 
		}
	}
}

void CHelpData::ExportFile()	// copy data out into file in help system's runtime dir
{
	// Ensure problem directory exists.
	CFBDDoc* pDoc = (CFBDDoc*) GetDocument();  // DocItem member gets containing doc
	CString strProblemDir = g_strAndesDir + "Pdata\\" + pDoc->m_strProblemId;
	CFileStatus status;
	BOOL bExists = CFile::GetStatus(strProblemDir, status);
	if (! bExists || ! (status.m_attribute & CFile::directory)) {
			// create it. 
			CreateDirectory(strProblemDir, NULL);
			// not much we can do if failed, helpsys will fail if needs file.
	}
			
	CString strPath = strProblemDir + "\\" + m_strFileName;
	try {
		CFile file(strPath, CFile::modeWrite | CFile::modeCreate | 
		                        CFile::shareDenyWrite);
		file.Write(m_pData, m_cbLength);
		file.Close();
	} catch (CFileException* pEx) {
		pEx->ReportError();
		pEx->Delete();
	}
}

void CHelpData::SetData(LPCSTR pszPathName) //  set/reset from given file
{
	// Should check filename against currently stored one? Currently our caller does 
	// this when fetching/creating this object to update.
	CFile fileSrc(pszPathName, CFile::modeRead | CFile::shareDenyWrite);
	int cbLength = fileSrc.GetLength();
	FreeData();
	m_pData = malloc(cbLength);
	int nRead = fileSrc.Read(m_pData, cbLength);
	ASSERT(nRead == cbLength);
	m_cbLength = nRead;

	// also save modified time, so can tell author when needs update.
	CFileStatus statSrc;
	fileSrc.GetStatus(statSrc);
	m_timeModified = statSrc.m_mtime;

	fileSrc.Close();

	// doc now changed
	GetDocument()->SetModifiedFlag(); 
}

// Doc method: find help data item in doc by filename
CHelpData* CFBDDoc::GetHelpData(LPCTSTR pszName)
{
	POSITION pos = GetStartPosition();
	while( pos != NULL ){
		CDocItem* pItem = GetNextItem( pos );
		if (pItem->IsKindOf(RUNTIME_CLASS(CHelpData))) {
			CHelpData* pHelpItem = (CHelpData*) pItem;
			if (pHelpItem->m_strFileName.CompareNoCase(pszName) == 0)
			return  pHelpItem;
		}
	}
	return NULL; // not found
}






























