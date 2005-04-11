/////////////////////////////////////////////////////////////////////////////
// FBDDoc.h : interface of the CFBDDoc class
// 
/////////////////////////////////////////////////////////////////////////////
#ifndef FBDDOC_INCLUDED
#define FBDDOC_INCLUDED 1

//////////////////////////////////////////////////////////////////////////////////
// CFBDDoc -- Class for main document representation
//
// We use a single document class to represent both a problem and the student solution.
// The document is initialized to contain items for a problem description and graphics by 
// the  instructor, and edited to add the components of a (possibly only partial) solution 
// by the students. It consists mainly of some parameters, a list of drawing objects 
// (including text objects). the set of student equations managed by the Equation 
// view, the set of variables, and, optionally the solution plan managed by the plan view.
//
// The drawing portion of the document is currently represented by a flat 
// list of objects, with no distinguished substructure (outside of groups). However 
// drawn objects carry a flag saying whether they were created in author or student mode.
// (If you're not part of the solution, you're part of the problem.)
//
// The same document class is currently also used for examples, although some 
// attributes only have significance when it is shown in the example viewing mode. 
// Examples should probably be a different document type. However, current design lets 
// us easily use our diagram view with same graphics for both problems and examples.
// 
//////////////////////////////////////////////////////////////////////////////////

class CDrawObj;		// Generic diagram drawing object class
class CHelpData;	// Embedded Help system data file

enum Status			// entry status type shared with views
{ 
	statusUnknown, 
	statusCorrect,
	statusError, 
};

enum WorkState {	// total state of student solution
	workNone,			// no entries made
	workPartial,		// at least one entry made
	workCompleted,		// all answers correctly filled in
	workUnknown,		// status not avaiable.
}; 

// Bring in plan object defs here 
#include "StageObj.h"
#include "PlanObj.h"


typedef CTypedPtrList<CObList, CVariable*> CVarList;

/*
// class for a set of choices, used in dialog boxes.
class CChoiceSet : public CObject
{
public:
	CString		m_strName;			// name of this set, e.g. "bodies", "positions", etc.
	CStringList m_choices;			// the list of choices 
};

// a list of CChoiceSets:
typedef CTypedPtrList<CObList, CChoiceList*> CChoiceSets;
*/

class CFBDDoc : public COleDocument
{
protected: // create from serialization only
	CFBDDoc();
	DECLARE_DYNCREATE(CFBDDoc)

// Attributes
public:	
	
	// Problem parameters set by author:

	CString	m_strProblemId;			// helpsys prob ID. set by author, empty if unset
	int 	m_nVersion;				// File format version number
	int		m_nProblemType;			// type of problem this is:
	// NB: Codes must match radio button order in problem property page
#define PROB_QUANT		0			// Quantitative problem
#define PROB_EXAMPLE	1			// Example for studying
#define PROB_QUAL		2			// Qualitative problem

	WorkState m_workState;			// solution state -- stored at top for easy access.
	WorkState UpdateWorkState();	// recalc work state, returning new val
	static WorkState LoadWorkState(LPCTSTR pszPathName); // read it from a problem file
	CString    GetWorkStateStr();   // return string for current work state

	CString     m_strScore;         // score on problem (as string)
	
	BOOL		m_bIncludePlan;		// Include High-Level Solution Window
	//
	// Info to send to help system:
	//
	int			m_nKBType;			// type of KB info to use
	// Codes must match radio button in problem property page
#define KB_CLIPS		0			
#define KB_ATMS			1

	int			m_nAssessor;		// assessor flags to send to help system
#define ASSESSOR_EXACT		 0
#define ASSESSOR_APPROXIMATE 1
#define ASSESSOR_NONE		 2

	DWORD		m_wConcept;			// concepts used -- affects variable choices
#define ID_PROB_KINEMATICS		0x00000001	
#define ID_PROB_FORCE			0x00000002
#define ID_PROB_ENERGY			0x00000004
#define ID_PROB_CIRCMOTION		0x00000008
#define ID_PROB_ROTKINEMATICS	0x00000010
#define ID_PROB_WORK			0x00000020
#define ID_PROB_VECTOR			0x00000040
#define ID_PROB_MOMENTUM		0x00000080
#define ID_PROB_GRAVITATION		0x00000100
#define ID_PROB_RELVEL          0x00000200
#define ID_PROB_FLUIDS          0x00000400
// E&M topics:
#define ID_PROB_CIRCUITS		0x00010000
#define ID_PROB_EM              0x00020000
#define ID_PROB_OPTICS          0x00040000

	CStringList m_strFeatures;		// list of feature strings, from .prb file
	void AddConceptsToFeatures();	// for old-style docs, sets feature list from concepts

	// whether to present choices for Z Axis vectors:
	BOOL	UseZAxis() { return m_wConcept & (ID_PROB_ROTKINEMATICS | ID_PROB_CIRCMOTION | ID_PROB_EM); }

	CString		m_strCreator;		// Solution files: id of student who created it

	// specification for creating new problems:
	CString m_strStatement;				// statement spec (including embedded answer box markers)
	CString m_strGraphicFile;			// File to import for new graphics

	// Choice lists:
	CStringList m_strObjects;			// list of problem objects (bodies)
	CStringList m_strTimes;				// list of distinguished time pts and intervals
	CStringList m_strPositions;			// list of positions of objects (obsolete, unused)
	CStringList m_strBranches;			// list of branches for circuit problems
	CStringList* GetChoiceList(const CString& strName); 

/*
	// New: use a named set of choices
	CChoiceSets m_choices;          // list of name-choice set pairs
	void GetList(CString strName, CStringList& strList);
	AddChoiceSet(CString strName);
*/

	// The list of drawing objects. (Managed in the FBDView) 
	// Includes both problem text and graphics, and student-drawn graphics:
	CDrawObjList m_objects;
	CDrawObjList* GetObjects() { return &m_objects; }
	CSize GetSize();				// returns max extent of drawn objects
	BOOL IsAxesDrawn();
	int GetAxesDirection();
	
	// The list of student defined variables
	CVarList m_Variables;
	void AddVariable(CVariable* pVar); 
	void RemoveVariable(CVariable* pVar);
	// for convenience: current list of variable names
	CStringList m_strVarNames;

	 // The old-style solution plan: (Managed in the PlanView)
	CPlanObj	m_plan;
	void DeletePlanItems();		// used by plan view
	BOOL m_bPlanWnd;			// was the plan window ever created?

	//The high level solution window (plan consists of stages)
	typedef CTypedPtrList<CObList, CStageObj*> CStageList;
	CStageList m_stages;
	void AddStage(CStageObj* pStage); 
	void RemoveStage(CStageObj* pStage);
	CSize GetHiLevelPlanSize();

	//Now added top-level Principles to apply (Principles View)
	typedef CTypedPtrList<CObList, CPrincObj*> CPrincList;
	CPrincList m_principles;
	void AddPrinciple(CPrincObj* pPrinc); 
	void RemovePrinciple(CPrincObj* pPrinc);

	// The list of equations and their status: (Managed in EQView)
#define NEQS 40						// fixed max number of equations
	Status		 m_statusEq[NEQS];	// array of Status values
	CString		 m_strEq[NEQS];		// array of Equation strings

	// During editing eq and plan info is maintained separately in the relevant
	// views, because some of it is in windows controls that they manage.
	// The data in the doc object is only updated from the view on serialization.

	// For fetching embedded help system files on DocItem list (see below):
	CHelpData* GetHelpData(LPCTSTR pszName);

	// counters to keep some basic statistics:
	int m_nStartTime;				// history time of problem open, for time on task
	int m_nHelpReqs;				// number of help requests (not implemented).
	
// Operations
public:
	// For manipulating the drawing objects:
	void Draw(CDC* pDC, CFBDView* pView = NULL);
	void DrawMasked(CDC* pDC);
	void Add(CDrawObj* pObj, BOOL bGenID = TRUE);
	CDrawObj* AddClone(CDrawObj* pOrigObj, BOOL bOffset = TRUE);
    void Remove(CDrawObj* pObj);
    void DeleteObjList(CDrawObjList* pObjList);
    void MoveToFront(CDrawObj* pObj);
	void MoveToBack(CDrawObj* pObj);
	void MoveToBackStudent(CDrawObj* pObj);
	
    CDrawObj* ObjectAt(const CPoint& point, BOOL (*pfnbIgnore)(CDrawObj* pObj) = NULL);
    CDrawObj* Lookup(LPCTSTR pszId);
    CDrawObj* FindByName(LPCTSTR pszName);
	COutlineItem* ItemAt(const CPoint& point);      // hi-level solution piece
   
    // Checking object definitions for conflicts:
    CDrawObj* GetMatchingObj(CDrawObj* pObj, BOOL bMatchName = TRUE);//if FALSE, match def
	CVariable* GetMatchingVar(CDrawObj* pObj, BOOL bMatchName = TRUE);//if FALSE, match Def
	BOOL IsMatchingTime(CString strTime1, CString strTime2);
    CString GetMatchingPredef(CString& strName);

	// Edit document properties (author-mode)
	void OnEditProperties();

	BOOL HelpAllowClose();

public:
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CFBDDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	virtual void OnCloseDocument();
	virtual BOOL SaveModified();
	//}}AFX_VIRTUAL
	virtual BOOL DoSave(LPCTSTR lpszPathName, BOOL bReplace = TRUE);

protected:
	BOOL m_bSaveAsCmd;		// flag if handling user's SaveAs command to DoSave
	BOOL DoSaveCopyAs();	// worker routine to save a copy
	//
	// Flagging special reasons for doc open:
	//
public:
	BOOL m_bDemoProblem;		// opened for demo mode, not for working 
	BOOL m_bFilePreview;		// opened only for preview in file open dialog
	BOOL m_bPrintOnly;			// opened only to print
protected:
	void SerializeHeader(CArchive& ar);

// Implementation
public:
	virtual ~CFBDDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
protected:
	// helper derives problem id from filename if necessary
	void EnsureProblemId(LPCTSTR lpszPathName);

	void LogOpen(LPCTSTR lpszPathName);     // log the open event
public: // so can call from log playback
	void CheckInitEntries();        // recheck initial entries on open partial solution

public: // counters public so can be set on log playback
    int nNextID;				// counter for generating problem-unique ids
	int nNextStageID;			//counter for generating unique stage ids

	void GenerateId(CDrawObj* pObj);
	void GenerateId(CVariable* pVar);
	void GenerateId(CStageObj* pStage);
protected:
	int  GetUnusedId();

public:	// so can be used by event broadcaster to send init entries
	void ImportProblem();
	BOOL LoadFromPrb(LPCSTR pszFileName);
	void LogInitEntries();

	CRect LayoutStatement(const CString& strStatement);
	void LayoutGraphic(const CString& strFileName, const CRect& rcStmt);
	void RemoveLayoutItems();
	void LayoutProblem();
	int GetNextAxesIndex();

	// Helpers for dealing with object metrics and font info.
	// Static so don't need a document pointer:
	static void SetLogicalUnits(CDC* pDC);
	static void GetDefaultFont(LOGFONT* plf);

// Generated message map functions
protected:
	//{{AFX_MSG(CFBDDoc)
	afx_msg void OnFileSaveAs();
	afx_msg void OnUpdateFileSaveAs(CCmdUI* pCmdUI);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

// Default document page dimensions, as function of inches:
// This default is only used when drawing grid w/page boundaries in CFBDView. 
// When actually rendering for printing, will layout into printer page area.
#define PAGEWIDTH  (8 * nLUsPerInch)	// assumed page width, logical units
#define PAGEHEIGHT (11 * nLUsPerInch)	// assumed page height, logical units

//
// Utility funcs for dealing with some data representations in the doc:
// SplitStr: split tokens at delimiters from a single CString into a StrList
//
extern void SplitStr(CString& str, CStringList& list, LPCTSTR pszSeps = " \t\n\r,");

// 
// Hints available for use by view for UpdateAllViews/OnUpdate
//
// We define them here so all views on this document which might receive
// them will know about them by including document header.
//
// Hints pertaining to diagram objects and variables:
#define HINT_UPDATE_WINDOW    0	
#define HINT_UPDATE_DRAWOBJ   1
#define HINT_UPDATE_SELECTION 2
#define HINT_DELETE_SELECTION 3
#define HINT_DELETE_VARIABLE  4
#define HINT_UPDATE_VARLIST	  5
#define HINT_UPDATE_TEMPOBJ	  6	    // sent by dialog on submit def
#define HINT_UPDATE_OLE_ITEMS 7
#define IS_DIAGRAM_UPDATE(hint) ((hint) <= HINT_UPDATE_OLE_ITEMS)

// Hints pertaining to hi-level plan, principle window:
#define HINT_UPDATE_OUTLINEITEM	8
#define HINT_UPDATE_HILEVELVW	9
#define HINT_DELETE_PROP		10
#define HINT_ADD_PROP			11
#define HINT_DELETE_PRINC		12
#define HINT_ADD_PRINC			13
#define HINT_UPDATE_PRINC       14
#define HINT_CHKCHANGE_PRINC	15
#define IS_PLAN_UPDATE(hint)	(((hint) >= HINT_UPDATE_OUTLINEITEM) && ((hint) <= HINT_CHKCHANGE_PRINC) )

// Hints pertaining to equations:
#define HINT_UPDATE_EQUATION  16

// Hints on application mode change:
#define	HINT_AUTHOR_MODE	  17	// hint arg is new m_bAuthormode.
#define HINT_TUTOR_MODE		  18	// hint arg is new m_bTutorMode



/////////////////////////////////////////////////////////////////////////////
// CFBDDocTemplate -- special document template to customize problem opening

class CFBDDocTemplate : public CMultiDocTemplate
{
// Constructors 
public:
	CFBDDocTemplate(UINT nIDResource, CRuntimeClass* pDocClass,
					CRuntimeClass* pFrameClass, CRuntimeClass* pViewClass);
	DECLARE_DYNAMIC(CFBDDocTemplate)

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CFBDDocTemplate)
	//}}AFX_VIRTUAL
	// Class Wizard does not help with DocTemplate-derivatives.  This class was
	// originally added as CCmdTarget, to allow for message maps. 
public:
	virtual CDocument* OpenDocumentFile(
		LPCTSTR lpszPathName, BOOL bMakeVisible = TRUE);

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CFBDDocTemplate)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
//
// Help system data files embedded in document file. 
// We put these as custom DocItem derivatives on the doc's DocItem list.
// The list also holds embedded OLE items in COleClientItems. but we
// are not using OLE for this.
// CHelpData object is just a header wrapping a block of opaque data.
//
class CHelpData : public CDocItem
{
public:
	void ExportFile();
	CHelpData::CHelpData();
	DECLARE_SERIAL(CHelpData);
	virtual CHelpData::~CHelpData();
	virtual void Serialize(CArchive& ar);
	
	virtual BOOL IsBlank();

	CString	m_strFileName;    // filename. Will go under /ANDES-DIR/Pdata/ProbID/
	CTime	m_timeModified;	  // time source file last modified
	int		m_cbLength;		  // length of data in bytes.	
	void*   m_pData;		  // data block in memory

	void	SetData(LPCSTR pszPathName); // set file data given full pathname
	
protected:
	void	FreeData();		  // free up data, emptying item.

};

#endif !FBDDOC_INCLUDED
