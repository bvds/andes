#if !defined(AFX_PROBLEMSET_H__FB053180_DD41_11D2_807C_8F49523D0910__INCLUDED_)
#define AFX_PROBLEMSET_H__FB053180_DD41_11D2_807C_8F49523D0910__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ProblemSet.h : header file
//

#include "FBDDoc.h"	// for WorkState enum

/////////////////////////////////////////////////////////////////////////////
// CProblemSet document

class CProblemSet;	//forward ref

// OptionSet: stores set of named, string-valued option settings
// Currently just a StringToString map
typedef CMapStringToString COptionSet;

// 
// per-task info in problem set
//
class CTask : public CObject {
protected:
	DECLARE_SERIAL( CTask )
public:
	BOOLEAN IsVideo();
	CTask() { m_pSet = NULL; m_work = workNone; m_strExt = ".fbd"; };
	void Serialize( CArchive& ar );
	
	CProblemSet* m_pSet;		// back pointer to containing problem set

	CString m_strName;			// problem id = file basename
	CString m_strExt;			// extension = type (default .fbd, .apx = example).
	COptionSet m_opts;			// customized option settings for this problem

	// dynamic student-mode info:
	WorkState m_work;			// how much is done on this task
	
	CString GetSolutionPath(BOOL bEnsureExists = FALSE);

#ifdef _DEBUG
	void DumpOpts();
#endif 
};

typedef CTypedPtrList<CObList, CTask*> CTaskList;

class CProblemSet : public CDocument
{
protected:
	CProblemSet();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CProblemSet)

// Attributes
public:
	COptionSet m_opts;			// default option settings for whole set
	CTaskList m_tasks;			// list of tasks to do.

	// dynamic info: 
	CString m_strName;			// ProbSet name = file's basename, set on open
	// Working directory to hold prob solutions?
	// Currently active problem?

	CString GetStudentSolnDir();

	BOOL m_bOli;				// true if this is an OLI .atd problem set doc

// Operations
public:
	void AddTask(LPCTSTR pszName, LPCTSTR pszExt = ".fbd");
	void RemoveTask(LPCTSTR pszName);
	CTask* FindTask(LPCTSTR pszName);
	void PreCloseProblem(CFBDDoc* pDoc);
	void PostCloseProblem(CFBDDoc* pDoc);
	
	// For OLI interface
	int GetHistory();
	int PutHistory(CString strPathName);
	int GetSolution(CString strPathName);
	int PutSolution(CString strPathName);
	int PutLog(CString strPathName);
	int GetProblemFiles(CString strProblemId);
	int GetProblemGraphic(CString strFileName);
	int SetScore(CFBDDoc* pDoc);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CProblemSet)
	public:
	virtual void Serialize(CArchive& ar);   // overridden for document i/o
	virtual void OnCloseDocument();
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	protected:
	virtual BOOL OnNewDocument();
	//}}AFX_VIRTUAL
	void Serialize2(CArchive& ar);

// Implementation
public:
	BOOL m_bViewSolution;
	CTask* GetFirstIncompleteTask();

	virtual ~CProblemSet();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
protected:
	void GetName(CString strPath);
	void UpdateSolutionState();
	void SetStatusMsg(LPCTSTR pszText);
	
	// Generated message map functions
protected:
	//{{AFX_MSG(CProblemSet)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#define HINT_UPDATE_STATUS_MSG 1

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PROBLEMSET_H__FB053180_DD41_11D2_807C_8F49523D0910__INCLUDED_)
