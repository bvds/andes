// EQView.h : interface of the CEQView class
// 
// This view presents a list of equations edit controls in a dialog based form view
/////////////////////////////////////////////////////////////////////////////

class CFBDDoc;			// type of doc this is a view of
class CSymbolMenu;

// Hdr requires edit class decls to declare embedded objs (not just ptrs to objs):
#include "EQEdit.h"		// class for our equation edit controls	

// 
// Equation entry controls are assigned a continuous block of control IDs in 
// the following range: 
//
#define FIRST_EQ_ID		IDC_EQ1
#define LAST_EQ_ID		IDC_EQ40
//
// Helper macros: There are two ways of referencing an equation entry, by
// zero-based array index or by the child window ID of the Windows edit control.
// We use the array index in all external functions, but Windows sends event
// info using the id. We always use "idEq" for control id, "nEq" for array index
//
#define NUM_EQS			(LAST_EQ_ID - FIRST_EQ_ID + 1)	 
#define IS_EQ_ID(id)    ((id) >= FIRST_EQ_ID && (id) <= LAST_EQ_ID)
#define EQ_INDEX(id)	((id) - FIRST_EQ_ID)	// maps control ID to array index:
#define IS_EQ_INDEX(nEq)((0 <= (nEq)) && ((nEq) < NUM_EQS))
#define EQ_ID(nEq)		(FIRST_EQ_ID + (nEq))	// maps array index to control ID

//
// The Equation View class.
//
class CEQView : public CFormView, public IEventHandler
{
protected: 
	CEQView();
	DECLARE_DYNCREATE(CEQView)

public:
	//{{AFX_DATA(CEQView)
	enum { IDD = IDD_EQ_FORM };
	CButton	m_btnGreek;
	//}}AFX_DATA

// Attributes
public:
	CFBDDoc* GetDocument();
	
private:
#ifndef EQ_RICHEDIT
	CEQEdit m_Edit[NUM_EQS];		// array of edit control objects
#else
	CEQRichEdit m_Edit[NUM_EQS];	// array of edit control objects
#endif
	
public:
	const CString GetEquation(UINT nEq);	// Get copy of specified equation string
	const Status GetEqStatus(UINT nEq);		// Get status of specified equation
	void SetEqStatus(UINT nEq, Status statusNew); 

	int GetActiveEq();				// Get index of current focus eq, -1 if none.
	int m_nLastActive;				// index of most recent active (focus) edit.

	BOOL IsBlank(UINT nEq);			// test if given eq is all blank, hence invisible

	int GetIdealHeight();			// tell containing splitter what size we'd like

// Operations
	void UpdateDoc();				// transfer data from controls into document data struct
	void DeleteEquation(UINT nEq);
	void SelectEquation(UINT nEq);
	void SolveFor(CString str);

	// Event playback support (IEventHandler implementation
	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);
	virtual void PointToObject(LPCTSTR pszObjID);
	
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEQView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual void OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView);
	//}}AFX_VIRTUAL

// Implementation
public:
	void EnablePane(BOOL bEnable);
	virtual ~CEQView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual BOOL PreTranslateMessage(MSG* pMsg); // hook to process std popup menu kbd cmd

protected:
	BOOL m_bIgnoreChange;		// set to turn off edit change notification handling
	void SyncEqCtl(UINT nEq);	// update display to match document data.

	int GetNextEqSlot(int nStart = 0);
	
	void CheckEquation(UINT idEq);
	// static callback function to handle async check completion:
	static void OnEqResult(DWORD dwContext, LPCTSTR pszResult);	
	
	// for manipulating status color of edit control id:
	Status GetCtlStatus (int idEq);
	void SetCtlStatus (int idEq, Status status);
	void ApplyStatusResult(LPCTSTR pszResult, UINT nEq);
	
	// Cut/Copy/Paste Helper inserted by Component Gallery "clipboard assistant":
	BOOL IsEditCtrl(CWnd* pWnd);

	void DoHelpWhatswrong(int nEq);

	BOOL m_bWasNonBlank;			// active eq was non-blank when got focus

	void EnsureVisible(int nEq);

	BOOL m_bEnabled;
// Generated message map functions
protected:
	//{{AFX_MSG(CEQView)
	afx_msg void OnEnter();
	afx_msg void OnHelpWhatswrong();
	afx_msg void OnUpdateHelpWhatswrong(CCmdUI* pCmdUI);
	afx_msg void OnCalculator();
	afx_msg void OnUpdateEditCopyCut(CCmdUI* pCmdUI);
	afx_msg void OnUpdateEditPaste(CCmdUI* pCmdUI);
	afx_msg void OnUpdateEditUndo(CCmdUI* pCmdUI);
	afx_msg void OnEditCopy();
	afx_msg void OnEditCut();
	afx_msg void OnEditPaste();
	afx_msg void OnEditUndo();
	afx_msg void OnEditDelete();
	afx_msg void OnCalculate();
	afx_msg void OnUpdateCalculate(CCmdUI* pCmdUI);
	afx_msg void OnSolvefor();
	afx_msg void OnUpdateSolvefor(CCmdUI* pCmdUI);
	afx_msg void OnUpdateSolveforMenu(CCmdUI* pCmdUI);
	afx_msg void OnUpdateGreek(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg void OnInsertGreekLetter(UINT nID);
	afx_msg void OnSolveForNth(UINT nID);
	// Handle events mapped from range of edit controls (ClassWizard doesn't manage)
	afx_msg void OnContextMenu(CWnd*, CPoint point);
	afx_msg void OnChange(UINT idEq);
	afx_msg void OnFocus(UINT idEq);
	afx_msg void OnKillFocus(UINT idEq);
	// to update buttons during idle time command updating:
	afx_msg LRESULT OnIdleUpdateCmdUI(WPARAM, LPARAM);
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in EQView.cpp
inline CFBDDoc* CEQView::GetDocument()
   { return (CFBDDoc*)m_pDocument; }
#endif


/////////////////////////////////////////////////////////////////////////////
