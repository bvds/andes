// ChildFrm.h : interface of the CChildFrame class
// 
// $Id: childfrm.h,v 1.1 2005/01/24 16:28:09 bvds Exp $
/////////////////////////////////////////////////////////////////////////////
#ifndef CHILDFRAME_INCLUDED
#define CHILDFRAME_INCLUDED 1

class CFBDDoc;

class CChildFrame : public CMDIChildWnd
{
	DECLARE_DYNCREATE(CChildFrame)
public:
	CChildFrame();

// Attributes
public:
	CSplitterWnd m_splitLeftRight;		// top-level split into left-right halves
	CSplitterWnd m_splitTopBotLeft;		// top/bottom splitter nested in left half
	CSplitterWnd m_splitTopBotRight;	// top/bottom splitter nested in right half
	
	// For nesting dynamic splitter inside EQ pane:
	// from Prosise, p622: override to avoid bug nesting dyn split in static splitter
	class CNestedDynSplit: public CSplitterWnd {
	    virtual SplitRow(int cyBefore) // must activate view before splitting
			{ GetParentFrame()->SetActiveView((CView*) GetPane(0, 0));
			   return CSplitterWnd::SplitRow(cyBefore); };
	} m_splitEQ;						// dynamic splitter for EQ pane */

	/* CDialogBar m_wndPropertyBar;		// object attribute bar */
	BOOL m_bActivated;				// flag set after first-time activation.
	CFBDDoc* m_pDoc;				// ptr to document saved from CreateClient
	CToolBar m_wndProblemBar;		// problem-specific toolbar

// Operations
public:
	void HideRight();			// minimize right half

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChildFrame)
	public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual void ActivateFrame(int nCmdShow = -1);
	virtual BOOL OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo);
	protected:
	virtual BOOL OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext);
	//}}AFX_VIRTUAL

// Implementation
public:
	void ShowHintPane();
	void HideHintPane();

	void HideEQPane();
	void ShowEQPane();

	
	virtual ~CChildFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	BOOL CreateQuantProbPanes(LPCREATESTRUCT lpcs, CCreateContext* pContext);
	BOOL CreateQualProbPanes(LPCREATESTRUCT lpcs, CCreateContext* pContext);
	BOOL CreateExamplePanes(LPCREATESTRUCT lpcs, CCreateContext* pContext);
	
// Generated message map functions
protected:
	
	//{{AFX_MSG(CChildFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnWindowMinimizeright();
	afx_msg void OnZDirMenu();
	afx_msg void OnUpdateZDir(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
#endif CHILDFRAME_INCLUDED
