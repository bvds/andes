// PlanView.h : header file
//
class CVariable;


typedef CTypedPtrMap<CMapPtrToPtr, HTREEITEM, CPlanItem*> CMapTreeitemToPlanitem;
typedef CTypedPtrMap<CMapPtrToPtr, CPlanItem*, HTREEITEM> CMapPlanitemToTreeitem;

#include "PlanObj.h"
#include "Childfrm.h"


typedef CTypedPtrList<CObList, CPlanItem*> CPlanItemList;
/////////////////////////////////////////////////////////////////////////////
// CPlanView view
class CDoneDlg;				//forward declaration
class CFBDDoc;



class CPlanView : public CTreeView, public IEventHandler
{
protected:
	CPlanView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CPlanView)

// Attributes
public:

	CString m_oldText;

	HTREEITEM m_Selected; //the one and only selected treeitem

	CFBDDoc*  GetDocument();
	void UpdateDoc();//function to create ordered list of planitems used in serialization

	//maps to serialize plan object
	CMapTreeitemToPlanitem m_mapTreeitemToPlanitem;
	CMapPlanitemToTreeitem m_mapPlanitemToTreeitem;
	
	CPlanItemList m_itemList;//list created for memory management purposes
	
// Operations
protected:
	CImageList m_imgSteps;

	HTREEITEM HitTest(CPoint point);

	HTREEITEM GetNextExistingItem(HTREEITEM hItem);//helper to traverse entire tree

	void PopulateTree();
	void InsertInitialNode();

	HTREEITEM InsertTreeItem(CPlanItem* pPlanItem, HTREEITEM hParent);
	void EditGoal(HTREEITEM selItem);
	void InsertGoal(HTREEITEM selItem);
	void InsertSubstep(HTREEITEM selItem, int nParentLevel);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPlanView)
	public:
	virtual void OnInitialUpdate();
	protected:
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CPlanView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	
// Generated message map functions
protected:
	CPlanItem* GetPlanItem(HTREEITEM hItem);
	//{{AFX_MSG(CPlanView)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnSelchanged(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnEdititem();
	afx_msg void OnUpdateEdititem(CCmdUI* pCmdUI);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnInsertSubitem();
	afx_msg void OnUpdateInsertSubitem(CCmdUI* pCmdUI);
	afx_msg void OnEditDelete();
	afx_msg void OnPlanAddnewgoal();
	afx_msg void OnUpdatePlanAddnewgoal(CCmdUI* pCmdUI);
	afx_msg void OnUpdateEditDelete(CCmdUI* pCmdUI);
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in eqtest1View.cpp
inline CFBDDoc* CPlanView::GetDocument()
   { return ((CChildFrame*)GetParentFrame())->m_pDoc; }
#endif

/////////////////////////////////////////////////////////////////////////////

#define LEVEL_GOAL				0
#define LEVEL_THIRDLAW_SUBGOAL	1
#define LEVEL_PRINCIPLE			2
#define LEVEL_STEP				3
#define	LEVEL_SUBSTEP			4
#define LEVEL_UNKNOWN_SUBGOAL	5
