#if !defined(AFX_HILEVELVW_H__4B4E5C01_E97F_11D1_A6D7_0000C0086DCF__INCLUDED_)
#define AFX_HILEVELVW_H__4B4E5C01_E97F_11D1_A6D7_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000



//formatting
#define SPACE_BETWEEN_CTRLS	5
#define VERTICAL_SPACING	5
#define HORIZONTAL_SPACING	5
#define STAGE_SPACING		15
#define LEFT_MARGIN			10
#define ITEM_HEIGHT			20
#define ITEM_WIDTH			5
#define INDENT_WIDTH		20


/////////////////////////////////////////////////////////////////////////////
// CHiLevelVw view
class CStageObj;
class COutlineItem;
class CFBDDoc;

#include "ChildFrm.h"
#include "MyGrids.h"

class CHiLevelVw : public CScrollView, public IEventHandler
{
protected:
	CHiLevelVw();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CHiLevelVw)

	CFBDDoc* GetDocument();

// Attributes
public:

	CFont* m_pFont;	//the font used for this view
	CFont* m_pBoldFont;//font used for statics
	CFont* GetBoldFont() {return m_pBoldFont;}

//	COutlineItem* m_pTimeItem;//the one and only time item
	COutlineItem* m_pFocusedItem;//the item with the focus
	CTableGrid*	  m_pFocusedTable;//the table with the focus	


	COutlineItem* GetLastItem();//get the last item in the outline
	COutlineItem* GetCurrentItem(){ return m_pFocusedItem;}//the focused item is the current item

// Operations
public:

//helper functions
	void InvalItemInView(COutlineItem* pItem) ;


	void UpdateFromUserSelection(CTableRow* pProp);//function called when table highlighted and
												//user need to choose a parallel direction
												//or an unknown to be used as the sought
												//for the next stage (substage)

	void UpdateStagesthatFollow(CStageObj* pStage);//if we delete a stage, we need to update
													//the vertical position of the stages
protected:											//that follow
	void UpdateScrollSize();

	// For tooltips:
	static COutlineItem* s_pTipObj;		// static saves hit obj for tooltip callback

public:
	// Event playback support (IEventHandler implementation
	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);
		// Coordinate transformations for scrolling view:
	void DocToClient(CRect& rect);
	void DocToClient(CPoint& point);
	void ClientToDoc(CRect& rect);
	void ClientToDoc(CPoint& point);


private:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CHiLevelVw)
	public:
	virtual void OnInitialUpdate();
	virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo = NULL);
	protected:
	virtual void OnDraw(CDC* pDC);      // overridden to draw this view
	virtual BOOL OnScrollBy(CSize sizeScroll, BOOL bDoScroll = TRUE);
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	//}}AFX_VIRTUAL
	virtual int OnToolHitTest( CPoint point, TOOLINFO* pTI ) const;


// Implementation
protected:
	virtual ~CHiLevelVw();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
protected:
	//{{AFX_MSG(CHiLevelVw)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnHelpWhatswrong();
	afx_msg void OnUpdateHelpWhatswrong(CCmdUI* pCmdUI);
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnUpdateAddpropTime(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg void OnDirectionMenuCmd(UINT nID);
	afx_msg void OnUpdateDirectionParallel(CCmdUI* pCmdUI);
	afx_msg void OnAddPropMenuCmd(UINT nID);
	afx_msg void OnClickGrid(UINT gridId);
    afx_msg LRESULT OnGetFont(WPARAM hFont, LPARAM lParam);
	afx_msg BOOL OnToolTipNotify( UINT id, NMHDR * pNMHDR, LRESULT * pResult );
	DECLARE_MESSAGE_MAP()

};

#ifndef _DEBUG  // debug version in eqtest1View.cpp
inline CFBDDoc* CHiLevelVw::GetDocument()
   { return ((CChildFrame*)GetParentFrame())->m_pDoc; }
#endif



/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_HILEVELVW_H__4B4E5C01_E97F_11D1_A6D7_0000C0086DCF__INCLUDED_)
