/////////////////////////////////////////////////////////////////////////////
// CEXView view: Example mode view. 
//
// This view displays a problem file using
// the "poor man's eyetracker" to cover and uncover bits of text on mouse
// movements so the system can determine what the students are looking at.
//
// Currently we have a bit of a hack to recreate the OLAE mode, which allowed 
// the user to uncover the free-body-diagram pane by clicking the left mouse button 
// and the problem diagram pane by clicking the right.
//
// Our documents are unstructured lists of objects, with no indication which
// elements are part of the diagram or sketch and which are not. We therefore
// allow the author to define special text objects which we call "custom masks"
// to cover the diagram and sketch. See the implementation file for details. 
//
/////////////////////////////////////////////////////////////////////////////

// forward references, so don't need to worry about including 
// FBDDoc.h and the dialogs before this.
class CFBDDoc;
class CExplainDlg;

#ifdef EX_SCROLL_VIEW				// define to get scrolling view code

#define	CBaseView CScrollView			

#else // Normal, non-scrolling view

#define	CBaseView CView

#endif 		



class CEXView : public CBaseView, public IEventHandler
{//construction
protected: 
	CEXView();				// create from serialization only
	DECLARE_DYNCREATE(CEXView)

// Attributes
public:
	BOOL m_bCtrlMode;
	BOOL m_bCoachMode;
	BOOL m_bExplaining;			//are we in the midst of explaining an item?

	// for scrolling view:
	BOOL m_bScaleToFit;
	BOOL m_bInitScroll;

	CDrawObj* m_pVisibleObj;	// currently visible screen region
	CDrawObj* m_pSelection;		// currently selected diagram object

	CDrawObj* m_pExpObj;		// the text object we are explaining
	CDrawObj* m_pExpSel;		// the drawn object we are explaining
	CLabel*   m_pDiagram;		// saved ptr to diagram region mask

	int m_menuPos;
	
	CExplainDlg* m_pDlg;		// floating explain button

	CFBDDoc* GetDocument();	

protected:
	CBitmap*	m_pQmarkBmp;	// question mark image
	CLabel*		m_pSketch;		// saved ptr to sketch region mask
	CDrawObj*	m_pFindObj;		// text object where goal defined
	CDrawObj*	m_pHintObj;		// object: Hint on what to explain next

	CFont m_fontText;			// font for example text
							
	CDrawObj* m_pVisible2nd;	// when ctrl btn down, there are up to 
	CDrawObj* m_pVisible3rd;	// three visible objects

// Operations
protected:
	void SetSelectedFBDObj(CDrawObj* pObj); // update selected digram object
	void OnClickFBDObj(UINT nFlags, CPoint point);// process click in diagram 
												// now unused
	void UpdateBtnPos(CDrawObj* pObj);
	void CheckStudyItem(CString idExStr);//asynchronous help call
	static void OnExResult (DWORD dwContext, LPCTSTR pszResult);//asynch help callback

public:
	void HideCtrlObjs();
	void ResetView();
	void RevampBtn(CDrawObj* pObj);

	void SetVisibleObj(CDrawObj* pObj);	// helper to update visible region
	void HighlightHint(CString strHints);//advice from help system
//	void EnsureObjectVisible(CDrawObj* pObj);

	CDrawObj* GetItemFromID(CString strId);//get related item in ids.txt
	CDrawObj* GetEXObj();//get the item that is currently uncovered
	int GetExMenu(BOOL bShow, CDrawObj* pObj);//Get explain menu
							//if bShow is true, Show it

	void RecalcTextExtents();
	void InvalObjRegion(CDrawObj* pObj);
	void UpdateScrollSize();

	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);	// for log playback

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEXView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual void OnInitialUpdate();
	virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo = NULL);
	protected:
	virtual BOOL OnScrollBy(CSize sizeScroll, BOOL bDoScroll = TRUE);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CEXView();
	// coordinate conversions for scrolling view
	void DocToClient(CRect& rect);
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	// coordinate conversions for scrolling view
//	void DocToClient(CRect& rect);
	void DocToClient(CPoint& point);
	void ClientToDoc(CRect& rect);
	void ClientToDoc(CPoint& point);

// Generated message map functions
protected:
	//{{AFX_MSG(CEXView)
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnViewOptions();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnUpdateExplain(CCmdUI* pCmdUI);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnViewFont();
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags);
	//}}AFX_MSG
	afx_msg void OnExplainCmd(UINT nID);
	afx_msg void OnUpdateExplainCmd(CCmdUI* pCmdUI);
public:
	afx_msg void OnExplainBtn();
	DECLARE_MESSAGE_MAP()

private:								// helper functions:
	void ShowRegion(CDrawObj* pLabel);
	void HideRegion(CDrawObj* pLabel);
	void DrawHighlightBox(CDrawObj* pObj);
	void LogInitObjects();
	CLabel* GetMask(LPCTSTR pszName);
	LPCTSTR MaskTypeStr(CDrawObj* pObj);	// for logging

	// static functions for handy object predicates:
	static BOOL IsMask(CDrawObj* pObj);
	static BOOL IsHideable(CDrawObj* pObj);
	static BOOL NotSelectable(CDrawObj* pObj);
};

#ifndef _DEBUG  // debug version in EXView.cpp
inline CFBDDoc* CEXView::GetDocument()
   { return (CFBDDoc*)m_pDocument; }
#endif
