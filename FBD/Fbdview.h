// FBDView.h : interface of the CFBDView class
// 
// $Id: Fbdview.h,v 1.6 2006/02/16 22:18:55 anders Exp $
/////////////////////////////////////////////////////////////////////////////

// forward references, so don't need to worry about including 
// FBDDoc.h and the dialogs before this.
class CFBDDoc;
class CDrawObj;
class CVector;
class CSystem;
class CAxes;
class CMotionDiagram;
class C2DMotion;
class CMDVector;
class CVectorDlg;
class CSystemDlg;
class CAxesDlg;



// Conditional code for changing base class to CScrollView:
#ifdef FBD_SCROLL_VIEW				// define to get scrolling view code
#define		CBaseView CScrollView			
#else // use non-scrolling view
#define		CBaseView CView
#endif 	

#include "EQEdit.h"					// subclassed edits for equations (answer boxes)

// conditional code for changine edit class to richedit derivative. rather than plain edit
#ifdef EQ_RICHEDIT					// define to use richedits
#define		CEQEditType CEQRichEdit
#else	// plain old edit controls
#define		CEQEditType CEQEdit
#endif 

// CChoiceBtn: Subclassed button ctrl linked to choice item data objects in document
class CChoiceBtn: public CButton
{
public:
	CChoice* pChoice;	// ptr to doc's choice item object where value is stored
	CGroup* pGroup;		// item's choice group object, NULL if not part of group
	DECLARE_DYNAMIC(CChoiceBtn) // Includes run-time type info for IsKindOf
};
	

class CFBDView : public CBaseView, public IEventHandler
{
protected: // create from serialization only
	CFBDView();
	DECLARE_DYNCREATE(CFBDView)

// Attributes
public:
	CFont m_fontText;
	void DeleteSelection();
	void CheckMultSel();

	CPoint GetPointOnLine(CDrawObj* pLine, CPoint intercept, int radius, int nAxis);
	CPoint ExtendStartLine(CDrawObj* pLine, int nAxis);
	CPoint ExtendEndLine(CDrawObj* pLine, int nAxis);

	int GetRadius(CPoint intercept, CPoint point);
	BOOL Intersect(CPoint p1, CPoint p2, CPoint p3, CPoint p4);
	BOOL CCW(CPoint p0, CPoint p1, CPoint p2);
	CRect GetAnglePosition(/*CPoint point*/);
	CPoint IntersectPoint(CDrawObj* pObj1, CDrawObj* pObj2, int nAxis);
	BOOL TwoLinesSelected(CDrawObj* pObj1, CDrawObj* pObj2);
	
	CFBDDoc* GetDocument();
	
	// 
	// Selection: a list of selected drawobjs
	// 
	CDrawObjList  m_Selection;	
	BOOL HaveSelection()	// true if have non-empty selection
		{ return ! m_Selection.IsEmpty(); }
			
	BOOL SingleSelection()	// true if single object selected
		{ return m_Selection.GetCount() == 1; }
			
	CDrawObj* SelectedObj() // gets single selected object. 
		{ ASSERT(SingleSelection()); // Only valid if SingleSelection 
	      return m_Selection.GetHead(); }
			
	
// Operations		(!!! not clear all these need to be public)
public:
	// Selection management:
	void Select(CDrawObj* pObj, BOOL bAdd = FALSE);
	void SelectWithinRect(CRect rect, BOOL bAdd = FALSE);
	void RemoveFromSel(CDrawObj* pObj);
	void Deselect(CDrawObj* pObj);
	// Following required to test OleClientItems by MFC OLE support.
	virtual BOOL IsSelected( const CObject* pDocItem ) const;

	// Mark object invalid in this view only:
	void InvalObjInView(CDrawObj* pObj);

	// Coordinate transformations for scrolling view:
	void DocToClient(CRect& rect);
	void DocToClient(CPoint& point);
	void ClientToDoc(CRect& rect);
	void ClientToDoc(CPoint& point);

	// Log/Script event handling:
	virtual BOOL DispatchEvent(EventID id, LPCTSTR pszArgs);
	virtual void PointToObject(LPCTSTR pszObjID);

private:
	void WRITELINE(CDC* pDC, CRect &rcRect, CString str, Status status);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CFBDView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual void OnPrepareDC(CDC* pDC, CPrintInfo* pInfo = NULL);
	virtual void OnInitialUpdate();
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView);
	virtual BOOL OnScrollBy(CSize sizeScroll, BOOL bDoScroll = TRUE);
	virtual void OnPrint(CDC* pDC, CPrintInfo* pInfo);
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL
	virtual int OnToolHitTest( CPoint point, TOOLINFO* pTI ) const;

// Implementation
public:
	virtual ~CFBDView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	 // Drawing tools
	enum drawMode			// Set of available drawing tools
	{						// code = menu/toolbar command ID.
		notDrawing = 0, 
		Vector = ID_DRAW_VECTOR,  // has subtype below.
		Axes = ID_DIAGRAM_COORDINATES, 
		System = ID_SYSTEM,  
		Label = ID_LABEL, 
		Angle = ID_ANGLE, 
		Selector = ID_SELECT, 
		GuideLine = ID_GUIDE_LINE, 
		Rectangle = ID_RECTANGLE, 
		RoundRect = ID_ROUNDRECT, 
		Ellipse = ID_ELLIPSE, 
		Line = ID_LINE, 
		Arc = ID_ARC, 
		Arc2 = ID_ARC2, 
		Polygon = ID_POLYGON, 
		BezierCurve = ID_POLYBEZIER,
		MotionDiagram = ID_RULER, 
		MotionBody = ID_MOTION_BODY, 
		DocArea = ID_DOC_AREA,
		Radius = ID_LABEL_RADIUS,
		HyperText = ID_HYPERTEXT,
	} m_drawMode;				// Major state == active tool

	HCURSOR GetDrawtoolCursor(drawMode tool);

	// state info used by all tools:
	CPoint	m_ptDown;			// Where mouse button first went down when tracking
	CPoint	m_ptPrev;			// Previous mouse location when tracking
	CPoint  m_ptLastLocal;		// Last point in logical ("local") coords.
	CDrawObj* m_pCurrentObj;	// Object currently being tracked

	// tool-specific state:
	int m_vectorType;			// VECTOR_* subtype during vector draw
	int m_nZDir;				// direction for special z-axis vector drawing

	enum SelectMode { none, move, resize, netSelect }
			m_selectMode;		// subtype for select tool
	int		m_nDragHandle;		// index of handle being dragged on resize
	CRect	m_posVirtual;		// during move: what position would be if no alignment

	// Align to grid mode:
public:
	BOOL	m_bAlignGrid;		// whether to align objects to grid
	BOOL	m_bShowGrid;		// whether to display the grid
	int		m_cxGrid;			// x grid interval, logical units
	int		m_cyGrid;			// y grid interval, logical units
	COLORREF m_colorGrid;		// color to draw grid lines
	enum { GRID_COLOR = RGB(192, 255, 255) }; // light-blue default (!not in default palette).
protected:
	void AlignToGrid(CPoint& point);
	void AlignToGrid(CRect& rect);

	// Active status
public:
	CString m_strStats;
	BOOL m_bEnabled;
	void EnablePane(BOOL bEnable);
	CDrawObj* CreateNew(LPCSTR pszTypeName);
	BOOL	m_bActive;			// Set if this is active view

	// Training card support:
	void KillTCard();
	void ShowTCard(int idTCard);
	void NextInCard();
	void CloseTCardHelp();
	
protected:
	// for managing run-time child button controls for multiple-choice items
	void CreateChoiceBtn(CChoice* pChoice, CGroup* pGroup = NULL, 
		                 BOOL bBeginGroup = FALSE);	// !!! move to a constructor
	void CreateChoiceBtns();
	CTypedPtrList<CObList, CChoiceBtn*> m_ChoiceBtns; // list of child button records
	CChoiceBtn* FindBtn(CChoice* pChoice);	// maps obj to its control in view
public: // so other modules can use this helper func:
	static BOOL IsChoiceGroup(CDrawObj* pObj);		// tests if group obj contains choices

protected:
	// For managing run-time child EQEdit controls used as Answer boxes.
	void CreateAnswerBoxes();
	CTypedPtrList<CObList, CEQEditType*> m_Answers;
	int m_idFocusEdit;					// id last active edit control, NULL if none
	BOOL IsActiveEdit()		 { return m_idFocusEdit != -1; }
	CEQEditType* GetActiveEdit() { ASSERT(IsActiveEdit());
	                               return (CEQEditType*) GetDlgItem(m_idFocusEdit); } 
	CEQEditType* FindAnswerEdit(CString strId);
	void SetAnswerStatus(CEQEditType* pAnswer, Status nStatus);
	BOOL m_bIgnoreChange;				// set to ignore change notifications we caused
	void ApplyAnswerStatus(LPCTSTR pszResult, CEQEditType* pEdit);


	// hyper text
	BOOL m_bOnHyperText;
	CHyperLnk* HyperAt(CPoint point);

	// Helpers:
protected:
	// for drawing:
	void DrawGrid(CDC* pDC);
	void DrawBorder(CDC* pDC);

	// for scrolling view:
	void UpdateScrollSizes();	// update on possible change in doc size

	// for printing
	void PrintPageHeader(CDC* pDC, CPrintInfo* pInfo, CString& strHeader);
	BOOL UseLandscapeLayout();
	void ForceLandscapePrinting(CDC* pDC, CPrintInfo* pInfo);

	// for managing OLE items in scrolling view
	void UpdateActiveItem();

	// worker routines for draw tools:
	CDrawObj* NewDrawObj(drawMode type, CPoint local);
	void TrackMove(UINT nFlags, CPoint point);
	void DoPostTracking(UINT nFlags, CPoint point);

	// For motion diagrams
	BOOL HaveMotionArea();		// prob has diagram area, either author's or student's
	CDocArea* InMotionArea(CPoint local);	// point is in a diagram area		
	CMotionDiagram* FindMotionRuler(CPoint local); // get master ruler obj for area pt
	void AddMotionVector(CMotionDiagram* pRuler, CMDVector* pVec);
	// 2D diagrams:
	C2DMotion* In2DArea(CPoint local); // get 2D diagram for area pt, NULL if none
	void Add2DVector(C2DMotion* p2D, CMDVector* pVec);
	BOOL GetComponentDiagrams(CMotionDiagram* &pmdX, CMotionDiagram* &pmdY);
	
	// filters for use with ObjectAt:
	static BOOL IsVector(CDrawObj* pObj);
	static BOOL Unselectable(CDrawObj* pObj);
	
	// For cut/copy/paste:
	UINT m_nClipboardFormat;	// saves our registered custom format
	CRect GetInitialPosition();
	void PasteEmbedded(COleDataObject& dataObject, CPoint point);
	void PasteNative(COleDataObject& dataObject);
	HGLOBAL GetTextData();
	HENHMETAFILE GetMetaFileData();

	// For tooltips:
	static CDrawObj* s_pTipObj;		// static saves hit obj for tooltip callback

	// Helper tests if we are active view
	BOOL IsActiveView() { return GetParentFrame()->GetActiveView() == this; };

// Generated message map functions
protected:
	int m_nLastActive;
	void DestroyControls();
	//{{AFX_MSG(CFBDView)
	afx_msg void OnDrawVector();
	afx_msg void OnDrawvectorAcceleration();
	afx_msg void OnDrawvectorForce();
	afx_msg void OnDrawvectorVelocity();
	afx_msg void OnDrawvectorMomentum();
	afx_msg void OnDrawvectorImpulse();
	afx_msg void OnDrawvectorUnitVector();
	afx_msg void OnDrawvectorMagDipole();
	afx_msg void OnDrawvectorElecDipole();
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnUpdateDrawVector(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorUnitVector(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorMagDipole(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorElecDipole(CCmdUI* pCmdUI);
	afx_msg void OnLabel();
	afx_msg void OnUpdateAngle(CCmdUI* pCmdUI);
	afx_msg void OnEditDelete();
	afx_msg void OnUpdateEditDelete(CCmdUI* pCmdUI);
	afx_msg void OnEditProperties();
	afx_msg void OnUpdateEditProperties(CCmdUI* pCmdUI);
	afx_msg void OnDecomposeVector();
	afx_msg void OnUpdateDecomposeVector(CCmdUI* pCmdUI);
	afx_msg void OnEditDuplicate();
	afx_msg void OnUpdateEditDuplicate(CCmdUI* pCmdUI);
	afx_msg void OnExampleText();
	afx_msg void OnUpdateExampleText(CCmdUI* pCmdUI);
	afx_msg void OnHelpWhatswrong();
	afx_msg void OnUpdateHelpWhatswrong(CCmdUI* pCmdUI);
	afx_msg void OnGroup();
	afx_msg void OnUpdateGroup(CCmdUI* pCmdUI);
	afx_msg void OnUngroup();
	afx_msg void OnUpdateUngroup(CCmdUI* pCmdUI);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnUpdateEditCopyCut(CCmdUI* pCmdUI);
	afx_msg void OnUpdateEditPaste(CCmdUI* pCmdUI);
	afx_msg void OnEditCopy();
	afx_msg void OnEditCut();
	afx_msg void OnEditPaste();
	afx_msg void OnCancelEdit();
	afx_msg void OnViewGrid();
	afx_msg void OnUpdateViewGrid(CCmdUI* pCmdUI);
	afx_msg void OnAligntogrid();
	afx_msg void OnUpdateAligntogrid(CCmdUI* pCmdUI);
	afx_msg void OnUpdateMotionBody(CCmdUI* pCmdUI);
	afx_msg void OnInsertObject();
	afx_msg void OnUpdateInsertObject(CCmdUI* pCmdUI);
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	afx_msg void OnDestroy();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnObjectMoveBack();
	afx_msg void OnObjectMoveForward();
	afx_msg void OnObjectMoveToBack();
	afx_msg void OnObjectMoveToFront();
	afx_msg void OnUpdateSingleSelect(CCmdUI* pCmdUI);
	afx_msg void OnUpdateTogglegroup(CCmdUI* pCmdUI);
	afx_msg void OnTogglegroup();
	afx_msg void OnChoiceItem();
	afx_msg void OnUpdateChoiceItem(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDocArea(CCmdUI* pCmdUI);
	afx_msg void OnPaint();
	afx_msg void OnUpdateAuthorprops(CCmdUI* pCmdUI);
	afx_msg void OnAuthorprops();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnDrawvectorComponent();
	afx_msg void OnUpdateDrawvectorComponent(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorAcceleration(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorForce(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorVelocity(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorMomentum(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDrawvectorImpulse(CCmdUI* pCmdUI);
	afx_msg void OnEditUndo();
	afx_msg void OnUpdateEditUndo(CCmdUI* pCmdUI);
	afx_msg void OnViewFont();
	afx_msg void On2dmotion();
	afx_msg void OnUpdate2dmotion(CCmdUI* pCmdUI);
	afx_msg void OnHelpDrawforce();
	afx_msg void OnUpdateHelpDrawforce(CCmdUI* pCmdUI);
	afx_msg void OnTCard(UINT idAction, DWORD dwActionData);
	afx_msg void OnHelpDrawacceleration();
	afx_msg void OnUpdateHelpDrawacceleration(CCmdUI* pCmdUI);
	afx_msg void OnHelpDrawbody();
	afx_msg void OnUpdateHelpDrawbody(CCmdUI* pCmdUI);
	afx_msg void OnHelpDrawvelocity();
	afx_msg void OnUpdateHelpDrawvelocity(CCmdUI* pCmdUI);
	afx_msg void OnHelpDrawmotionbody();
	afx_msg void OnHypertext();
	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
	afx_msg void OnDrawvectorDisplacement();
	afx_msg void OnUpdateDrawvectorDisplacement(CCmdUI* pCmdUI);
	afx_msg void OnHelpDrawdisplacement();
	afx_msg void OnUpdateHelpDrawdisplacement(CCmdUI* pCmdUI);
	afx_msg void OnUpdateLabel(CCmdUI* pCmdUI);
	afx_msg void OnUpdateHypertext(CCmdUI* pCmdUI);
	afx_msg void OnUpdatePolybezier(CCmdUI* pCmdUI);
	afx_msg void OnUpdateAxes(CCmdUI* pCmdUI);
	afx_msg void OnAngle();
	afx_msg void OnUpdateZDirMenu(CCmdUI* pCmdUI);
	afx_msg void OnUpdateGreek(CCmdUI* pCmdUI);
	afx_msg void OnDrawvectorTorque();
	afx_msg void OnUpdateDrawvectorTorque(CCmdUI* pCmdUI);
	afx_msg void OnDrawvectorRelpos();
	afx_msg void OnUpdateDrawvectorRelpos(CCmdUI* pCmdUI);
	afx_msg void OnUpdateHelpDrawmotionbody(CCmdUI* pCmdUI);
	afx_msg void OnInsertpicture();
	afx_msg void OnUpdateInsertpicture(CCmdUI* pCmdUI);
	afx_msg void OnDrawvectorEfield();
	afx_msg void OnUpdateDrawvectorEfield(CCmdUI* pCmdUI);
	afx_msg void OnDrawvectorBfield();
	afx_msg void OnUpdateDrawvectorBfield(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg void OnMcQuestion();
	afx_msg void OnSelectDrawTool(UINT nID);
	afx_msg void OnUpdateDrawTool(CCmdUI* pCmdUI);
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	afx_msg void OnChoiceClicked(UINT idBtn);
	afx_msg void OnAnswerFocus(UINT idEq);
	afx_msg void OnAnswerKillFocus(UINT idEq);
	afx_msg void OnAnswerEnter();
	afx_msg void OnAnswerChange(UINT idEq);
	afx_msg BOOL OnToolTipNotify( UINT id, NMHDR * pNMHDR, LRESULT * pResult );
	afx_msg void OnSetZDir(UINT nID);
	afx_msg void OnUpdateSetZDir(CCmdUI* pCmdUI);
	afx_msg void OnInsertGreekLetter(UINT nID);
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in FBDView.cpp
inline CFBDDoc* CFBDView::GetDocument()
   { return (CFBDDoc*)m_pDocument; }
#endif


/////////////////////////////////////////////////////////////////////////////
