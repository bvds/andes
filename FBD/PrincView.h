#if !defined(AFX_PRINCVIEW_H__7121E800_E20F_11D2_B260_0000C5465DC1__INCLUDED_)
#define AFX_PRINCVIEW_H__7121E800_E20F_11D2_B260_0000C5465DC1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PrincView.h : header file
//


/////////////////////////////////////////////////////////////////////////////
// CPrincView view

class CPrincView  : public CTreeView, public IEventHandler
{
protected:
	CPrincView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CPrincView)
	CFBDDoc* GetDocument();

// Attributes
public:
protected:
	BOOL ItemIsChecked(HTREEITEM hItem);
	void SyncCheckState(HTREEITEM hItem, CPrincItem* pItem);

// Operations
public:
	void SelectItem(CPrincItem* pItem);
	CPrincItem* ArgToItem(LPCTSTR pszArg);
protected:
	void AddPrincItem(CPrincItem* pPrinc, HTREEITEM hParent = TVI_ROOT, 
										  HTREEITEM hInsertAfter = TVI_LAST);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPrincView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void OnDraw(CDC* pDC);      // overridden to draw this view
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	//}}AFX_VIRTUAL

	// Event playback support (IEventHandler implementation
	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);
	virtual void PointToObject(LPCTSTR pszObjID);

// Implementation
protected:
	virtual ~CPrincView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	// CImageList m_imgNormal;
	CImageList m_imgState;
	HTREEITEM m_hTitleItem;		// remembers dummy item used for title

	HTREEITEM FindItemData(DWORD dwData, HTREEITEM hItem = TVI_ROOT);
	HTREEITEM FindPrincItem(CPrincItem* pItem)
			{ return FindItemData((DWORD) pItem); }

	void ToggleCheckState(HTREEITEM hItem);
	void OnCheckStateChange(HTREEITEM hItem);

	// Generated message map functions
protected:
	CString ItemToArg(CPrincItem* pItem);
	BOOL HaveSelectedPrinciple();
	BOOL ItemIsDisabled(HTREEITEM hItem);

	//{{AFX_MSG(CPrincView)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnPrincipleAdd();
	afx_msg void OnPrincipleDelete();
	afx_msg void OnUpdatePrincipleDelete(CCmdUI* pCmdUI);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnSelchanging(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnItemexpanding(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnHelpWhatswrong();
	afx_msg void OnUpdateHelpWhatswrong(CCmdUI* pCmdUI);
	afx_msg void OnPrincipleModify();
	afx_msg void OnUpdatePrincipleModify(CCmdUI* pCmdUI);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in eqtest1View.cpp
inline CFBDDoc* CPrincView::GetDocument()
    { return (CFBDDoc*)m_pDocument; }
#endif


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PRINCVIEW_H__7121E800_E20F_11D2_B260_0000C5465DC1__INCLUDED_)
