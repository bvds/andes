#if !defined(AFX_EXPLANVW_H__2382E142_3FB7_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_EXPLANVW_H__2382E142_3FB7_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// EXPlanVw.h : header file
//
class CExplainBodyDlg;
class CExp2Dlg;
class CExp4Dlg;

typedef CTypedPtrMap<CMapPtrToPtr, HTREEITEM, CPlanItem*> CMapTreeitemToPlanitem;
typedef CTypedPtrMap<CMapPtrToPtr, CPlanItem*, HTREEITEM> CMapPlanitemToTreeitem;

#include "PlanView.h"

typedef CTypedPtrList<CObList, CPlanItem*> CPlanItemList;

/////////////////////////////////////////////////////////////////////////////
// CEXPlanVw form view

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

class CEXPlanVw : public CFormView
{
protected:
	CEXPlanVw();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CEXPlanVw)

// Form Data
public:
	//{{AFX_DATA(CEXPlanVw)
	enum { IDD = IDD_EXAMPLE_PLAN };
	CStatic	m_txtBrowserType;
	CButton	m_Template;
	CStatic	m_txtHeading;
	CStatic	m_txtDirect;
	CButton	m_Done;
	CButton	m_Submit;
	CStatic	m_txtInstruct;
	CTreeCtrl	m_ctrlPlan;
	//}}AFX_DATA

// Attributes
public:

	//three template dialogs
	CExplainBodyDlg* m_pTemplateDlg;
	CExp2Dlg* m_pTemp2Dlg;
	CExp4Dlg* m_pTemp4Dlg;

	
// Operations
public:
	int m_nPos;//position in menu in EXView
	BOOL m_bBrowser;//showing the rule browser
	BOOL m_bPlan;//showing the plan
	BOOL m_bExplained;//has the EXText or obj already been explained?
	BOOL m_bRuleExp;//has the rule been explained?
	void InsertRules();
	void CollapseTree();
	void OnSubmit(BOOL bDblClick);
	void OnDone(BOOL bCancel);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEXPlanVw)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CFBDDoc* GetDocument();//gets current document

	void UpdateItems();
	void InsertPlan();
	void PopulatePlan();
	void ApplyStatus(HTREEITEM hItem, LPCSTR pszResult);

	CString LookupLispString(CString listItem);//gets lisp string to send to help
	CString GetItemPosition(HTREEITEM hItem);//gets position to send to help

	HTREEITEM GetNextExistingItem(HTREEITEM hItem);
	HTREEITEM InsertPlanItem(CPlanItem* pPlanItem, HTREEITEM hParent);
	HTREEITEM FindItemFromId(int ID);//finds htreeitem from rule id

	HTREEITEM m_Selected;//the currently selected tree item
	HTREEITEM m_lastItem;//last item inserted in the plan or rule tree

	//maps to serialize plan object
	CMapTreeitemToPlanitem m_mapTreeitemToPlanitem;
	CMapPlanitemToTreeitem m_mapPlanitemToTreeitem;
	CImageList m_imgStatus;
	CImageList m_imgExPlan;

	virtual ~CEXPlanVw();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CEXPlanVw)
	afx_msg void OnSelchangedPlantree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDblclkPlantree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnEnable(BOOL bEnable);
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	afx_msg void OnClickPlantree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnTemplate();
	afx_msg void OnSelchangingPlantree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnItemexpandedPlantree(NMHDR* pNMHDR, LRESULT* pResult);
	//}}AFX_MSG
public:
	void ShowTemplate(int rule, int pos, BOOL bExplained);
	HTREEITEM GetItemFromPosition(CString pos);
	afx_msg void OnSubmit();
	afx_msg void OnDone();
	afx_msg void OnUpdateSubmit(CCmdUI* pCmdUI);
	DECLARE_MESSAGE_MAP()
};


#ifndef _DEBUG  // debug version in eqtest1View.cpp
inline CFBDDoc* CEXPlanVw::GetDocument()
   { return (CFBDDoc*)m_pDocument; }
#endif
/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EXPLANVW_H__2382E142_3FB7_11D1_A09F_0000C0086DCF__INCLUDED_)

