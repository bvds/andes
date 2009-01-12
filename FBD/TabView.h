#ifndef __AFXEXT_H__
#include <afxext.h>
#endif
#if !defined(AFX_TABVIEW_H__2140CCC1_D9C3_11D1_A6D7_0000C0086DCF__INCLUDED_)
#define AFX_TABVIEW_H__2140CCC1_D9C3_11D1_A6D7_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
//////////////////////////////////////////////////////////////////////
// CtabObject
//////////////////////////////////////////////////////////////////////
class  CTabObject : public CObject
{
public:


   // Constructor
   CTabObject();

   // Destructor
   ~CTabObject();

   // Accessor Functions
   void				SetView(CWnd * pView);
   CWnd *			GetView(void);
   void				SetRunTime(CRuntimeClass* pViewClass);
   CRuntimeClass *	GetRunTime(void);
   void				SetID(UINT nID);
   UINT				GetID(void);
   void				SetTabTitle(LPCSTR pszTabTitle);
   LPCSTR			GetTabTitle(void);

   // Data


protected:

   // Virtual function overrides
   #ifdef _DEBUG
      virtual void AssertValid() const;
      virtual void Dump(CDumpContext& dc) const;
   #endif




private:

   // Data
   CWnd*			m_pView;          // pointer to CWnd object
   BOOL				m_bActive;           // is this tab active?
   CRuntimeClass *  m_pRunTimeViewClass; // Pointer to the runtime class of the view
   UINT				m_nID;               // Resource ID of the view class
   LPCSTR			m_pszTabTitle;       // String that appears on the tab


};
/////////////////////////////////////////////////////////////////////
//  CTabObjArray header 
/////////////////////////////////////////////////////////////////////


class CTabObject;

class CTabObjArray : public CPtrArray
{
public:


   // Construction
   CTabObjArray();

   // Destructrion
   ~CTabObjArray();

   // Data
   int m_nCurTab;          // index of current tab
};
// TabView.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CTabView form view
#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

class CTabView : public CFormView, public IEventHandler 
{
protected:
	CTabView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CTabView)

	CFBDDoc* GetDocument();
	

// Form Data
public:
	//{{AFX_DATA(CTabView)
	enum { IDD = IDD_TABVIEW };
	CTabCtrl	m_ctrlTab;
	//}}AFX_DATA

// Attributes
protected:
	CView* m_pCurView;
	BOOL m_bInitialized;
	int m_nTabs;
	int m_nBottomFirstTab;
	CTabObjArray  m_tabObjects;             // Array which will hold CTabInfo CObject's


// Operations
protected:
	void ResizeAllViews();
	void SwitchTabView(int nTab);
	void FindSizeOfView(LPRECT viewRect);
	BOOL AddTabObj(CRuntimeClass* pViewClass,	 CDocument * pDoc, 
						 UINT nID, 	 LPCTSTR pszTabTitle);

	CView* CreateTabView(int nIndex, BOOL bBorder = FALSE, BOOL bShow = FALSE);

public:
	// Event playback support (IEventHandler implementation
	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTabView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CTabView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CTabView)
	afx_msg void OnSelchangeTabview(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDestroy();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // 
inline CFBDDoc* CTabView::GetDocument()
   { return (CFBDDoc*)m_pDocument; }
#endif



/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TABVIEW_H__2140CCC1_D9C3_11D1_A6D7_0000C0086DCF__INCLUDED_)
