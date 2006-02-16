#if !defined(AFX_VARVIEW_H__332AD501_5A9F_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_VARVIEW_H__332AD501_5A9F_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// VarView.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CVarView form view

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

#include "Childfrm.h"
#include "ListVwEx.h"

class CDrawObj;
class CVariable;
class CVector;

class CVarView : public CListViewEx, public IEventHandler
{
protected:
	CVarView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CVarView)

// Form Data
public:
	

// Attributes
public:
	// The list of checked objects that influence the varview 
	CChkObjList  m_VarList;
	BOOL		IsAxesDrawn();


protected:
	CImageList m_imgStatus;//icons showing variable status
	CFBDDoc* GetDocument();

	void SizeToFit(int nCol, CString str);//size list box column to fit	
								//inserted string
	void InsertTimeVariables();
	CVariable* CheckInVarList(CCheckedObj* pVar);

	CString m_strVar;
//	CGreekMenu m_GrkMenu;
// Operations
protected:
	void DeleteListItem(CCheckedObj* pObj);//delete item, remove from varlist
	void InsertListItem(CCheckedObj* pObj);//insert item, update components, set data
	void UpdateListItem(CCheckedObj* pRealObj, CCheckedObj* pTempObj);

	void UpdateAllComponents();//updates all x and y components in list
	void UpdateComponents(CCheckedObj* pObj);//updates pObj's components
	CVector* FindCompOf(CString strCompOf);
	
	void PopulateList();//populate list from document's object and variable lists

	int FindIndex(CCheckedObj* pObj);//Find Index of given object
	int FindLabel(LPCTSTR pszLabel);

public:
	// quantity table accessors:
	static CString LookupTypeId(int nID);
	static CString LookupStrValue(int nID);
	static int LookupId(CString strTypeId);
	static int ValueToId(CString strValue);
	static CString LookupPrefix(int nID);
	static CString LookupSpec(int nID);
	static CString LookupDlgValue(int nID);
	// initializer
	static void InitQuantTable();
	// for building menu of add variable commands
	static BOOL HasFeature(CString strTypeId);
	static void AddScalarVars(CMenu* pMenu, BOOL bQuantityChoice);
	

private:
	// quantity table implementation
	static void AddQuant(int nID, CString strName, CString strPrefix, CString strValue, 
		                 CString strDlgPrefix, CString strSpec);
	static void LoadQuantInfo(LPCSTR pszPathName);
	// for loading feature-set abbreviations
	static void AddFeatureSet(CString strName, CString strFeatureList);
	static void LoadFeatureSets(LPCTSTR pszPathName);

public:	
	CString GetVecDirString(CVector* pVec);
	BOOL m_bEnabled;
	void EnablePane(BOOL bEnable);
	// Event playback support (IEventHandler implementation
	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);
	virtual void PointToObject(LPCTSTR pszObjID);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVarView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	//}}AFX_VIRTUAL

// Implementation
protected:
	void UpdateAngleVarDir(CCheckedObj* pObj);
	void UpdateAxisDir(CDrawObj* pAxes);
	virtual ~CVarView();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CVarView)
	afx_msg void OnVariableModify();
	afx_msg void OnUpdateVariableModify(CCmdUI* pCmdUI);
	afx_msg void OnUpdateVariableDelete(CCmdUI* pCmdUI);
	afx_msg void OnVariableWhatswrong();
	afx_msg void OnUpdateVariableWhatswrong(CCmdUI* pCmdUI);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnContextMenu(CWnd* pWnd, CPoint point);
	afx_msg void OnItemchanged(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnUpdateVariableAddtime(CCmdUI* pCmdUI);
	afx_msg void OnUpdateEditDelete(CCmdUI* pCmdUI);
	afx_msg void OnVariableDelete();
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnUpdateVariableAddangle(CCmdUI* pCmdUI);
	afx_msg void OnVariableSolvefor();
	afx_msg void OnUpdateVariableSolvefor(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg void OnVariableNew(UINT nID);
	afx_msg void OnUpdateVariableNew(CCmdUI* pCmdUI);

	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in eqtest1View.cpp
inline CFBDDoc* CVarView::GetDocument()
   { return ((CChildFrame*)GetParentFrame())->m_pDoc; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VARVIEW_H__332AD501_5A9F_11D1_A09F_0000C0086DCF__INCLUDED_)
