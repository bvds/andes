#if !defined(AFX_ITEMCTRL_H__6CDC3722_EE2F_11D1_A6D7_0000C0086DCF__INCLUDED_)
#define AFX_ITEMCTRL_H__6CDC3722_EE2F_11D1_A6D7_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ItemCtrl.h : header file
//
/////////////////////////////////////////////////////////////////////////////
class CFBDDoc;
class COutlineItem;
class CStageObj;

enum Status;


/////////////////////////////////////////////////////////////////////////////
// CItemCtrl window
class CItemCtrl : public CWnd
{
// Construction
public:
	CItemCtrl();
	virtual BOOL Create(CWnd* pParentWnd, int ctrlID, CPoint pos);

	DECLARE_DYNCREATE(CItemCtrl);

// Attributes
public:
	virtual void SetText(CString text);
	virtual void SetPosition(CRect pos);
	virtual void SetId(int id);
	virtual void SetOutlineItem(COutlineItem* pItem);
	virtual void SetFocus(BOOL bFocus);

	int HitTest(CPoint point);
	
	virtual CString			GetText()		{ return m_strText; }

	virtual CRect			GetPosition()	{ return m_position; }
	virtual int				GetId()			{ return m_nId; }
	virtual COutlineItem*	GetOutlineItem() { return m_pItem; }
	virtual char			GetChar();//purely virtual
	virtual CFont*			GetFont();
	virtual BOOL IsFocused()				{return FALSE;}

	virtual void ShowMenu(UINT menuId);

	virtual void MoveControl(CRect pos);

//	CFBDDoc*	m_pDocument;
	CString		m_strText;
	CString		m_strHelp;
	int			m_nMargin;
	BOOL		m_bInitState;
//	Status GetStatus(){return m_status;};

protected:
	int				m_nId;
	CRect			m_position;
	COutlineItem*	m_pItem;
//	Status m_status;


// Operations
public:
	CStageObj* GetStage();
	int GetStageID(); 
	int GetCtrlID();
	int GetItemType();
	virtual void Enable(BOOL bEnable);

	virtual void Select(BOOL bSelect);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CItemCtrl)
	//}}AFX_VIRTUAL

// Implementation
public:
	void UpdatePosition(int x, int y);
	virtual ~CItemCtrl();

	// Generated message map functions
protected:
	//{{AFX_MSG(CItemCtrl)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// ItemCtrls
#define IDB_BTN_FIRST	14001
#define IDB_BTN_LAST	14999

//button types
#define IDB_BTN_ADDPROP		14025
#define IDB_BTN_DELPROP		14035
#define IDB_BTN_SHOWEQN		14036
#define IDB_BTN_DELSTAGE	14031

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CItemBtn window

class CItemBtn : public CItemCtrl
{
// Construction
public:
	CItemBtn();
	virtual BOOL Create(CWnd* pParentWnd, int ctrlID, CPoint pos);

	DECLARE_DYNCREATE(CItemBtn);

// Attributes
public:

	virtual char GetChar();//returns the symbolic character which is serialized
							 //for purposes of recreating the control
// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CItemBtn)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CItemBtn();

	// Generated message map functions
protected:
	//{{AFX_MSG(CItemBtn)
	afx_msg BOOL OnClicked();
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};
/////////////////////////////////////////////////////////////////////////////
#define IDS_TIME_BASE	10315
/////////////////////////////////////////////////////////////////////////////
// CItemStc window

class CItemStc : public CItemCtrl
{
// Construction
public:
	CItemStc();
	virtual BOOL Create(CWnd* pParentWnd, int ctrlID, CPoint pos);


	DECLARE_DYNCREATE(CItemStc);

// Attributes
public:
	virtual void Enable(BOOL bEnable);//Eat this in static controls
										//statics are initially enabled and remain so.

	virtual char   GetChar();//returns the symbolic character which is serialized
							 //for purposes of recreating the control
	virtual CFont* GetFont();


// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CItemStc)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CItemStc();

	// Generated message map functions
protected:
	//{{AFX_MSG(CItemStc)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////
//CCheckedItem

class CCheckedItem : public CItemCtrl
{
// Construction
public:

	CCheckedItem();
	DECLARE_DYNCREATE(CCheckedItem);

	
	virtual void CheckObject() {}
	virtual void OnEditProperties(int id) {}
  // 	virtual void NotifyDelete() {}			// Notify help system object has been deleted
  // 	virtual void NotifyChange() {}	// Update help system on some change.
   	virtual CDialog* GetPropertyDlg()			// return initialized type-specific dialog
   			{ return NULL; }				// no default dialog in base
   	
	// Following is 1-arg method for ctrlitems
	virtual void ApplyStatus(LPCTSTR pszResult); 

	COLORREF StatusColor(Status status);
	Status GetStatus(){return m_status;};

protected:
	Status m_status;

};

/////////////////////////////////////////////////////////////////////////////
// Cell states
#define CCIS_FOCUSED            0x0001
#define CCIS_SELECTED           0x0002
#define CCIS_DISABLED		    0x0004

/////////////////////////////////////////////////////////////////////////////
// CCellCtrl window
class CTableRow;
class CCheckedObj;
typedef CTypedPtrList<CObList, CCheckedObj*> CObjList;

class CCellCtrl : public CCheckedItem
{
// Construction
public:
	CCellCtrl();
	
	virtual BOOL Create(CWnd* pParentWnd, int ctrlID, CPoint pos);

	DECLARE_DYNCREATE(CCellCtrl);

	virtual char GetChar();//returns the symbolic character which is serialized
							 //for purposes of recreating the control
	virtual void CheckObject();
	virtual void SetFocus(BOOL bFocus);


// Attributes
public:
    UINT    state;         // Cell state (selected/focus etc)
  //  LPARAM  lParam;        // 32-bit value to associate with item

	//for drawing purposes
	BOOL IsPressed();
	BOOL IsFocused();
	BOOL IsDisabled();
//	BOOL m_MouseOnBtn;
	CObjList m_properties;//contains list of table rows -> property variable, image list & string list
	//however, we have no use for image list and strings here.
	void AddProperty(CCheckedObj* pProp);


// Operations
public:
	virtual void Select(BOOL bSelect);
	virtual void Enable(BOOL bEnable);
	virtual void SetText(CString text);
	void OnEditProperties(int id);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCellCtrl)
	public:
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CCellCtrl();

	// Generated message map functions
protected:
	//{{AFX_MSG(CCellCtrl)
	afx_msg void OnPaint();
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	//}}AFX_MSG
	afx_msg BOOL OnClicked();
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_ITEMCTRL_H__6CDC3722_EE2F_11D1_A6D7_0000C0086DCF__INCLUDED_)
