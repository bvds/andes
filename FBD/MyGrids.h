///////////////////////////////////////////////////////////////////////////////////
//
// CMyGrids.h
//
//////////////////////////////////////////////////////////////////////////////////
#ifndef MYGRIDS_INCLUDED
#define MYGRIDS_INCLUDED 1

#define ID_HEADER_FORCE			0
#define ID_HEADER_KINEMATICS	1
#define ID_HEADER_OTHER			2

class CHiLevelVw;

/////////////////////////////////////////////////////////////////////////////
// There are two Types of grid controls, a Table and a Menu
#include "GridCtrl.h"
#include "DrawObj.h"

class CTableGrid;

class CTableRow : public CVariable
{
//Construction
public:
	CTableRow();
	DECLARE_SERIAL(CTableRow);
	
	void Serialize(CArchive& ar);
	void SerializeLists(CArchive &ar);

//operations
	virtual CDialog* GetPropertyDlg();
	virtual CDrawObj* Clone();
	virtual BOOL OnEditProperties();
	virtual void NotifyDelete();
	void NotifyChange();
	void CheckObject();
	int SetType(int nID);

	virtual BOOL InVarList()			
			{ return FALSE; }//all checked obj's in list unless overrided


//attributes
	//unknown, known, sought
	CIntList	m_images;//holds list of images (in value magnitude direction columns)
	//formulas & equivalent quantities->(holds m_strId)
	CStringList m_strings;//holds list of strings (in value magnitude direction columns)
	CTableGrid* m_pTable;//back pointer to contain parent table
//Implementation
public:
	virtual ~CTableRow();

};

typedef CTypedPtrList<CObList, CTableRow*> CPropList;

class CTableGrid : public CGridCtrl
{
	// Construction
public:
	CTableGrid();
	CTableGrid(int nID);

	// Override this function to fill InPlaceListBoxes
	virtual BOOL Create(CWnd* pParentWnd, int ctrlID, CPoint pos);
	void	Initialize();

	DECLARE_DYNCREATE(CTableGrid);

//attributes
	CPropList m_properties;//contains list of table rows -> property variable, image list & string list

	virtual char GetChar();//returns the symbolic character which is serialized
							 //for purposes of recreating the control

	virtual void CheckObject();

//operations	
public:
	virtual void Print(CDC* pDC, CPrintInfo* pInfo);
	virtual BOOL DrawCell(CDC* pDC, int nRow, int nCol, CRect rect, BOOL bEraseBk=FALSE);

	void AddHeader(int nId);
	void AddProperty(CTableRow* pProp);
	void DeleteProperty();
	void RemoveProperty(CTableRow* pProp);

	void Highlight(BOOL bHighlight);

private:
	BOOL m_bOldEnabledState;//when highlighting, remember old enabled state
	BOOL m_bHighlight;//TRUE when highlighted
public:
	BOOL IsHighlighted() {return m_bHighlight;}

//	BOOL m_bCollapsed;//keeps track of toggle
//	void ToggleCollapse();
	void Deselect();

	virtual void Enable(BOOL bEnable);

	CHiLevelVw* GetParent();

protected:
	int  AddRow(CString txt);
	void SelectRow(int nRow);

	void FillListItems(int nCol, LPARAM cltList);
	void InsertRichText(CDC* pDC, LPCTSTR lpString, int nCount, LPRECT lpRect, UINT uFormat);
	
	// Implementation
public:
	virtual ~CTableGrid();
	virtual void OnEndEditCell(int nRow, int nCol, CString str);
	CTableRow* GetMatchingProp(CString strDef);

// Generated message map functions
protected:
	//{{AFX_MSG(CTableGrid)
	//}}AFX_MSG
	afx_msg BOOL OnClicked();
	DECLARE_MESSAGE_MAP()

};

class CMenuGrid : public CGridCtrl
{
	// Construction
public:
	CMenuGrid();
	virtual BOOL Create(CWnd* pParentWnd, int ctrlID, CPoint pos);
	DECLARE_DYNCREATE(CMenuGrid);

//attributes
	virtual char GetChar();//returns the symbolic character which is serialized
							 //for purposes of recreating the control

	virtual void CheckObject();

	CString GetHelpString();//Need lisp string counterpart to actual law text

	//for drawing purposes
	BOOL m_bPressed;
	BOOL IsDisabled();
	BOOL IsPressed();
	BOOL IsFocused();
	virtual void SetFocus(BOOL bFocus);
	
	//override of drawing so that menu grids look lik the cell buttons
    virtual void  OnDraw(CDC* pDC);

// Operations
public:
	void FillListItems(int nCol, LPARAM cltList);
	void Select(BOOL bSelect);
	
	virtual void OnEndEditCell(int nRow, int nCol, CString str);

	virtual void Enable(BOOL bEnable);

// Implementation
public:
	virtual ~CMenuGrid();

// Generated message map functions
protected:
	//{{AFX_MSG(CMenuGrid)
	//}}AFX_MSG
    afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
    afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	DECLARE_MESSAGE_MAP()

};
 

/////////////////////////////////////////////////////////////////////////////
class CCtrlData : public CObject
{
// Construction
public:
	DECLARE_SERIAL(CCtrlData);
	CCtrlData();
	void Serialize(CArchive& ar);
	void Update(CItemCtrl* pCtrl);

// Attributes
public:
	char    m_chType;
	CString m_strLabel;
//	BOOL	m_bEnabled;

	int		m_bInitState;//for cellctrls&btns-hold m_bInitBtn, for tables, m_bCollapsed
	int		m_nTableId;//exist only for table grids
	
	CObjList		m_properties;//exist only for table grids, list of properties
	//Going to adjust so that m_properties reflect sought and system.  Of course,
	//this list will only include one item.
	COutlineItem*	m_pItem;//pointer back to the item

	CFBDDoc* m_pDocument;
// Implementation
public:
	virtual ~CCtrlData();

};

#endif MYGRIDS_INCLUDED
