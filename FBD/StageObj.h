// CStageObj.h - header file

#ifndef STAGEOBJ_INCLUDED
#define STAGEOBJ_INCLUDED 1

class CHiLevelVw;
class CItemCtrl;
class CCellCtrl;
class CStageObj;

#include "MyGrids.h"
#include "Logedit.h"	// Added by ClassView
/////////////////////////////////////////////////////////////////////////////
// COutlineItem window
/*typedef struct { 
   int id; 
	char* str; 
} ITEMHELP; 

extern const ITEMHELP itemHelp[];
extern const int nItemHelps;*/

typedef struct { 
    int id; 
    char ctrlType; 
	char* str; 
} OUTLINEFORMAT; 

extern const OUTLINEFORMAT items[];
extern const int nItems;



class COutlineItem : public CObject
{
// Construction
public:
	DECLARE_SERIAL(COutlineItem);
	COutlineItem();
	COutlineItem(int nType);
	void Serialize(CArchive& ar);
	void UpdateDatas();//helper function for serialization of controls
	CCtrlData* NewData(char chType, CString strText);

// Attributes
public:
	int m_nItemType;//following are the item types

	#define ITEMTYPE_FIRST		4
	#define ITEM_LAW			4
	#define ITEM_SYSPROP		5
	#define ITEM_DIRECTION		6
	#define ITEM_CHKSOLV		7
	#define ITEMTYPE_LAST		ITEM_CHKSOLV
	#define ITEM_TABLE			9


	int		GetId() { return m_nItemType; }
	void	SetId(int id);

	CRect	m_position;//Absolute position
	CRect	GetPosition() { return m_position; }
	void	SetPosition(CRect pos);
	void	UpdatePosition(COutlineItem* pInsAfter);

	CStageObj* m_pStage;//pointer back to stage object
	CStageObj* GetStage() { return m_pStage; }

	CString GetPrintDef();
	CString GetTipDef();

	typedef CTypedPtrArray<CObArray, CItemCtrl*>	CCtrlArray;
	CCtrlArray	m_controls;//Array of controls in the item

	typedef CTypedPtrList<CObList, CCtrlData*>		CCtrlDataList;
	CCtrlDataList m_datas;//list of control item data, used for serialization only
							//contains the info to create the controls

	int HitTest(CPoint point);

private:

// Operations
public:

	void SetFocus();

	void Enable(BOOL bEnable);

	void MoveItem(int x, int y);
	void UpdateCtrlsthatFollow(CItemCtrl* pCtrl);//when an item's controls changes size,
												//we need to update the horizontal position 
												//of the controls that follow

//	BOOL		AddControls(int nItemType, CHiLevelVw* pView);
	CItemCtrl*	AddControl(CHiLevelVw* pView, char ch, CString str);
	void		AddDatas(int nItemType);

	CCellCtrl*	ReplaceCellText(CString str);
	CMenuGrid*	GetFocusedGrid();
	CCellCtrl*	GetCell();
	CGridCtrl*  GetGrid();

	CTableGrid* AddTable(int nId);//Addtable updates the control's data list
									//do not use this function in ReadInTable.
	CTableGrid* ReadInTable(CCtrlData* pData);//read in the table saved in the control data

   
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COutlineItem)
	//}}AFX_VIRTUAL

// Implementation
public:
	CItemCtrl* CtrlAt(const CPoint& point);
	virtual ~COutlineItem();

};

typedef CTypedPtrArray<CObArray, COutlineItem*> CItemArray;
typedef CTypedPtrList<CObList, CItemCtrl*>		CCtrlList;

class CPrincObj;
/////////////////////////////////////////////////////////////////////////////
// CStageObj

class CStageObj : public CObject
{
// Construction
public:
	DECLARE_SERIAL(CStageObj);
	CStageObj();
	void Serialize(CArchive& ar);
	// Attributes
public:
	int m_nId;					//unique identifier of this stage

	CItemArray	m_items;		//Array of OutlineItems in the stage
	CHiLevelVw* m_pView;		//pointer back to hi level solution view
	CCtrlList	m_tables;		//List of tables in the stage
	CTableGrid* m_pFocusedTable;//The table with the focus

	CPrincObj*	m_pPrinc;	//the law we are applying in this stage
	CFBDDoc*	m_pDocument;
	int			m_nEqs;//number of equations (directions selected)

// Operations
public:

	void			AddTemplate();
	COutlineItem*   InsertTableItem();

	COutlineItem*	GetItem(int nID);//returns item with nID
	COutlineItem*   GetNext(int nID);//returns next item after item with nID
	COutlineItem*	GetLast();		 //returns last item

	CTableGrid*		GetTable(int nId);

	void AddTable(CTableGrid* pTable);
	void DeleteTable(CTableGrid* pTable);

	void OnPropertyDelete();
//	void CollapseTables();//when "done/undo done" clicked
	void HighlightTables(BOOL bHighlight, int nID);
	
	//when an item changes size (table height when props are added or deleted), we need to 
	//update the vertical position of the items that follow
	void UpdateItemsthatFollow(COutlineItem* pItem);
													
	CRect	GetPosition();				//returns position rect of stage

	//helper function to control enabling/disabling of the system property buttons
	void EnableBtn(BOOL bEnable, UINT nBtnID);
	void Enable(BOOL bEnable);
	//helper function to help with positioning of stages when previous stages are deleted
	//or change size (ie properties added)
	void	MoveStage(int x, int y);

protected:
	void	Delete(COutlineItem* pItem);//deletes the item and its controls


// Overrides

	// Implementation
public:
	virtual ~CStageObj();

};

///////////////////////////////////////////////////////////////////////////////
// CPrincItem: Common base class for nodes in principle window tree
//
// Used to define a single generic type for data attached to tree items.
// Has a "fat" interface to be used for both parent and child nodes
// so as to allow recursive treatment if desired. (We don't currently need such
// generality for the trivial structure of our principle tree, but it's
// easy enough to do and allows for easy extensibility if we ever want it.)
// 
class CPrincItem : public CObject
{
public:
	DECLARE_SERIAL(CPrincItem);
	CPrincItem() { m_pParent = NULL; 
				   m_bChecked = FALSE; 
				   m_status = statusUnknown; 
				   m_checkStatus = statusUnknown; }
	virtual ~CPrincItem();
	void		Serialize(CArchive& ar);

	BOOL		m_bChecked;		// Checked "done" by user
	Status		m_status;		// Entry status as checked by help system	
	Status		m_checkStatus;	// if checked, status of check

	// to fetch text to display for item
	virtual CString GetDisplayString() { return ""; } // none in base class 

	// to fetch associated plan stage id string for help system calls
	virtual CString GetStageId() { return ""; } 

	// For parent items: List of subitems 
	CTypedPtrList<CObList, CPrincItem*> m_subItems;
	void AddSubItem(CPrincItem* pSubItem)
		{ m_subItems.AddTail(pSubItem);
		  pSubItem->m_pParent = this; }
	void RemoveAllSubItems();
	BOOL HasSubItems() { return m_subItems.GetCount() > 0; }
	// Handle state change in child items:
	virtual void OnChildStateChange() {};

	// For child items: back pointer to parent
	CPrincItem* m_pParent;
	BOOL IsSubItem()	{ return m_pParent != NULL; }
};

//
// CPrincObj -- Principle to be applied (top-level PrincItem).
//
class CPrincStep;		// forward ref

class CPrincObj : public CPrincItem  
{
// Construction
public:
	DECLARE_SERIAL(CPrincObj);
	CPrincObj(BOOL bCreateStage = FALSE);
	void Serialize(CArchive& ar);

// Attributes
public:
	CFBDDoc*	m_pDocument;
	CString		m_strLaw;
	CStageObj*	m_pStageObj;
	CStringList m_listBodies;
	CString		GetBodyStr() const; // get bodies as single sp-delimited string
	CStringList m_errors;	// list of specific errors (for dialog)
	
	// String constants for law names (long, human-readable)
	static const char szKinematics[]; 
	static const char szNewtonsFirst[]; 
	static const char szNewtonsSecond[]; 
	static const char szConsOfEnergy[];
	static const char szNetWork[];

	CString GetHelpSysName();	// gets shorter law id used by help system 
	
// Operations
public:
	void SetBodies(const CStringList& bodies);

// Overrides
public:
	virtual CString GetDisplayString();
	virtual void OnChildStateChange();
	virtual CString GetStageId() 
	{	CString strStageId;	
		ASSERT(m_pStageObj); 
		strStageId.Format("stage-%d", m_pStageObj->m_nId);
		return strStageId; 
	}

// Implementation
public:
	void CheckObject();
	void SetLaw(LPCTSTR szLawName);
	virtual ~CPrincObj();

protected:
	// Worker to add a substep
	CPrincStep* AddStep(const CString& strText);
	// currently never remove substeps

	void LoadV1(CArchive& ar);
};

//
// CPrincStep -- sub step in applying given principle
//
class CPrincStep: public CPrincItem 
{
// Construction
public:
	DECLARE_SERIAL(CPrincStep); 
	CPrincStep()  {};
	CPrincStep(const CString& strText)  : m_strText(strText) {};
	void Serialize(CArchive& ar);

// Attributes
	CString m_strText;
	virtual CString GetDisplayString() { return m_strText; }
	virtual CString GetStageId()
	{	// just get it from parent principle	
		ASSERT(m_pParent);
		ASSERT_KINDOF(CPrincObj, m_pParent);
		return ((CPrincObj*) m_pParent)->GetStageId(); 
	}

	// String constants for law names (long, human-readable)
	static const char szFBD[]; 
	static const char szEquations[]; 

	CString GetStepId();	// get shorter help system step id

// Operations
// Implementation
public:
	virtual ~CPrincStep();
};
#endif STAGEOBJ_INCLUDED
