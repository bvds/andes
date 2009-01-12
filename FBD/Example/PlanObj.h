//PlanObj.h  : header file
//
//
// System objects:
//
#ifndef PLANOBJ_INCLUDED
#define PLANOBJ_INCLUDED 1


typedef CList<int, int> CIndexList;

class CPlanItem: public CObject
{
protected:
	
	DECLARE_SERIAL(CPlanItem);
	CPlanItem();

public:
	void Serialize(CArchive& ar);
	void SerializeList(CArchive &ar);


// Attributes
public:
	int	 GetLevel();
	void SetLevel(int level);

	int	 GetSearchType();
	void SetSearchType(int type);

	CString GetText();
	void	SetText(CString text);

	int	 GetImage();
	void SetImage(int image);

	CPlanItem* GetParent();
	void	   SetParent(CPlanItem* pParent);


private:
	WORD			m_itemLevel;
	WORD			m_itemSearchType;	
	CString			m_itemText;
	DWORD			m_itemImage;
	CPlanItem*		m_itemParent;
//	CIndexList*		m_pLinkedList;
	CString			m_position; // May want to use in EXPlan




};


typedef CTypedPtrList<CObList, CPlanItem*> CPlanItemList;
/////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
// CPlanObj: 
///////////////////////////////////////////////////////////////////////////
class CPlanObj : public CObject
{
protected:
	DECLARE_SERIAL(CPlanObj);
	CPlanObj();
	
// Constructors
public:

	void Serialize(CArchive& ar);

// Attributes
//	
	CPlanItemList m_items;

// Operations
};

#endif PLANOBJ_INCLUDED