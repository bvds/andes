// PlanObj.cpp : Implementation file

#include "stdafx.h"

#include "FBD.h"
#include "FBDDoc.h"
#include "history.h"
#include "PlanObj.h"



////////////////////////////////////////////////////////////////
//	CPlanObj
////////////////////////////////////////////////////////////////


IMPLEMENT_SERIAL(CPlanObj, CObject, VERSIONABLE_SCHEMA | 1)
CPlanObj::CPlanObj()
{}			// used by serialization only

//If add newer versions of this class, note base classes must use SerializeClass 
// to archive version info for derived class! See documentation.


void CPlanObj::Serialize(CArchive& ar)
{
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	CObject::Serialize(ar);
	if (ar.IsLoading())
	{
		// restore document back pointer from archive struct
		if (nDocVersion >= 2)
		{
			DWORD wTemp;
			// Couldn't get class version cause started with 0
			// had to "up" document version
			if (nDocVersion < 9)
				ar >> wTemp; // nTreeItems
		
		}
		else
			AfxThrowArchiveException(CArchiveException::badSchema);

	}
	m_items.Serialize(ar);

}

//////////////////////////////////////////////////////////////////////////
//   
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CPlanItem, CObject, VERSIONABLE_SCHEMA | 5);
CPlanItem::CPlanItem()
{
	m_itemSearchType = NULL;
//	m_pLinkedList = NULL;
}

void CPlanItem::Serialize(CArchive& ar)
{

	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	// document versions >= 1 added serialization of base class descriptor
	CObject::Serialize(ar);
	
	if (ar.IsStoring())
	{						     // store object specific data
		ar << m_itemImage;
		
		ar << m_itemParent;
		ar << m_itemText;
		ar << m_itemLevel;
		ar << m_itemSearchType;
		ar << m_position;// may want to use in EXPlan
	} 
	else 
	{							// load object specific data
		if (nDocVersion >= 2)
		{
			DWORD temp;
			ar >> m_itemImage;
			if (nClassVersion ==0)
				ar >> temp;
			ar >> m_itemParent;
			ar >> m_itemText;
			m_itemLevel = LOWORD(temp);
			m_itemSearchType = HIWORD(temp);
			m_position = "0"; //may want to use in EXPlan
			
		} 
		if (nDocVersion >=3)
		{
			WORD temp;
			if (nClassVersion < 4){
				ar >> temp;//m_linkedObj
				ar >> temp;//m_linkedEQ
			}
			if(nClassVersion > 0)
			{
				ar >> m_itemLevel;
				ar >> m_itemSearchType;
			}
			if(nClassVersion > 1)
			{
				ar >> m_position;//may want to use in EXPlan
				if (nClassVersion < 4)
					ar >> temp;//m_status
			}
			if (nClassVersion == 3)
			{
				ar >> temp;//m_linkedVar
			}
			if (nClassVersion <= 4)
				SerializeList(ar);

		}
	}
}

void CPlanItem::SerializeList(CArchive &ar)
{
	
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	if (nDocVersion>=3)
	{
			DWORD size;
			ar >> size;
			DWORD index;
			if (size != 0){	
				 CIndexList* pLinkedList = new CIndexList;//create new integer list of list box selections 
				for (UINT i =0; i<size; i++){
					ar >> index;
					pLinkedList->AddTail(index);
				}
				delete pLinkedList;

			}
	}
	
}

void CFBDDoc::DeletePlanItems()
{
	//when in example edit, the plan view deletes all the planitems
	//when STUDYING an example, these are deleted here
	//this is called in the Document destructor
	POSITION pos = m_plan.m_items.GetHeadPosition();
	while (pos != NULL)
	{
		CPlanItem* pPlanItem = m_plan.m_items.GetNext(pos);
		delete pPlanItem;
	}
	m_plan.m_items.RemoveAll();
	return;
}

void CPlanItem::SetLevel(int level)
{
   m_itemLevel = level;
}

int CPlanItem::GetLevel()
{
   return (m_itemLevel);
}
void CPlanItem::SetImage(int image)
{
   m_itemImage = image;
}

int CPlanItem::GetImage()
{
   return (m_itemImage);
}

void CPlanItem::SetSearchType(int type)
{
   m_itemSearchType = type;
}

int CPlanItem::GetSearchType()
{
   return (m_itemSearchType);
}

void CPlanItem::SetParent(CPlanItem* pParent)
{
   m_itemParent = pParent;
}

CPlanItem* CPlanItem::GetParent()
{
   return (m_itemParent);
}

void CPlanItem::SetText(CString text)
{
   m_itemText = text;
}

CString CPlanItem::GetText()
{
   return (m_itemText);
}


