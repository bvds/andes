// LispReader.h: interface for the CLispReader class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_LISPREADER_H__0055A6DF_884B_481D_951C_62B8B55BFA14__INCLUDED_)
#define AFX_LISPREADER_H__0055A6DF_884B_481D_951C_62B8B55BFA14__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000



class CLispReader  
{
public:
	CLispReader(FILE* fp);
	virtual ~CLispReader();

	class Obj: public CObject // generic base class for lisp objects we can read
	{
	public:
		virtual BOOL IsList() = 0;
		BOOL         IsAtom() { return ! IsList(); }
		virtual ~Obj() {};
	};

	class Atom : public Obj
	{
	public:
		Atom(const CString& strValue) : m_strValue(strValue) {};

		virtual BOOL IsList() { return FALSE; }
		CString m_strValue;
	};
    
	typedef CTypedPtrList<CObList, Obj*> LispObjList;

	class List: public Obj
	{
	public:
		virtual BOOL IsList() { return TRUE; }
		LispObjList m_objects;

		void Append(Obj* pObj) { m_objects.AddTail(pObj); }
		virtual ~List() {
			// delete the objects
			while (!m_objects.IsEmpty()) {
   				delete m_objects.RemoveHead();
			}
		}
	};

	CString GetToken();
	Obj*    GetObject();

// Implementation:
	FILE* m_fp;
};

#endif // !defined(AFX_LISPREADER_H__0055A6DF_884B_481D_951C_62B8B55BFA14__INCLUDED_)
