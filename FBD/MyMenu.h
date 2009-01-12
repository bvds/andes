//MyMenu.h
//Header file for My owner-drawn menus


class CMyMenu : public CMenu
{
public:

	CMyMenu();
	DECLARE_DYNCREATE(CMyMenu);

	virtual void MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct);
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	virtual void ChangeMenuItem(UINT nID, LPCTSTR lpz);

	virtual ~CMyMenu();

};

class CGreekMenu : public CMyMenu 
{
public:

	CGreekMenu();
	DECLARE_DYNCREATE(CGreekMenu);

	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	virtual void ChangeMenuItem(UINT nID, LPCTSTR lpz);

	virtual ~CGreekMenu();

};

class CPinkMenu : public CMyMenu
{
public:

	CPinkMenu();
	DECLARE_DYNCREATE(CPinkMenu);

	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);
	virtual void ChangeMenuItem(UINT nID, LPCTSTR lpz);
	
	virtual ~CPinkMenu();

};

class CVarMenu : public CMyMenu
{
public:
	static int MakeVectorPropCode(LPCSTR pszProp, LPCSTR pszVectorType);
	int SplitQuantId(int nID, CString& strVecProp, CString& strPrefix);
	CVarMenu();
	DECLARE_DYNCREATE(CVarMenu);

	void AttachProblemMenu(DWORD dwConceptFlag, BOOL b_IncludeVectors=FALSE);

	virtual ~CVarMenu();
};
