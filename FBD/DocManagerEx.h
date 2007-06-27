// DocManagerEx.h: interface for the CDocManagerEx class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_DOCMANAGEREX_H__8E887B03_57A7_11D2_985B_0060088F5D43__INCLUDED_)
#define AFX_DOCMANAGEREX_H__8E887B03_57A7_11D2_985B_0060088F5D43__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

// MultiExt: New CDocManager-derived class
class CDocManagerEx : public CDocManager  
{
	DECLARE_DYNAMIC(CDocManagerEx)
public:
	CDocManagerEx();
	virtual ~CDocManagerEx();

// Document functions
	virtual void RegisterShellFileTypes(BOOL bCompat);
	void UnregisterShellFileTypes();

/*
    CDocument* OpenDocumentFile(LPCTSTR lpszFileName);
	// helper for standard commdlg dialogs
	virtual BOOL DoPromptFileName(CString& fileName, UINT nIDSTitle,
			DWORD lFlags, BOOL bOpenFileDialog, CDocTemplate* pTemplate);
*/
};

#endif // !defined(AFX_DOCMANAGEREX_H__8E887B03_57A7_11D2_985B_0060088F5D43__INCLUDED_)
