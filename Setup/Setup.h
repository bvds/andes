// Setup.h : main header file for the SETUP application
//

#if !defined(AFX_SETUP_H__5201E765_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_SETUP_H__5201E765_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

#define		ID_SKIPALL	3

/////////////////////////////////////////////////////////////////////////////
// CSetupApp:
// See Setup.cpp for the implementation of this class
//
class CMySheet;

class CSetupApp : public CWinApp
{
public:
	BOOL AcceptLicense();
	CSetupApp();

	// Operating system info:
	DWORD m_dwOS;		// major OS family: VER_PLATFORM_WIN32_NT => NT/2000/XP 
	//					//                  VER_PLATFORM_WIN32_WINDOWS => 95/98/Me
	BOOL m_bWin2000orXP;// TRUE if Win2000 or XP

	//try to open dll's, report errors as a side note if existing Dll's not current or DNE
	BOOL HasCurrentDLL(CString newDLL, CString oldDLL);
	//function does actual version comparison.  Returns true if has same version or newer
	BOOL CheckDLL(CString newFile, CString oldFile);

	BOOL ExistingFileNewer(CString strNewFile, CString strOldFile);//compare time stamp

	BOOL m_bReboot;
	CString GetProductVersion(CString strFile);
	CString GetFileVersion(CString strFile);

	void GetFolderPaths(CString& winPath, CString& progGroupPath);

	BOOL CreateDirectories();
	BOOL CreateShortcuts();
	BOOL MakeRegEntries();
	CString m_strInstDir;//the directory where Andes will be installed
	CString m_strCurDir;//the current directory 

	int m_nFiles;//number of files that will be copied
	BOOL m_bSkipNewer;
	BOOL CheckDiskSpace();//Reads the files, sums up their size,
						//checks space left on disk & compares the two
						//Also counts # of files and stores in m_nFiles	
	BOOL CopyTheFiles();
	void RenameOnStartup(LPCTSTR szNewName, LPCTSTR szOldName);

    BOOL FileExists(LPCTSTR lpsz);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSetupApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CSetupApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
protected:
	BOOL m_bCopyFilesOnly;
	void CleanUpSourceFiles();
	CMySheet* m_pSheet;
};

extern CSetupApp theApp;

#define IDERROR		3
/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SETUP_H__5201E765_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
