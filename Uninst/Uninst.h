// Uninst.h : main header file for the UNINST application
//

#if !defined(AFX_UNINST_H__7E891B87_4A0D_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_UNINST_H__7E891B87_4A0D_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CUninstApp:
// See Uninst.cpp for the implementation of this class
//

class CUninstApp : public CWinApp
{
public:
	CString m_strInstDir;
	CUninstApp();

	DWORD m_dwOS;//current OS, VER_PLATFORM_WIN32_NT or VER_PLATFORM_WIN32_WINDOWS (win 95)

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CUninstApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CUninstApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

extern CUninstApp theApp;
/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_UNINST_H__7E891B87_4A0D_11D1_A09F_0000C0086DCF__INCLUDED_)
