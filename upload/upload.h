// upload.h : main header file for the UPLOAD application
//

#if !defined(AFX_UPLOAD_H__0EFAAC85_2A04_11D1_BC04_0000C037C67D__INCLUDED_)
#define AFX_UPLOAD_H__0EFAAC85_2A04_11D1_BC04_0000C037C67D__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

/////////////////////////////////////////////////////////////////////////////
// CUploadApp:
// See upload.cpp for the implementation of this class
//

class CUploadApp : public CWinApp
{
public:
	CUploadApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CUploadApp)
	public:
	virtual BOOL InitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CUploadApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_UPLOAD_H__0EFAAC85_2A04_11D1_BC04_0000C037C67D__INCLUDED_)
