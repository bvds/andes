// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__5201E769_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_STDAFX_H__5201E769_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxdisp.h>        // MFC OLE automation classes
#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows Common Controls
#endif // _AFX_NO_AFXCMN_SUPPORT
#include <afxpriv2.h>		// needed for CArchiveStream by CPicture

//#include <stdio.h>
//#include <conio.h>
#include <process.h>

#include <afxole.h>         // MFC OLE 2.0 support

#include <winnls.h>

#include <shlwapi.h>

#include <malloc.h>			// for _alloca, used in RunningAsAdministrator


//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__5201E769_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
