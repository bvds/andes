// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
// 
// $Id: Stdafx.h,v 1.1 2005/01/24 16:28:10 bvds Exp $

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#define OEMRESOURCE
#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxtempl.h>		// MFC collection templates
#include <afxdisp.h>        // MFC OLE automation classes
#include <afxole.h>         // MFC OLE 2.0 support
#include <afxodlgs.h>       // MFC OLE 2.0 dialog support
#include <afxsock.h>		// MFC Winsock support

#include <shlobj.h>

#ifndef _AFX_NO_AFXCMN_SUPPORT
#include <afxcmn.h>			// MFC support for Windows 95 Common Controls
#include <afxcview.h>
#endif // _AFX_NO_AFXCMN_SUPPORT

#include <afxpriv.h>		// MFC internal defs, used by CG generated clipboard code

#include <mmsystem.h>		// MCI defs, used for PlaySound, audio logging


// #ifdef ROVING		// Used in roving student version only 
#include <afxinet.h>		// MFC WinInet lib wrappers, for auto upload/download
// #endif // ROVING

#ifdef HELPIFC_TCP	// Used in Wizard of OZ version only

// ATL currently only used by the NetMeeting COM object wrapper classes used 
// to implement application sharing via NetMeeting COM interfaces.
// Don't need them if not using Netmeeting code.
#if	0			// not needed since not using Netmeeting anymore
// Bring in ATL template definitions
#include <atlbase.h>
extern CComModule _Module;	// required to compile ATL macros
#include <atlcom.h>

// NetMeeting COM interfaces, generated from type library
#include "NetMeeting/imsconf2.h"
#endif 0		// end no-longer-used netmeeting includes

#endif // HELPIFC_TCP
