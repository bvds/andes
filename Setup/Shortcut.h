// Shortcut.h : header file
//

// CShortcut
// Written 1996 by Rob Warner
// rhwarner@southeast.net
// http://users.southeast.net/~rhwarner
// Distribute freely, modify to your heart's content <g>
// Let me know if you find it useful, or if you've improved it <vbg>

// To use: You must #include <afxole.h> in your stdafx.h.
//         You must also initialize/uninitialize COM.  One way to do this is
//         in your derived CWinApp::InitInstance(), add the following line:
//
//          ::CoInitialize(NULL);
//
//         And in your derived CWinApp::ExitInstance(), add the following line:
//
//          ::CoUnitialize();
//

/////////////////////////////////////////////////////////////////////////////
// CShortcut

#ifndef _INC_SHORTCUT
#define _INC_SHORTCUT

#include <winnetwk.h>
#include <shlobj.h>
#include <shellapi.h>


typedef struct tagSHORTCUTSTRUCT {
    CString strPath;
    CString strTarget;
    CString strStartDir;
    CString strDescription;
    CString strIconLocation;
    CString strArgs;
    int     nIconIndex;
    WORD    wHotkey;
    int     nShowCmd;
} SHORTCUTSTRUCT, *LPSHORTCUTSTRUCT;

class 
#ifdef _AFXEXT
AFX_EXT_CLASS
#endif
CShortcut : public CObject
{
// Constructors
public:
    CShortcut();
    BOOL Create(LPCTSTR, LPCTSTR, LPCTSTR, LPCTSTR, LPCTSTR, LPCTSTR, int,
        WORD, int);
    
// Attributes
public:
    

    
    
protected:
    BOOL    m_bDirty;
    CString m_strPath;
    CString m_strTarget;
    CString m_strStartDir;
    CString m_strDescription;
    CString m_strIconLocation;
    CString m_strArgs;
    int     m_nIconIndex;
    HICON   m_hLargeIcon;
    HICON   m_hSmallIcon;
    WORD    m_wHotkey;
    int     m_nShowCmd;

// Operations
public:
    BOOL Save();

// Overrides

// Implementation
public:
    virtual ~CShortcut();

#ifdef _DEBUG
    virtual void Dump(CDumpContext&);
#endif // _DEBUG



};

#endif
