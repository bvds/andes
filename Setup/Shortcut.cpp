// Shortcut.cpp : implementation file
//

#include "stdafx.h"
#include "Setup.h"
#include "Shortcut.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CShortcut
CShortcut::CShortcut()
{
    // Initialize data members
    m_bDirty           = FALSE;
    m_strPath          = _T("");
    m_strTarget        = _T("");
    m_strStartDir      = _T("");
    m_strDescription   = _T("");
    m_strIconLocation  = _T("");
    m_strArgs          = _T("");
    m_nIconIndex       = 0;
    m_wHotkey          = MAKEWORD((BYTE) 0, (BYTE) 0);
    m_nShowCmd         = 0;
    HICON m_hLargeIcon = NULL;
    HICON m_hSmallIcon = NULL;
}


CShortcut::~CShortcut()
{
    ASSERT(m_bDirty == FALSE);
	
}

#ifdef _DEBUG
void CShortcut::Dump(CDumpContext& dc)
{
    CObject::Dump(dc);

    dc << _T("\nm_bDirty = ")          << m_bDirty
       << _T("\nm_strPath = ")         << m_strPath
       << _T("\nm_strTarget = ")       << m_strTarget
       << _T("\nm_strStartDir = ")     << m_strStartDir
       << _T("\nm_strDescription = ")  << m_strDescription
       << _T("\nm_strIconLocation = ") << m_strIconLocation
       << _T("\nm_strArgs = ")         << m_strArgs
       << _T("\nm_nIconIndex = ")      << m_nIconIndex
       << _T("\nm_wHotkey = ")         << m_wHotkey
       << _T("\nm_nShowCmd = ")        << m_nShowCmd
       << _T("\nm_hLargeIcon = ")      << m_hLargeIcon
       << _T("\nm_hSmallIcon = ")      << m_hSmallIcon
       << _T("\n");
}
#endif // _DEBUG

/////////////////////////////////////////////////////////////////////////////
// Create
// Remarks: Creates a shortcut and saves it
// Inputs:  LPCTSTRs containing full path to file name, full path to target,
//          start directory, description, arguments, icon location, an int
//          containing icon index, a WORD containing hot key, and an int
//          containing show command
// Returns: BOOL; TRUE if successful, FALSE if unsuccessful
BOOL CShortcut::Create(LPCTSTR lpszPath,     LPCTSTR lpszTarget, 
                       LPCTSTR lpszStartDir, LPCTSTR lpszDescription, 
                       LPCTSTR lpszArgs,     LPCTSTR lpszIconLocation,
                       int nIconIndex,       WORD    wHotkey,
                       int nShowCmd)
{
    m_bDirty          = TRUE;
    m_strPath         = lpszPath;
    m_strTarget       = lpszTarget;
    m_strStartDir     = lpszStartDir;
    m_strDescription  = lpszDescription;
    m_strArgs         = lpszArgs;
    m_strIconLocation = lpszIconLocation;
    m_nIconIndex      = nIconIndex;
    m_wHotkey         = wHotkey;
    m_nShowCmd        = nShowCmd;

    return Save();
}


/////////////////////////////////////////////////////////////////////////////
// Save
// Remarks: Saves the shortcut
// Inputs:  None
// Returns: BOOL; TRUE if successful, FALSE if unsuccessful
BOOL CShortcut::Save()
{
    // Save only if we have to
    if (m_bDirty == FALSE)
    {
        return TRUE;
    }

    ASSERT(m_strPath != _T(""));
	::CoInitialize(NULL);
    IShellLink* psl;
    HRESULT hres;
    BOOL bRet = FALSE;

    // Create shell link instance
    hres = ::CoCreateInstance(CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER,
        IID_IShellLink, (LPVOID*) &psl);
    if (SUCCEEDED(hres))
    {
        // Get a pointer to persist file interface
        IPersistFile* ppf;
        hres = psl->QueryInterface(IID_IPersistFile, (LPVOID*) &ppf);
        if (SUCCEEDED(hres))
        {
            // Convert to ANSI
            WORD wsz[MAX_PATH];
            ::MultiByteToWideChar(CP_ACP, 0, (LPCTSTR) m_strPath, -1, wsz, 
                MAX_PATH);

            // Set attributes of link
            psl->SetPath((LPCTSTR) m_strTarget);
			int e = GetLastError();
            psl->SetWorkingDirectory((LPCTSTR) m_strStartDir);
            psl->SetIconLocation((LPCTSTR) m_strIconLocation, m_nIconIndex);
            psl->SetDescription((LPCTSTR) m_strDescription);
            psl->SetArguments((LPCTSTR) m_strArgs);
            psl->SetHotkey(m_wHotkey);
            psl->SetShowCmd(m_nShowCmd);

            // Save the updated link
            hres = ppf->Save(wsz, TRUE);
            if (SUCCEEDED(hres))
            {
                bRet = TRUE;
                m_bDirty = FALSE;
            }
            ppf->Release();
        }
        psl->Release();
    }
	::CoUninitialize();
    return bRet;
}

