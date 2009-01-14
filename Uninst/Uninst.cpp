// Uninst.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "Uninst.h"
#include "UninstDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CUninstApp

BEGIN_MESSAGE_MAP(CUninstApp, CWinApp)
	//{{AFX_MSG_MAP(CUninstApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CUninstApp construction

CUninstApp::CUninstApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CUninstApp object

CUninstApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CUninstApp initialization

BOOL CUninstApp::InitInstance()
{
	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

	//find OS
	OSVERSIONINFO osvi;
	ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	if (! GetVersionEx ( &osvi) ) 
		return FALSE;

	m_dwOS = osvi.dwPlatformId;

	SetRegistryKey("Andes Group");

	free((void*)m_pszProfileName);
	//Change the name of the registry key.
	//The CWinApp destructor will free the memory.
	m_pszProfileName = strdup("FBD");

/*	Now *always* uninstall the version in the executable's directory, no matter what
	m_strInstDir = GetProfileString("Settings", "Install Directory", "");
	if (m_strInstDir.IsEmpty()) {
		AfxMessageBox(IDS_IDIR_NOTFOUND);
		return FALSE;
	}
*/
	// If unset, use EXE's directory as default. 
	if (m_strInstDir.IsEmpty()) 
	{
		TCHAR szPathBuf[_MAX_PATH];
		VERIFY(::GetModuleFileName(m_hInstance, szPathBuf, _MAX_PATH));
		char * pLastBackslash = strrchr(szPathBuf, '\\'); // !!assumes never "C:\fbd.exe"
		if (pLastBackslash != NULL) {
			pLastBackslash[1]  = '\0';	// clobbers ch in buf after dir end
			m_strInstDir = szPathBuf;
		}
	}

	if (m_strInstDir.Right(1) != "\\")
   		m_strInstDir += "\\";


	CString str;
	str.Format(IDS_RUSURE, m_strInstDir);
	if (AfxMessageBox(str, MB_YESNO|MB_ICONQUESTION|MB_DEFBUTTON2, -1) == IDNO)
		return FALSE;

	CUninstDlg* dlg;
	dlg = new CUninstDlg();
	dlg->Create(IDD_UNINST_DIALOG, 0);
	m_pMainWnd = dlg;
	m_pMainWnd->UpdateWindow();
	
	if (!dlg->BeginUninstall())
		return FALSE;

	char szThisFile[MAX_PATH];
	strcpy(szThisFile, m_strInstDir);
	lstrcat(szThisFile, "\\Uninst.exe");

	if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
	{
		MoveFileEx(szThisFile, NULL, MOVEFILE_DELAY_UNTIL_REBOOT);
	}	
	else //Windows 95
	{
		char szWinInitFile[MAX_PATH];
		GetWindowsDirectory(szWinInitFile, MAX_PATH);
		lstrcat(szWinInitFile, "\\WININIT.INI");
	
		WritePrivateProfileString("Rename", "NUL", szThisFile, szWinInitFile);
	}
	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return TRUE;
}

