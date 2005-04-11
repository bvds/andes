// Setup.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include <direct.h>    // for _getcwd
#include "Setup.h"
#include "Files.h"

#include "Shortcut.h"
// windows we use
#include "MainWnd.h"
#include "MySheet.h"
#include "StartPg.h"
#include "LicenseDlg.h"
#include "InstDirPg.h"
#include "SettingPg.h"
#include "Cleanup.h"
#include "ChkSpaceDlg.h"
#include "RegDlg.h"
#include "ProgressDlg.h"
#include "FinishPg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// WinNT/2000 Security helper functions (bottom of file):
extern BOOL RunningAsAdministrator ();
extern BOOL RunningAsPowerUser();

/////////////////////////////////////////////////////////////////////////////
// CSetupApp

BEGIN_MESSAGE_MAP(CSetupApp, CWinApp)
	//{{AFX_MSG_MAP(CSetupApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSetupApp construction

CSetupApp::CSetupApp()
{
	m_bSkipNewer = FALSE;
	m_bReboot = FALSE;
	m_bCopyFilesOnly = FALSE;
	m_bWin2000orXP = FALSE;
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CSetupApp object

CSetupApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CSetupApp initialization
BOOL CSetupApp::InitInstance()
{
	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif
	
	// Parse command-line flags:
	// -Copy arg means copy ANDES files only (no DLLS, registry, shortcuts).
	//  Useful for extracting install files only from a working directory
	//  when building install image, or for mirroring working directory.
	CString strCmdLine(m_lpCmdLine);
	strCmdLine.MakeUpper();
	if (strCmdLine.Find("-COPY") != -1 || strCmdLine.Find("/COPY") != -1)  
		m_bCopyFilesOnly = TRUE;

	// Create and show main window
	m_pMainWnd = new CMainWnd;
	m_nCmdShow = SW_SHOWMAXIMIZED;
    m_pMainWnd->ShowWindow (m_nCmdShow);
	m_pMainWnd->UpdateWindow ();

	// find OS
	OSVERSIONINFO osvi;
	ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	if (! GetVersionEx ( &osvi) ) 
		return FALSE;
	m_dwOS = osvi.dwPlatformId;
	// Following info from latest Platform SDK docs on MSDN web site:
	m_bWin2000orXP = osvi.dwMajorVersion >= 5; // really == 5, but allow for future
	// If needed: dwMinorVersion = 0 on Win2000; dwMinorVersion = 1 on WinXP.

	// on NT/2000/XP, make sure we have Administrator privileges. Needed to create
	// shortcuts in All Users profile and registry entries. 
	// "Power User" group on Win2000 will suffice as well.
	if (m_dwOS == VER_PLATFORM_WIN32_NT && ! (RunningAsAdministrator() || RunningAsPowerUser())) 
	{
		AfxMessageBox("Administrator or Power User privileges are required to install ANDES.\
 \
Setup will exit.");
		// !!! could tell them how to use the "RunAs" command -- hold shift while right-click over
		// .exe and select RunAs -- so they don't have to log off workstation.
		return FALSE;
	}

	// Assemble list of all files to install from relevant data.
	BuildFileList();
	m_nFiles = files.GetSize();
	
	// Build the wizard property sheet
	CMySheet dlg("Setup");
    m_pSheet = &dlg;

	CStartPg   pageStart;
	CInstDirPg pageInstDir;
	CSettingPg pageSetting;
	CFinishPg  pageFinish;

	dlg.AddPage(&pageStart);
	dlg.AddPage(&pageInstDir);
	dlg.AddPage(&pageSetting);
	dlg.AddPage(&pageFinish);
	dlg.SetWizardMode();
	
	// run the wizard to do the install
	int resp = dlg.DoModal();

	TRACE("RESP is %d\n Cancel is %d\n Finish is %d\n", resp, IDCANCEL, ID_WIZFINISH);

	if (resp == ID_WIZFINISH)
	{
		if (theApp.m_bReboot)
		{
			if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
			{
				HANDLE hToken;              // handle to process token 
				TOKEN_PRIVILEGES tkp;       // pointer to token structure  
				// Get the current process token handle so we can get shutdown // privilege.  
				if (!OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES 
						| TOKEN_QUERY, &hToken)) 
					TRACE("OpenProcessToken failed.");  
				// Get the LUID for shutdown privilege.  
				LookupPrivilegeValue(NULL, SE_SHUTDOWN_NAME, &tkp.Privileges[0].Luid);  
				tkp.PrivilegeCount = 1;  // one privilege to set    
				tkp.Privileges[0].Attributes = SE_PRIVILEGE_ENABLED;  
				// Get shutdown privilege for this process.  
				AdjustTokenPrivileges(hToken, FALSE, &tkp, 0, (PTOKEN_PRIVILEGES) NULL, 0); 
				 // Cannot test the return value of AdjustTokenPrivileges.  
				if (GetLastError() != ERROR_SUCCESS) 
					TRACE("AdjustTokenPrivileges enable failed."); 
			}
			CCleanup myDlg;
			myDlg.m_strText.Format(IDS_REBOOT);
			myDlg.m_bRestart = TRUE;
			if (myDlg.DoModal()==IDOK)
			{
				//reboot system
				if (!ExitWindowsEx( EWX_LOGOFF|EWX_REBOOT ,0))
					AfxMessageBox("Setup was unable to reboot automatically.\n Please reboot manually.");
			}
		}
	}
	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return FALSE;
}

BOOL CSetupApp::AcceptLicense()
{
	// Load the contents of file "AndesLicense.txt";
	CFile fileSrc("AndesLicense.txt", CFile::modeRead | CFile::shareDenyWrite);
	int cbSize = fileSrc.GetLength();
	char* pData = (char*) alloca(cbSize + 1);
	int nRead = fileSrc.Read(pData, cbSize);
	pData[nRead] = '\0'; // make null-terminated string

	// run the license dialog
	CLicenseDlg dlg;
	dlg.m_strText = (LPCTSTR) pData;
	return dlg.DoModal() == IDOK && dlg.m_nYesNo == 0; // 0=> yes, 1=> no
}

BOOL CSetupApp::CheckDiskSpace()
{
	// popup dialog which instructs user to wait while we check disk space
	CChkSpaceDlg* dlg;
	dlg = new CChkSpaceDlg(m_pSheet);
	dlg->Create(IDD_CHECKSPACE_DLG, 0);
	dlg->ShowWindow(SW_SHOW);
	char szPathName[255];
	GetCurrentDirectory(255, szPathName);
	m_strCurDir = szPathName;
	int strlen = m_strCurDir.GetLength();
	if (m_strCurDir[strlen-1] != '\\')
		m_strCurDir = m_strCurDir + "\\";
	CString fName;
	DWORD fSize = 0;

	// here we are getting the size of each file we will be copying 
	// and adding them together to find the total disk space we will
	// be needing.  We could also just add this up manually and hard 
	// code it into the program
	// We report every file error to aid when debugging install image missing files.
	for (int i = 0; i < files.GetSize(); i++) {
		fName = files[i];
		try {
			CString fFullPath = m_strCurDir + fName;
			CFile f( fFullPath, CFile::modeRead);
			fSize = fSize + f.GetLength();	//increment total space 
		}
		catch (CFileException* e) 
		{
			CString msg;
			if (e->m_cause == CFileException::fileNotFound) {
				// temp hack -- ignore missing sols.txt since not all made yet
				if (fName.Find("sols.txt") == -1) {
					msg.Format("File not found: %s", fName);
					AfxMessageBox(msg);
				}
			} else {
				char szCause[255] = "";
				e->GetErrorMessage(szCause, 255);
				msg.Format("Error opening file: %s %s", fName, szCause);
				AfxMessageBox(msg);
			}
		e->Delete();
		}
	}
	// doesn't quit here if found error, but will die on filecopy	
	
	// Calculate the amount of free space on selected drive
	int pos = m_strInstDir.Find("\\");
	CString instDrive = m_strInstDir.Left(pos + 1);
	unsigned long sectCluster, byteSector, freeCluster, totCluster;
	LPDWORD pSector, pByte, pFree, pTot;
	pSector = &sectCluster;
	pByte = &byteSector;
	pFree = &freeCluster;
	pTot = &totCluster;
	BOOL bHaveSpace = GetDiskFreeSpace(instDrive, pSector, pByte, pFree, pTot);
	if (!bHaveSpace)	// Syscall failed!
		return FALSE;
	DWORD freeSpace = byteSector * sectCluster * freeCluster;
	
	dlg->ShowWindow(SW_HIDE);
	dlg->DestroyWindow();

	CString strMessage;
	strMessage.Format("Found needed space = %d bytes, free space = %d bytes", fSize, freeSpace);
	AfxMessageBox(strMessage);

	if (fSize > freeSpace) { //Not enough space
		
		strMessage.Format(IDS_NOSPACE, fSize, freeSpace);
		AfxMessageBox(strMessage);
		return FALSE;
	}
	return TRUE;
}

BOOL CSetupApp::CopyTheFiles()
{
	int strlen = m_strInstDir.GetLength();
	if (m_strInstDir[strlen-1] != '\\')
		m_strInstDir = m_strInstDir + "\\";
	
	// Create directories first
	if (!CreateDirectories())
		return FALSE;

	// show progress of file copying
	CProgressDlg* dlg;
	dlg = new CProgressDlg(m_pSheet);
	dlg->Create(IDD_PROGRESS_DLG, 0);
	dlg->ShowWindow(SW_SHOW);

#ifndef PATCH  // not needed in patch install
	// copy system files
	if (! m_bCopyFilesOnly) // skip if only used to copy ANDES files.
	{
		char buf[MAX_PATH];
		GetSystemDirectory(buf, MAX_PATH);
		CString sysDir = buf;
		sysDir = sysDir + "\\";
		if (!dlg->MyCopyDLL("Mfc42.dll", theApp.m_strCurDir, sysDir)){
			dlg->ShowWindow(SW_HIDE);
			dlg->DestroyWindow();
			return FALSE;
		}
		if (!dlg->MyCopyDLL("Msvcrt.dll", theApp.m_strCurDir, sysDir)){
			dlg->ShowWindow(SW_HIDE);
			dlg->DestroyWindow();
			return FALSE;
		}

		// Ensure codec is installed if need be
		InstallCodec();
	}
#endif

	// copy files listed in install list
	for (int i = 0; i < files.GetSize(); i++)
	{
		CString fileName = files[i];
		if (!dlg->MyCopyFile(fileName, theApp.m_strCurDir, m_strInstDir)){
			dlg->ShowWindow(SW_HIDE);
			dlg->DestroyWindow();
			return FALSE;
		}
	}

	// hide and destroy the filecopy progress dialog
	dlg->ShowWindow(SW_HIDE);
	dlg->DestroyWindow();

	// if just copying files we are finished.
	if (m_bCopyFilesOnly)  {
		m_pSheet->ShowWindow(SW_SHOW); // normally done by reg dialog when done, urgh.
		return TRUE;
	}

	// show the "wait for reg entries" modeless dialog
	CRegDlg* pDlg;
	pDlg = new CRegDlg(m_pSheet);
	pDlg->Create(IDD_REG_DLG, 0);
	pDlg->ShowWindow(SW_SHOW);

	// Make registry entries and shortcuts
	if (!MakeRegEntries()){
		AfxMessageBox("Failed to create registry entries");
		return FALSE;
	}
	if (!CreateShortcuts()){
		// Start menu shortcut creation can fail on NT or 2000: give warning.
		// Message assume that creation of shortcut named "Andes Physics Workbench" 
		// inside Install Directory worked OK.
		CString strMsg;
		strMsg.Format("Warning: Failed to create Start Menu entries for running Andes.\nTo run Andes, go to %s and click \'Andes Physics Workbench\'. You can also copy this shortcut to your Desktop", m_strInstDir);
		AfxMessageBox(strMsg);
		// don't treat as fatal error
		// return FALSE;
	}

#if 0 
	// Useful if running out of standard zip distribution, but unnecessary 
	// w/self-extractor for software since it cleans up temp files anyway. 
	// Also unsafe if running to install out of fileserver working directory
	
	// Query with the "cleanup install files?" modal dialog and cleanup if yes.
	CCleanup qdlg;
	qdlg.m_strText.Format(IDS_CLEANUP);
	if (qdlg.DoModal()==IDOK){
		CString str;
		str.LoadString(IDS_WAITREMOVE);
		pDlg->m_txt.SetWindowText(str);
		pDlg->UpdateWindow();
		CleanUpSourceFiles();
	}
#endif 0

	// Finished with install work, take down post-copy dialog
	pDlg->ShowWindow(SW_HIDE);
	pDlg->DestroyWindow();
	return TRUE;
}



BOOL CSetupApp::MakeRegEntries()
{
	SetRegistryKey("Andes Group");//assigns profile name to appname

	free((void*)m_pszProfileName);
	//Change the name of the registry key.
	//The CWinApp destructor will free the memory.
	m_pszProfileName=strdup("FBD");
	
	//Make adjustments to registry
	//Getshort file name of install directory->lisp uses
	char szShortInstDir[MAX_PATH];
	GetShortPathName(m_strInstDir, szShortInstDir, MAX_PATH);
#if 0 // obsolete
	//Insert Install directory information
	WriteProfileString("Settings", "Install Directory", szShortInstDir);
#else // new
	// always overwrite with empty string to force use of executable directory so
	// new version can coexist with any existing ones.
	WriteProfileString("Settings", "Install Directory", "");
#endif // new

	// Whether this is author or student version install
#ifdef STUDENT_INSTALL
	
	WriteProfileInt("Settings", "Author", 0);

#if  USNA_EVAL || EXPERIMENT	// evaluation version, USNA or PITT
	WriteProfileInt("Settings", "Upload", 1);	// set flag to run uploader
# else // non-eval  student version
	WriteProfileInt("Settings", "Upload", 0);	// set flag to disable uploading
# endif 

#else // AUTHOR INSTALL, (default if no special #defines)

	WriteProfileInt("Settings", "Author", 1);
	WriteProfileInt("Settings", "Upload", 0);

#endif

	// Whether or not to use problem set interface (clear in case old entry)
	WriteProfileInt("Settings", "NoProblemSets", 0);
	
	//Experiment information: configured help systems
	WriteProfileInt("Help", "Conceptual", 1);
	WriteProfileInt("Help", "Example",	1);
	WriteProfileInt("Help",	"Procedural",	1);

#if EXPERIMENT
	// Enable textbook buttoni
	WriteProfileInt("Settings", "Experiment", 1);
	//CString strURL = m_strInstDir + "toc.html";
	//WriteProfileString("Settings", "Textbook", strURL);
#endif // EXPERIMENT

	//Create registry entries for App Paths
	CString keyPath = "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\";
	
	HKEY hKey;
	CString keyApp = keyPath + "App Paths\\FBD-TCP.EXE";
	RegCreateKey(HKEY_LOCAL_MACHINE, keyApp, &hKey);

	CString data1 = m_strInstDir;
	LPCSTR lpz1 = (LPCSTR)data1;
	PSZ addrstr1 = (LPSTR)lpz1;
	RegSetValueEx(hKey, "Path", 0, REG_SZ, (LPBYTE)addrstr1, strlen(data1)+1);

	CString data2 = m_strInstDir + "FBD-TCP.EXE";
	LPCSTR lpz2 = (LPCSTR)data2;
	PSZ addrstr2 = (LPSTR)lpz2;
	RegSetValueEx(hKey, NULL, 0, REG_SZ, (LPBYTE)addrstr2, strlen(data2)+1);

	//Create registry entries for uninstall programs
#ifdef OLI
	CString keyUninst = keyPath + "Uninstall\\AndesOLI";
#else
	CString keyUninst = keyPath + "Uninstall\\Andes";
#endif 
	RegCreateKey(HKEY_LOCAL_MACHINE, keyUninst, &hKey);

	CString	data = m_strInstDir + "Uninst.exe ";
	LPCSTR lpz = (LPCSTR)data;
	PSZ addrstr = (LPSTR)lpz;
	
	RegSetValueEx(hKey, "UninstallString", 0, REG_SZ, (LPBYTE)addrstr, strlen(data)+1);
#ifdef OLI
	data = "AndesOLI";
#else
	data = "Andes";
#endif
	lpz = (LPCSTR)data;
	addrstr = (LPSTR)lpz;

	RegSetValueEx(hKey, "DisplayName", 0, REG_SZ, (LPBYTE)addrstr, strlen(data)+1);
	//Create registry entry for version information
	keyPath = "Software\\Andes Group\\Andes\\";
	CString strVersion =  theApp.GetProductVersion("FBD-tcp.exe");	
	CString keyVersion = keyPath + strVersion;
	RegCreateKey(HKEY_LOCAL_MACHINE, keyVersion, &hKey);

	// run the installed workbench with /register flag to silently register it as handler for its filetypes
	HINSTANCE hInst= ::ShellExecute(NULL, NULL, m_strInstDir + "fbd-tcp.exe", "/register", NULL, SW_HIDE);
	if ((int)hInst <= 32) {
		AfxMessageBox("Warning: failed to register ANDES file types\nRun Andes Workbench (fbd-tcp.exe) once to register file types");
	}

	return TRUE;

}

// Code for locating user profile directory, in which we make our shortcuts:

// Shell library version 4.71 or greater has SHGetSpecialFolderPath API which 
// would tell us where profiles are. It's not available on older systems, though. 
// Version history of this library is:
// 4.00: Windows 95, Windows NT 4.0, Internet Explorer 3.0, and Internet Explorer 4.0
//		             without Web Integrated Desktop. 
// 4.71: Internet Explorer 4.0 with Web Integrated Desktop. 
// 4.72: Internet Explorer 4.01 with Web Integrated Desktop.
//
// By now almost all of our users should in fact have 4.71 or greater. Still,
// doesn't hurt to check for it. Following function copied from Platform SDK
// documentation retrieves version number of shel32.dll
HRESULT GetShell32Version(LPDWORD pdwMajor, LPDWORD pdwMinor)
{
	HINSTANCE   hShell32;

	if(   IsBadWritePtr(pdwMajor, sizeof(DWORD)) || 
		  IsBadWritePtr(pdwMinor, sizeof(DWORD)))
	   return E_INVALIDARG;

	*pdwMajor = 0;
	*pdwMinor = 0;

	//Load the DLL.
	hShell32 = LoadLibrary(TEXT("shell32.dll"));

	if(hShell32)
	   {
	   HRESULT           hr = S_OK;
	   DLLGETVERSIONPROC pDllGetVersion;
   
	   /*
	   You must get this function explicitly because earlier versions of the DLL 
	   don't implement this function. That makes the lack of implementation of the 
	   function a version marker in itself.
	   */
	   pDllGetVersion = (DLLGETVERSIONPROC)GetProcAddress(hShell32, TEXT("DllGetVersion"));
   
	   if(pDllGetVersion)
		  {
		  DLLVERSIONINFO    dvi;
      
		  ZeroMemory(&dvi, sizeof(dvi));
		  dvi.cbSize = sizeof(dvi);
   
		  hr = (*pDllGetVersion)(&dvi);
      
		  if(SUCCEEDED(hr))
			 {
			 *pdwMajor = dvi.dwMajorVersion;

			 *pdwMinor = dvi.dwMinorVersion;
			 }
		  }
	   else
		  {
		  /*
		  If GetProcAddress failed, the DLL is a version previous to the one 
		  shipped with IE 3.x.
		  */
		  *pdwMajor = 4;
		  *pdwMinor = 0;
		  }
   
	   FreeLibrary(hShell32);

	   return hr;
	   }

	return E_FAIL;
	}

BOOL HaveShell471()		// true if we have Shell lib version 4.71 or greater
{
	DWORD dwMajor, dwMinor;
	HRESULT hr = GetShell32Version(&dwMajor, &dwMinor);

	if (SUCCEEDED(hr)) TRACE("Shell32 version: %d.%d\n", dwMajor, dwMinor);
	else TRACE("Failed to get Shell32 version\n");

	return ((SUCCEEDED(hr)) && (dwMajor >= 4) && (dwMinor >= 71));
}

// find paths to Windows Folder and the folder containing the Start Menu
// program groups to make shortcuts in.
//
// On true multi-user systems we return the appropriate directory uder the
// "All Users" profile folder, which hold Desktop and Start Menu items that 
// are mandatory for  all users on multi-user  systems. Note this means
// the installer must have permission to modify these, normally requiring
// admin privileges.
//
// On Win95/98/Me, we assume no user profiles, so it's just the start
// menu directory.
//
// OUT parameter results are paths, i.e. include trailing backslash.
void CSetupApp::GetFolderPaths(CString& winPath, CString& progGroupPath)
{
	// Get the windows directory
	char szPathTemp[MAX_PATH];
	GetWindowsDirectory(szPathTemp, MAX_PATH);
	TRACE("Windows Directory = %s\n", szPathTemp);
	winPath = CString(szPathTemp) + '\\';

	// Windows dir is initial default profile dir. OK as is for Win95/98/Me
	// !!! Maybe should check if multiple user profiles are enabled (how??)
	CString profileDir = szPathTemp;

	// But if it's WinNT/2000/XP we want Start Menu under the All Users profile
	if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
	{
		// WinNT stores user profiles here:
		profileDir = winPath + "Profiles\\All Users";
		
		// On new Win2000/XP or upgrade from 95/98, profiles are in:
		//       [Windows installation drive]:\Documents and Settings
		// If Win2000/XP was installed as *upgrade* from NT, profiles remain
		// in the old NT location.
		if (m_bWin2000orXP)
		{
	/* Following isn't getting called, at least on XP we tried
			// Try to use shell API to locate path all the way to 
			//  PROFILES/All Users/Start Menu/Programs.
			// Note this func is unreliable on Windows95/98 -- directory it returns
			// may not be effective if multiple user profiles not enabled
			if (HaveShell471()) //  have appropriate version of shell dll.
			{
				TRACE("Using Shell Library to find Program Group Dir\n");
				HRESULT hr = SHGetSpecialFolderPath(NULL, szPathTemp, CSIDL_COMMON_PROGRAMS, FALSE);
				if (SUCCEEDED(hr)) {
					TRACE("Program Group dir = %s\n", szPathTemp);
					// temp: write a log file to  test this step on NT/2000/XP test systems
					FILE * fp = fopen(m_strInstDir + "\\Setup.log", "w");
					fprintf(fp, "Program Group dir = %s\n", szPathTemp);
					fclose(fp);
					// end temp
					progGroupPath = CString(szPathTemp) + '\\';
					// early return since we now have complete path to return
					return;
				}
			}
     */
			// get System drive from windows path (not necessarily C:)
			char szDrive[_MAX_DRIVE];
			_splitpath(winPath, szDrive, NULL, NULL, NULL);

			// try standard location for 2000/XP
			profileDir = CString(szDrive) 
					+ "\\Documents and Settings\\All Users";

			// if not there, try the NT location, in case an upgrade
			if (! FileExists(profileDir) ) 
			{
				profileDir = winPath + "Profiles\\All Users";
			}			
		}
	}

	// Return path to Start Menu Programs under profile directory determined above 
	progGroupPath = profileDir + "\\Start Menu\\Programs\\";
}


BOOL CSetupApp::CreateShortcuts()
{
	//Make shortcut: a link to FBD executable in the installation directory.
	// The link has more friendly name "Andes Physics Workbench".
	// This virtually always succeeds if the file copying is allowed, so users
	// can fall back to locating install directory and launching via this link
	// even if subsequent Start Menu or Desktop shortcut creation fails.
	CShortcut SC;
	CString strPathObj = m_strInstDir + "\\Andes Physics Workbench.lnk";
	CString strTarget = m_strInstDir + "FBD-tcp.exe";
	if (!SC.Create(strPathObj, strTarget, m_strInstDir, "Shortcut to FBD","",
		strTarget,0, NULL, SW_SHOW)){
						TRACE("Failed to create Workbench link");
						return FALSE;
	}
#ifdef OLI
	// no Start Menu shortcuts in OLI -- don't want students launching ANDES
	return TRUE;
#endif OLI

	// Following creates Start Menu shortcuts by direct shell link creation in the
	// appropriate user profile directories, using the CShortcut helper class.
	CString winPath, progGroupPath;
	GetFolderPaths(winPath, progGroupPath);
	CString groupDir = progGroupPath + "Andes";

#if 0
	// temp: for debugging
	CString strMsg;
	strMsg.Format("Creating shortcuts, progGroupPath=%s", groupDir);
	AfxMessageBox(strMsg);
#endif 0

	// Create Andes program group directory
	CreateDirectory(groupDir, NULL);

	// Make shortcut in program group:  Link to workbench on Start Menu
	CShortcut SC2;
	strPathObj = groupDir + "\\Andes Physics Workbench.lnk";
	if (!SC2.Create(strPathObj, strTarget, m_strInstDir, "FBD Start Menu","",
		strTarget,0, NULL, SW_SHOW)){
						TRACE("Failed to create Workbench shortcut");
						return FALSE;
	}
	
	// Make shortcut in program group:  Link to helpfile on Start Menu
	CShortcut SC3;
	strPathObj = groupDir + "\\Andes Help.lnk";
	strTarget = m_strInstDir + "\\FBD-tcp.hlp";
	CString iconLoc = winPath + "WinHlp32.exe"; // steal icon from WinHlp32.exe
	if (!SC3.Create(strPathObj, strTarget, m_strInstDir, "Help Start Menu","",
		iconLoc, 1, NULL, SW_SHOW)){
						TRACE("Failed to create Andes Help File shortcut");
						return FALSE;
	}

#ifdef EXPERIMENT
	//Make shortcut:  Put on start menu
	CShortcut SC4;
	strPathObj = groupDir + "\\Upload Andes Data.lnk";
	strTarget = m_strInstDir + "\\Upload.exe";
	if (!SC4.Create(strPathObj, strTarget, m_strInstDir, "Upload Start Menu","",
		strTarget, 0, NULL, SW_SHOW)){
						TRACE("Failed to create Uploader shortcut");
						return FALSE;
	}
#endif EXPERIMENT

	return TRUE;
}

BOOL CSetupApp::CreateDirectories()
{
	// make sure install directory is created
	CreateDirectory(m_strInstDir, NULL);
	
	// create directories listed in directory list
	for (int i = 0; i < directories.GetSize() ; i++)
	{
		CString dir = directories[i];
		CString strDir = m_strInstDir + dir;
		if (!CreateDirectory(strDir, NULL)){
			//don't fail if directory already exists
			if (GetLastError() != ERROR_ALREADY_EXISTS)
				return FALSE;
		}
	}
	
	return TRUE;
}


void CSetupApp::CleanUpSourceFiles()
{
	// Remove unzipped files from temp directory
	for (int i = 0; i < files.GetSize(); i++)
	{
		CString fileName = files[i];
		CString fullFileName = theApp.m_strCurDir + fileName;

		DWORD dwOldAttr = GetFileAttributes(fullFileName);
		DWORD dwNewAttr = dwOldAttr & (~FILE_ATTRIBUTE_READONLY);
		SetFileAttributes(fullFileName, dwNewAttr);
		
		WIN32_FIND_DATA wdata;
		HANDLE hfile = FindFirstFile(fullFileName, &wdata);
		if (hfile != INVALID_HANDLE_VALUE){
			FindClose(hfile);
			DeleteFile(fullFileName);
		}
	}
	int nSysFiles = 2;//Remove system dll's from temp directory
	//having difficulty, can't delete, don't know why, works during debug
	CString fileName = "MSVCRT.dll";
	while (nSysFiles >= 0)
	{
		CString fullFileName = theApp.m_strCurDir + fileName;

		DWORD dwOldAttr = GetFileAttributes(fullFileName);
		DWORD dwNewAttr = dwOldAttr & (~FILE_ATTRIBUTE_READONLY);
		SetFileAttributes(fullFileName, dwNewAttr);
		WIN32_FIND_DATA wdata;
		HANDLE hfile = FindFirstFile(fullFileName, &wdata);
		if (hfile != INVALID_HANDLE_VALUE){
			FindClose(hfile);
			DeleteFile(fullFileName);
		}
		fileName = "MFC42.DLL";
		nSysFiles--;
	}

	// remove directories from temp directory
	// use reverse order so subdirs deleted before parents.
	for (int j = directories.GetUpperBound(); j >= 0; --j) {
		CString dir = directories[j];
		CString strDir = theApp.m_strCurDir + dir;
		RemoveDirectory(strDir);
	}
	
	// arrange for this setup file in temp directory to be removed upon next reboot
	char szThisFile[MAX_PATH];
	strcpy(szThisFile, m_strCurDir);
	lstrcat(szThisFile, "\\Setup.exe");

	if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
	{
		MoveFileEx(szThisFile, NULL, MOVEFILE_DELAY_UNTIL_REBOOT);
	}	
	else // Windows 95
		RenameOnStartup("NUL", szThisFile);
}



void CSetupApp::RenameOnStartup(LPCTSTR szNewName, LPCTSTR szOldName)
{
	//this setup file in temp directory to be removed upon next reboot
	char szWinInitFile[MAX_PATH];
	GetWindowsDirectory(szWinInitFile, MAX_PATH);
	lstrcat(szWinInitFile, "\\WININIT.INI");
	
	WritePrivateProfileString("Rename", szNewName, szOldName, szWinInitFile);
}

// we are only try to read a file that exists
BOOL CSetupApp::HasCurrentDLL(CString newDLL, CString oldDLL)
{
	CString msg;//error message
	try{
		CFile fNew( newDLL, CFile::modeRead);
	}
	catch (CFileException* e) {
		msg.Format(IDS_ERROR_OPEN, newDLL);
		AfxMessageBox(msg);
		e->Delete();
	}
	
	try{
		CFile fOld( oldDLL, CFile::modeRead);
	}
	catch (CFileException* e) {
		if(e->m_cause == CFileException::sharingViolation){
			//we don't want to report a sharing error if older DLL on system,
			//we will copy with different name and rename on reboot
			e->Delete();
			if (CheckDLL(newDLL, oldDLL)){
				return TRUE;
			//if newer or equivalent DLL on system, no need to copy later
			}
			return FALSE;
		}
		else
		{
			msg.Format(IDS_ERROR_OPEN, oldDLL);
			AfxMessageBox(msg);
			e->Delete();
			return FALSE;
		}
	}
	if (CheckDLL(newDLL, oldDLL))
		return TRUE;

	return FALSE;
}

// Fill FILETIME ft with modification time for named file
// returns TRUE for success, else FALSE
BOOL GetFileWriteTime(LPCTSTR szFileName, FILETIME& ft)
{
	CFile file;
	if (file.Open(szFileName, CFile::modeRead))
		return ::GetFileTime((HANDLE)file.m_hFile, NULL, NULL, &ft);

	return FALSE;
}

BOOL CSetupApp::ExistingFileNewer(CString newFile, CString oldFile)
{
	FILETIME ftOld, ftNew;
	//: only return TRUE if we can get both file times
	return (GetFileWriteTime(newFile, ftNew) &&
		    GetFileWriteTime(oldFile, ftOld) &&
			(::CompareFileTime(&ftNew, &ftOld) < 0));  // ftNew earlier than ftOld
}



BOOL CSetupApp::CheckDLL(CString strNewFile, CString strOldFile)
{
	CString strNewVersion = GetFileVersion(strNewFile);
	int nNewFirst, nNewSecond, nNewThird;
	sscanf(strNewVersion,  "%d.%d.%d", &nNewFirst, &nNewSecond, &nNewThird);
	CString strOldVersion = GetFileVersion(strOldFile);
	int nOldFirst, nOldSecond, nOldThird;
	sscanf(strOldVersion, "%d.%d.%d", &nOldFirst, &nOldSecond, &nOldThird);

	if (nNewFirst == nOldFirst)
	{
		if (nNewSecond == nOldSecond)
		{
			return (nNewThird <= nOldThird);
		}
		else
			return (nNewSecond < nOldSecond);
	}
	else
		return (nNewFirst < nOldFirst);


}

CString CSetupApp::GetProductVersion(CString strFile)
{
	CString strVersion;
	DWORD dwVerInfoSize;
	DWORD dwHwnd;
	void* pBuffer;
	TCHAR szName[_MAX_PATH];
	strcpy(szName, strFile);
	UINT  uVersionLen = 0; 
	LPVOID  lpVersion = NULL; 
	VS_FIXEDFILEINFO *pFixedInfo; // pointer to fixed file info structure
//	VERIFY(::GetModuleFileName(AfxGetInstanceHandle(), szExeName, sizeof(szExeName)));
	dwVerInfoSize = GetFileVersionInfoSize(szName, &dwHwnd);
	if (dwVerInfoSize) { 
		pBuffer = malloc(dwVerInfoSize); 
		if (pBuffer != NULL){
			GetFileVersionInfo(szName, dwHwnd, dwVerInfoSize, pBuffer);
			VerQueryValue(pBuffer,_T("\\"),(void**)&pFixedInfo,(UINT *)&uVersionLen);
			strVersion.Format("%u.%u.%u", HIWORD (pFixedInfo->dwProductVersionMS),
							  LOWORD (pFixedInfo->dwProductVersionMS),
							  HIWORD (pFixedInfo->dwProductVersionLS));
			// include fourth digit only if non-zero. Set it for pre-release updates, tests.
			if (LOWORD (pFixedInfo->dwProductVersionLS)) {
				strVersion.Format("%s.%u", strVersion, LOWORD(pFixedInfo->dwProductVersionLS));
			}
			
			free(pBuffer);
		}
	}
	return strVersion;
}

/////////////////////////////////////////////////////////////////////////////
// FileExists
// Remarks: Determines whether a file exists
// Inputs:  LPCTSTR containing full path to file name
// Returns: BOOL; TRUE if file exists, FALSE if file does not exist
BOOL CSetupApp::FileExists(LPCTSTR lpsz)
{
    ASSERT(lpsz != NULL);

    WIN32_FIND_DATA wfd;
    HANDLE hFind = ::FindFirstFile(lpsz, &wfd);
    ::FindClose(hFind);

    // Make sure file exists
    return (hFind != INVALID_HANDLE_VALUE);
}

CString CSetupApp::GetFileVersion(CString strFile)
{
	CString strVersion;
	DWORD dwVerInfoSize;
	DWORD dwHwnd;
	void* pBuffer;
	TCHAR szName[_MAX_PATH];
	strcpy(szName, strFile);
	UINT  uVersionLen = 0; 
	LPVOID  lpVersion = NULL; 
	VS_FIXEDFILEINFO *pFixedInfo; // pointer to fixed file info structure
//	VERIFY(::GetModuleFileName(AfxGetInstanceHandle(), szExeName, sizeof(szExeName)));
	dwVerInfoSize = GetFileVersionInfoSize(szName, &dwHwnd);
	if (dwVerInfoSize) { 
		pBuffer = malloc(dwVerInfoSize); 
		if (pBuffer != NULL){
			GetFileVersionInfo(szName, dwHwnd, dwVerInfoSize, pBuffer);
			VerQueryValue(pBuffer,_T("\\"),(void**)&pFixedInfo,(UINT *)&uVersionLen);
			strVersion.Format("%u.%u.%u", HIWORD (pFixedInfo->dwFileVersionMS),
							  LOWORD (pFixedInfo->dwFileVersionMS),
							  HIWORD (pFixedInfo->dwFileVersionLS));
	//						LOWORD (pFixedInfo->dwProductVersionLS));	
			free(pBuffer);
		}
	}
	return strVersion;
}

// Following code for NT taken from sample on MSDN. 
// Added cast to compile in cpp

/*------------------------------------------------------------------
| Name: RunningAsAdministrator
| Desc: checks if user has administrator privileges
| Notes: This function returns TRUE if the user identifier associated with 
|   this process is a member of the the Administrators group.
------------------------------------------------------------------*/
BOOL RunningAsAdministrator ( VOID)
{
 BOOL   fAdmin;
 HANDLE  hThread;
 TOKEN_GROUPS *ptg = NULL;
 DWORD  cbTokenGroups;
 DWORD  dwGroup;
 PSID   psidAdmin;

 SID_IDENTIFIER_AUTHORITY SystemSidAuthority= SECURITY_NT_AUTHORITY;

 // First we must open a handle to the access token for this thread.

 if ( !OpenThreadToken ( GetCurrentThread(), TOKEN_QUERY, FALSE, &hThread))
 {
  if ( GetLastError() == ERROR_NO_TOKEN)
  {
   // If the thread does not have an access token, we'll examine the
   // access token associated with the process.

   if (! OpenProcessToken ( GetCurrentProcess(), TOKEN_QUERY, 
                 &hThread))
   return ( FALSE);
  }
  else 
   return ( FALSE);
 }

 // Then we must query the size of the group information associated with
 // the token. Note that we expect a FALSE result from GetTokenInformation
 // because we've given it a NULL buffer. On exit cbTokenGroups will tell
 // the size of the group information.

 if ( GetTokenInformation ( hThread, TokenGroups, NULL, 0, &cbTokenGroups))
  return ( FALSE);

 // Here we verify that GetTokenInformation failed for lack of a large
 // enough buffer.

 if ( GetLastError() != ERROR_INSUFFICIENT_BUFFER)
  return ( FALSE);

 // Now we allocate a buffer for the group information.
 // Since _alloca allocates on the stack, we don't have
 // to explicitly deallocate it. That happens automatically
 // when we exit this function.

 if ( ! ( ptg= (TOKEN_GROUPS *)_alloca ( cbTokenGroups))) 
  return ( FALSE);

 // Now we ask for the group information again.
 // This may fail if an administrator has added this account
 // to an additional group between our first call to
 // GetTokenInformation and this one.

 if ( !GetTokenInformation ( hThread, TokenGroups, ptg, cbTokenGroups,
          &cbTokenGroups) )
  return ( FALSE);

 // Now we must create a System Identifier for the Admin group.

 if ( ! AllocateAndInitializeSid ( &SystemSidAuthority, 2, 
            SECURITY_BUILTIN_DOMAIN_RID, 
            DOMAIN_ALIAS_RID_ADMINS,
            0, 0, 0, 0, 0, 0, &psidAdmin) )
  return ( FALSE);

 // Finally we'll iterate through the list of groups for this access
 // token looking for a match against the SID we created above.

 fAdmin= FALSE;

 for ( dwGroup= 0; dwGroup < ptg->GroupCount; dwGroup++)
 {
  if ( EqualSid ( ptg->Groups[dwGroup].Sid, psidAdmin))
  {
   fAdmin = TRUE;

   break;
  }
 }

 // Before we exit we must explicity deallocate the SID we created.

 FreeSid ( psidAdmin);

 return ( fAdmin);
}
/* eof - RunningAsAdministrator */

// Added following for Win2000:
// Builtin "Power Users" group has greater privileges than users, but
// a little less than Adminstrators. They should be allowed to install
// software that doesn't modify system configuration. They have enough
// for us as well, and they have modify access on C:\Program Files.
// This group not used on WinXP, I don't think.

/*------------------------------------------------------------------
| Name: RunningAsPowerUser
| Desc: checks if user is member of "Power Users" group 
| Notes: This function returns TRUE if the user identifier associated with 
|   this process is a member of the the Power Users group.
------------------------------------------------------------------*/
BOOL RunningAsPowerUser ( VOID)
{
 BOOL   fAdmin;
 HANDLE  hThread;
 TOKEN_GROUPS *ptg = NULL;
 DWORD  cbTokenGroups;
 DWORD  dwGroup;
 PSID   psidAdmin;

 SID_IDENTIFIER_AUTHORITY SystemSidAuthority= SECURITY_NT_AUTHORITY;

 // First we must open a handle to the access token for this thread.

 if ( !OpenThreadToken ( GetCurrentThread(), TOKEN_QUERY, FALSE, &hThread))
 {
  if ( GetLastError() == ERROR_NO_TOKEN)
  {
   // If the thread does not have an access token, we'll examine the
   // access token associated with the process.

   if (! OpenProcessToken ( GetCurrentProcess(), TOKEN_QUERY, 
                 &hThread))
   return ( FALSE);
  }
  else 
   return ( FALSE);
 }

 // Then we must query the size of the group information associated with
 // the token. Note that we expect a FALSE result from GetTokenInformation
 // because we've given it a NULL buffer. On exit cbTokenGroups will tell
 // the size of the group information.

 if ( GetTokenInformation ( hThread, TokenGroups, NULL, 0, &cbTokenGroups))
  return ( FALSE);

 // Here we verify that GetTokenInformation failed for lack of a large
 // enough buffer.

 if ( GetLastError() != ERROR_INSUFFICIENT_BUFFER)
  return ( FALSE);

 // Now we allocate a buffer for the group information.
 // Since _alloca allocates on the stack, we don't have
 // to explicitly deallocate it. That happens automatically
 // when we exit this function.

 if ( ! ( ptg= (TOKEN_GROUPS *)_alloca ( cbTokenGroups))) 
  return ( FALSE);

 // Now we ask for the group information again.
 // This may fail if an administrator has added this account
 // to an additional group between our first call to
 // GetTokenInformation and this one.

 if ( !GetTokenInformation ( hThread, TokenGroups, ptg, cbTokenGroups,
          &cbTokenGroups) )
  return ( FALSE);

 // Now we must create a System Identifier for the Power Users group.

 if ( ! AllocateAndInitializeSid ( &SystemSidAuthority, 2, 
            SECURITY_BUILTIN_DOMAIN_RID, 
            DOMAIN_ALIAS_RID_POWER_USERS,
            0, 0, 0, 0, 0, 0, &psidAdmin) )
  return ( FALSE);

 // Finally we'll iterate through the list of groups for this access
 // token looking for a match against the SID we created above.

 fAdmin= FALSE;

 for ( dwGroup= 0; dwGroup < ptg->GroupCount; dwGroup++)
 {
  if ( EqualSid ( ptg->Groups[dwGroup].Sid, psidAdmin))
  {
   fAdmin = TRUE;

   break;
  }
 }

 // Before we exit we must explicity deallocate the SID we created.

 FreeSid ( psidAdmin);

 return ( fAdmin);
}
/* eof - RunningAsAdministrator */


// Helper to launch application given command line
// RETURNS process handle, NULL on failure
// Arg is passed as command line to StartProcess: Best if program name has 
// quoted full path in case path has spaces, e.g as follows: 
//		"C:\Program Files\Andes.exe"
extern HANDLE StartProcess(LPCTSTR pszCmdLine, LPCTSTR pszWorkingDir=NULL)
{
	STARTUPINFO si;
    PROCESS_INFORMATION pi;

	ZeroMemory( &si, sizeof(si) );
    si.cb = sizeof(si);

	// CmdLineArg is in-out parameter so need modifiable copy:
	LPTSTR pszCmdLineArg = _tcsdup(pszCmdLine);
    
	// Start the child process.
	BOOL bSuccess = ::CreateProcess( NULL, // Module name  
        pszCmdLineArg,		// Command line (in-out) 
        NULL,				// Process handle not inheritable. 
        NULL,				// Thread handle not inheritable. 
        FALSE,				// Set handle inheritance to FALSE. 
        0,					// No creation flags. 
        NULL,				// Use parent's environment block. 
		pszWorkingDir,		// if NULL, uses parent's working dir
        &si,				// Pointer to STARTUPINFO structure.
        &pi );				// Pointer to PROCESS_INFORMATION structure.

	free(pszCmdLineArg);

	if (!bSuccess)
		return NULL;
	// else succeeded:

	// We don't need thread handle so close it
	CloseHandle( pi.hThread );

    // return the process handle. Should be closed when finished.
    return ( pi.hProcess );
}

// Install ACELP audio codec used in our videos if needed.
// We do this by checking if file already exists; if not, we launch the INF
// file with the appropriate command (depends on Win98 vs NT). 
// Don't know how to check if this succeeds.
void CSetupApp::InstallCodec()
{
	const CString strCodecName = "sl_anet.acm";
	const CString strInfName = "acelpacm.inf";
	
	// check for codec file in system dir. This is a weak test, since file could
	// exist but codec not be registered (this will happen if you uninstall the codec),
	// but we assume that is unlikely. More reliable test would use audio compression
	// manager interface.
	char szSysDir[MAX_PATH];
	::GetSystemDirectory(szSysDir, MAX_PATH);
	CString strCodecPath = CString(szSysDir) + "\\" + strCodecName;
	if (FileExists(strCodecPath)) {
		// AfxMessageBox("Codec appears to be installed");
		return;
	}

	// else build command to launch the inf file
	CString strCmdLine;
	if (m_dwOS == VER_PLATFORM_WIN32_NT) 
		strCmdLine = "Rundll32.exe setupapi.dll,InstallHinfSection DefaultInstall 132 ";
	else // Win9x
		strCmdLine = "Rundll.exe setupx.dll,InstallHinfSection DefaultInstall 132 ";
	// Get full path to the INF file in our current working directory.
	char szWorkingDir[MAX_PATH];
	if (_getcwd(szWorkingDir, MAX_PATH) == NULL)
		AfxMessageBox("GetCwd failed while launching audio codec installer!");
	CString strInfPath = CString(szWorkingDir) + "\\" + strInfName;
	
	strCmdLine += strInfPath;

	// OK, now run this command line
	// AfxMessageBox("Trying to install codec");
	HANDLE hProcess = StartProcess(strCmdLine);
	if (hProcess == NULL) {
		CString strMsg;
		strMsg.Format("Failed to launch audio codec installer. Error code = %d", ::GetLastError());
		AfxMessageBox(strMsg);
	}

	// We could wait for RunDll process to finish and query its exit status to try to tell
	// if it succeeded. For now, just leave it running -- should be quick if it succeeds.
	::CloseHandle(hProcess);
}


