// UninstDlg.cpp : implementation file
//

#include "stdafx.h"
#include "Uninst.h"
#include "files.h"
#include "UninstDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CUninstDlg dialog

CUninstDlg::CUninstDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CUninstDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CUninstDlg)
	//}}AFX_DATA_INIT
}

void CUninstDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CUninstDlg)
	DDX_Control(pDX, IDC_PROGRESS, m_Progress);
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDC_SCCHK, m_scChk);
	DDX_Control(pDX, IDC_REGCHK, m_regChk);
	DDX_Control(pDX, IDC_PROGCHK, m_progChk);
	DDX_Control(pDX, IDC_DIRCHK, m_dirChk);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CUninstDlg, CDialog)
	//{{AFX_MSG_MAP(CUninstDlg)
	ON_WM_ENTERIDLE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CUninstDlg message handlers

BOOL CUninstDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	m_Ok.EnableWindow(FALSE);
	
	// Assemble list of files to remove
	BuildFileList();
	int nFiles = CalcNumFiles();
	int nDirs = CalcNumDirs();
	int nRange = nFiles + nDirs + 8;//add 4 for shortcuts, 4 for reg entries
	m_Progress.SetRange(0, nRange);
	m_Progress.SetStep(1);
	return TRUE;  // return TRUE  unless you set the focus to a control
}

BOOL CUninstDlg::BeginUninstall()
{
	DeleteFiles();
	DeleteDirectories();
	DeleteShortcuts();
	RemoveRegEntries();
	m_Ok.EnableWindow(TRUE);
	Sleep(3000);
	return TRUE;
}

void CUninstDlg::DeleteShortcuts()
{
	CString fullFileName = theApp.m_strInstDir + "Andes Physics Workbench.lnk";
	MyDeleteFile(fullFileName);
	m_Progress.StepIt();

	OSVERSIONINFO osvi;
	ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	if (! GetVersionEx ( &osvi) ) 
		return;

	//Get the windows directory
	char buf[MAX_PATH];
	GetWindowsDirectory(buf, MAX_PATH);
	CString winDir = buf;
	winDir = winDir + "\\";

	CString groupDir = winDir;
	if (osvi.dwPlatformId == VER_PLATFORM_WIN32_NT)
		groupDir = groupDir + "Profiles\\All Users\\";

	groupDir = groupDir + "Start Menu\\Programs\\Andes";

	fullFileName = groupDir + "\\Andes Physics Workbench.lnk";
	MyDeleteFile(fullFileName);
	m_Progress.StepIt();

	fullFileName = groupDir + "\\Andes Help.lnk";
	MyDeleteFile(fullFileName);
	m_Progress.StepIt();

//	fullFileName = "C:\\Windows\\Start Menu\\Programs\\Andes\\Upload Andes Files.lnk";
//	MyDeleteFile(fullFileName);
//	m_Progress.StepIt();

	RemoveDirectory(groupDir);
	m_scChk.ShowWindow(SW_SHOW);
	m_Progress.StepIt();
	UpdateWindow();


}

void CUninstDlg::DeleteFiles()
{
	for (int i = 0; i < files.GetSize(); i++)
	{
		CString fileName = files[i];
		CString fullFileName = theApp.m_strInstDir + fileName;
		m_Progress.StepIt();
		MyDeleteFile(fullFileName);
	}
	m_progChk.ShowWindow(SW_SHOW);
//	m_Progress.StepIt();
	UpdateWindow();


}

void CUninstDlg::DeleteDirectories()
{
	// use reverse order so subdirs deleted before parents.
	for (int i = directories.GetUpperBound(); i >= 0; --i)
	{
		CString dir = directories[i];
		CString strDir = theApp.m_strInstDir + dir;
		RemoveDirectory(strDir);
		m_Progress.StepIt();
	}
	m_dirChk.ShowWindow(SW_SHOW);
//	m_Progress.StepIt();
	UpdateWindow();

}




void CUninstDlg::PostNcDestroy() 
{
	delete this;
	CDialog::PostNcDestroy();
}

void CUninstDlg::OnOK() 
{
	DestroyWindow();
}

void CUninstDlg::MyDeleteFile(CString fName)
{
	//Remove read-only attribute
	DWORD dwAttr = GetFileAttributes(fName);
	dwAttr = dwAttr & (~FILE_ATTRIBUTE_READONLY);
	SetFileAttributes(fName, dwAttr);
	WIN32_FIND_DATA wdata;
	HANDLE hfile = FindFirstFile(fName, &wdata);
	if (hfile != INVALID_HANDLE_VALUE){
		FindClose(hfile);
		DeleteFile(fName);
	}
}

int CUninstDlg::CalcNumFiles()
{
	// int nFiles =  (sizeof(files)/sizeof(files[0]));
	int nFiles = files.GetSize();
	return nFiles;
}

int CUninstDlg::CalcNumDirs()
{
	// int nDirs =  (sizeof(directories)/sizeof(directories[0]));
	int nDirs = directories.GetSize();
	return nDirs;
}

void CUninstDlg::RemoveRegEntries()
{
	HKEY hKey;
	
	CString strKey = "Software";
	CString strMyKey = "Andes Group";
 
	//Delete Profile Settings
	if (RegOpenKeyEx(HKEY_CURRENT_USER, strKey, 0, KEY_ALL_ACCESS, &hKey) == ERROR_SUCCESS)
	{
		if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
			DeleteSubkeys(hKey, strMyKey);
		RegDeleteKey(hKey, strMyKey);
	}
	m_Progress.StepIt();
	//Delete registry entry for version information
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, strKey, 0, KEY_ALL_ACCESS, &hKey) == ERROR_SUCCESS)
	{
		if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
			DeleteSubkeys(hKey, strMyKey);
		RegDeleteKey(hKey, strMyKey);
	}
	m_Progress.StepIt();

	//Delete registry entries for App Paths
	CString strKeyPath = "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\";
	
	strKey = strKeyPath + "App Paths";
	strMyKey = "FBD.EXE";

	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, strKey, 0, KEY_ALL_ACCESS, &hKey)== ERROR_SUCCESS)
	{
		if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
			DeleteSubkeys(hKey, strMyKey);
		RegDeleteKey(hKey, strMyKey);
	}
	m_Progress.StepIt();
	
	//Delete registry entries for uninstall programs
	strKey = strKeyPath + "Uninstall";
	strMyKey = "Andes";

	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, strKey, 0, KEY_ALL_ACCESS, &hKey) == ERROR_SUCCESS)
	{
		if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
			DeleteSubkeys(hKey, strMyKey);
		RegDeleteKey(hKey, strMyKey);
	}
	m_Progress.StepIt();
	m_regChk.ShowWindow(SW_SHOW);
	UpdateWindow();


}

void CUninstDlg::DeleteSubkeys(HKEY hKey, CString strKey)
{
	HKEY hKeyResult;

	//Delete Profile Settings
	if (RegOpenKeyEx(hKey, strKey, 0, KEY_ALL_ACCESS, &hKeyResult) == ERROR_SUCCESS)
	{
    
		FILETIME ft;
		TCHAR szTemp[MAX_PATH]; 
		DWORD dwSize = sizeof(szTemp); 
		//Because we delete as we enumerate, the index of the key stays at zero
		while (RegEnumKeyEx(hKeyResult, 0/*dwIndex*/, szTemp, &dwSize, NULL, NULL, NULL, &ft) != ERROR_NO_MORE_ITEMS)
		{
			DeleteSubkeys(hKeyResult, szTemp);
			RegDeleteKey(hKeyResult, szTemp);
			szTemp[MAX_PATH]; 
			dwSize = sizeof(szTemp);
		}
	}

	return;

}