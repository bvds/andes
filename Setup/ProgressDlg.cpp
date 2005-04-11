// ProgressDlg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "MySheet.h"
#include "InstDirPg.h"
#include "ExitDlg.h"
#include "VerMsgDlg.h"
#include "ProgressDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProgressDlg dialog


CProgressDlg::CProgressDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CProgressDlg::IDD, pParent)
{
	m_pSht = (CMySheet*)pParent;
	//{{AFX_DATA_INIT(CProgressDlg)
	m_bCancel = FALSE;

	//}}AFX_DATA_INIT
}


void CProgressDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CProgressDlg)
	DDX_Control(pDX, IDC_FILENAME, m_ctrlFileName);
	DDX_Control(pDX, IDC_PROGRESS1, m_ctrlCopyProgress);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CProgressDlg, CDialog)
	//{{AFX_MSG_MAP(CProgressDlg)
	ON_BN_CLICKED(IDC_STOP, OnCancel)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProgressDlg message handlers

BOOL CProgressDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	int numFiles = CalcNumFiles();
	numFiles = numFiles + 2;//Add 2 for the two shared system files
	//we also copy but are not in our files.h
	m_ctrlCopyProgress.SetRange(0, numFiles);
	m_ctrlCopyProgress.SetStep(1);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


int CProgressDlg::CalcNumFiles()
{
	
	return theApp.m_nFiles;
}

BOOL CProgressDlg::MyCopyFile(CString strFileName, CString strSourcePath, CString strDestPath)
{	
	CString strSourceFile = strSourcePath + strFileName;
	CString strDestFile = strDestPath + strFileName;
	m_ctrlFileName.SetWindowText(strFileName);
	//Remove read-only attribute
	DWORD dwOldAttr = GetFileAttributes(strSourceFile);
	DWORD dwNewAttr = dwOldAttr & (~FILE_ATTRIBUTE_READONLY);
	SetFileAttributes(strDestFile, dwNewAttr);
	//Have to step progress here, because files with newer version already installed
	//will not show progress
	m_ctrlCopyProgress.StepIt();
	BOOL bHitCancel = CheckCancelButton();
	if (bHitCancel){
		CExitDlg exDlg;
		if (exDlg.DoModal() == IDCANCEL)
			return FALSE;
	}
	if (theApp.FileExists(strDestFile))
	{
		// timestamp check not reliable across time-zone changes. since Winzip sets a timezone-relative
		// timestamp on files. For now, just omit this check (so always replace files). Can enable
		// the check while testing an installer to check for errors.
#if 0   
		if (theApp.ExistingFileNewer(strSourceFile, strDestFile))
		{
			if (theApp.m_bSkipNewer)//if we are to skip files where existing newer
				return TRUE;		//go to next file 
				
			//Otherwise put up message box that warns user that they are about to 
			//replace a file with an older version
			CVerMsgDlg dlg;
			CString str;
			str.Format(IDS_VERMSG, strFileName);
			dlg.m_strMsg = str;
			//User responds, skip replacement of this file,
			//skip replacement of all newer files, or replace anyway
			int nResp = dlg.DoModal();
			if (nResp == ID_SKIPALL)//if user chooses
				theApp.m_bSkipNewer = TRUE;//set boolean to skip replacement of all newer files
		
			if (nResp != IDCANCEL)//user clicked skip or skip all
				return TRUE;	//go to next file
				//if user chose replace anyway, we are continuing on
		}
#endif 0

		if ( !CopyFile(strSourceFile, strDestFile, FALSE) )
		{
			CString str;
			if (GetLastError() == ERROR_SHARING_VIOLATION){
				str.Format(IDS_ERROR_SHARING, strDestFile);
			}
			else
			{
				str.Format(IDS_ERROR_COPY, strSourceFile, strDestFile);
			}
			AfxMessageBox(str);
			return FALSE;//exit program
		}
	}
	else if (!CopyFile(strSourceFile, strDestFile, TRUE))
	{
		if (GetLastError() == ERROR_FILE_NOT_FOUND)
		{
			// !!! temp hack -- don't complain for missing sols.txt, since not all 
			// created yet.
			if (strSourceFile.Find("sols.txt") == -1) 
			{
				CString str;
				str.Format(IDS_ERROR_FNF, strSourceFile);
				AfxMessageBox(str);
				return FALSE;//exit program
			}
		}
		else//we encountered some other error other than file exists or not found
		{
			CString str;
			str.Format(IDS_ERROR_COPY, strSourceFile, strDestFile);
			AfxMessageBox(str);
			return FALSE;//exit program

		}

	}

	//if was read-only, return to read-only
//	dwNewAttr = dwOldAttr & (FILE_ATTRIBUTE_READONLY);
	//force a read-only
	dwNewAttr = dwOldAttr | (FILE_ATTRIBUTE_READONLY);
	SetFileAttributes(strDestFile, dwNewAttr);
	return TRUE;
}

BOOL CProgressDlg::MyCopyDLL(CString strFileName, CString strSourcePath, CString strDestPath)
{	
	CString strSourceFile = strSourcePath + strFileName;
	CString strDestFile = strDestPath + strFileName;
	m_ctrlFileName.SetWindowText(strFileName);
	//Remove read-only attribute
	DWORD dwOldAttr = GetFileAttributes(strSourceFile);
	DWORD dwNewAttr = dwOldAttr & (~FILE_ATTRIBUTE_READONLY);
	SetFileAttributes(strDestFile, dwNewAttr);
	//Have to step progress here, because files with newer version already installed
	//will not show progress
	m_ctrlCopyProgress.StepIt();
	BOOL bHitCancel = CheckCancelButton();
	if (bHitCancel){
		CExitDlg exDlg;
		if (exDlg.DoModal() == IDCANCEL)
			return FALSE;
	}
	if (theApp.FileExists(strDestFile))
	{
		if (theApp.HasCurrentDLL(strSourceFile, strDestFile))
		{//user already has the current or a more current DLL
			return TRUE;		
		}
		if ( !CopyFile(strSourceFile, strDestFile, FALSE) )
		{
		//Now we only show sharing violation if we are copying newer versions of 
		//the dll's.  This is checked in CSetupApp within the IsExistingDLLNewer
		//(returns FALSE if we are copying a newer dll)
		//if they have the correct dll, we ignore the sharing violation and continue
		//with the setup program
			CString str;
			UINT e = GetLastError();
			if (e == ERROR_SHARING_VIOLATION || e == ERROR_ACCESS_DENIED)
			{

				CString strOldDestFile = strDestFile;
					
				CString strMyDll;
				strMyDll.Format("My%s", strFileName);
						
				strDestFile = strDestPath + strMyDll;
				dwOldAttr = GetFileAttributes(strSourceFile);
				dwNewAttr = dwOldAttr & (~FILE_ATTRIBUTE_READONLY);//removeReadOnly
				SetFileAttributes(strDestFile, dwNewAttr);			
				if (!CopyFile(strSourceFile, strDestFile, FALSE))
				{
					str.Format(IDS_ERROR_COPY, strSourceFile, strDestFile);
					AfxMessageBox(str);
					return FALSE;//exit program
				}
				if (theApp.m_dwOS == VER_PLATFORM_WIN32_NT)
				{
					MoveFileEx(strDestFile, strOldDestFile, MOVEFILE_DELAY_UNTIL_REBOOT|MOVEFILE_REPLACE_EXISTING);
				}
				else //windows 95
				{
					theApp.RenameOnStartup(strOldDestFile, strDestFile);//change name back
				}
				theApp.m_bReboot = TRUE;//need to reboot

			}
			else
			{
				str.Format(IDS_ERROR_COPY, strSourceFile, strDestFile);
				AfxMessageBox(str);
				return FALSE;//exit program
			}
		}
	}
	else if (!CopyFile(strSourceFile, strDestFile, TRUE))
	{//will return false if file already exists on the computer, but we already checked
		
		if (GetLastError() == ERROR_FILE_NOT_FOUND)
		{
			CString str;
			str.Format(IDS_ERROR_FNF, strSourceFile);
			AfxMessageBox(str);
			return FALSE;//exit program
		}
		else//we encountered some other error other than file exists or not found
		{
			CString str;
			str.Format(IDS_ERROR_COPY, strSourceFile, strDestFile);
			AfxMessageBox(str);
			return FALSE;//exit program

		}
	}
	//if was read-only, return to read-only
//	dwNewAttr = dwOldAttr & (FILE_ATTRIBUTE_READONLY);
	//force a read-only
	dwNewAttr = dwOldAttr | (FILE_ATTRIBUTE_READONLY);
	SetFileAttributes(strDestFile, dwNewAttr);
	return TRUE;
}


void CProgressDlg::PostNcDestroy() 
{
	delete this;
	
	CDialog::PostNcDestroy();
}

void CProgressDlg::OnCancel() 
{
	m_bCancel = TRUE;
}

void CProgressDlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CDialog::OnShowWindow(bShow, nStatus);
	if (bShow)
		m_pSht->ShowWindow(SW_HIDE);
		
}

void CProgressDlg::PumpMessages()
{
    // Must call Create() before using the dialog
    ASSERT(m_hWnd!=NULL);

    MSG msg;
    // Handle dialog messages
    while(PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
    {
      if(!IsDialogMessage(&msg))
      {
        TranslateMessage(&msg);
        DispatchMessage(&msg);  
      }
    }
}

BOOL CProgressDlg::CheckCancelButton()
{
    // Process all pending messages
    PumpMessages();

    // Reset m_bCancel to FALSE so that
    // CheckCancelButton returns FALSE until the user
    // clicks Cancel again. This will allow you to call
    // CheckCancelButton and still continue the operation.
    // If m_bCancel stayed TRUE, then the next call to
    // CheckCancelButton would always return TRUE

    BOOL bResult = m_bCancel;
    m_bCancel = FALSE;

    return bResult;
}
