// TransferDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "TransferDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTransferDlg dialog


CTransferDlg::CTransferDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CTransferDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CTransferDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT

	m_bUpload = TRUE;
	m_bQuit = FALSE;
	m_bDebug = FALSE;
	m_bLogUploadFailed = TRUE; 

	// Wininet handles
	hInet = NULL;
	hFTP = NULL;
	
	// compiled-in account defaults for Pitt Fall 01 experiment
        // BvdS this must be dead code, if not, find how to kill it.
	// m_strHost = "unix.cis.pitt.edu";
	// m_strUserName = "andes2";
	// m_strPassword = m_strUserName;
}


void CTransferDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTransferDlg)
	DDX_Control(pDX, IDC_STATUS_MSG, m_textStatusMsg);
	DDX_Control(pDX, IDC_TRANSFER, m_textTransferMsg);
	DDX_Control(pDX, IDC_MESSAGE_TEXT, m_textMsg);
	//}}AFX_DATA_MAP
}

// message to ourselves to run the transfer
#define WM_DOIT		WM_APP + 17		// arbitrary, shouldn't matter

BEGIN_MESSAGE_MAP(CTransferDlg, CDialog)
	//{{AFX_MSG_MAP(CTransferDlg)
	ON_WM_CLOSE()
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
	ON_MESSAGE(WM_DOIT, DoTransfer)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTransferDlg message handlers



BOOL CTransferDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// Customize message with student name
	CString strMsg;
	strMsg.Format("%s student files for %s", m_bUpload ? "Uploading" : "Downloading",
		                                    m_strStudent);
	m_textTransferMsg.SetWindowText(strMsg);


	// Since there may not be registry entries, we can tell if we're debugging by 
	// presence of a marker file named "debug". 
	CFileStatus statFile;
	m_bDebug = CFile::GetStatus(g_strAndesDir + "Debug", statFile);
	
	// Put message in our queue to actually do it after brief delay, so tranfser
	// starts automatically after we return to framework and it runs the message loop
	// (Need delay for some reasons. Framework will show dialog on first idle point in msg loop.)
	SetTimer(WM_DOIT, 100, NULL);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CTransferDlg::OnTimer(UINT nIDEvent) 
{
	if (nIDEvent == WM_DOIT) {	
		KillTimer(nIDEvent);
		DoTransfer();
	} else // maybe base class wants it?
		CDialog::OnTimer(nIDEvent);
}

// Do the work
void CTransferDlg::DoTransfer()
{
	CString strMsg;

try_again:
	BOOL bSuccess = FALSE;
	// reset log upload success flag, the one failure we bother the student about
	m_bLogUploadFailed = m_bUpload;		// until upload succeeds. FALSE if downloading
	
	// open the wininet internet library
	if ( !(hInet = InternetOpen ("Andes Workbench",  INTERNET_OPEN_TYPE_PRECONFIG, 
		                          NULL, 0, 0))) {
        Error(GetLastError(), "Couldn't initialize Internet library. ");
    } else {

		// try to make the FTP connection
		strMsg.Format("Attempting FTP connection to %s", m_strHost); 
		AddLog(strMsg);
		{ 
			CWaitCursor hourglass;	// shows hourglass cursor till end of block
			hFTP = InternetConnect (hInet, m_strHost, INTERNET_INVALID_PORT_NUMBER, 
									m_strUserName,  m_strPassword, INTERNET_SERVICE_FTP, 
									/* INTERNET_FLAG_PASSIVE */ 0, 0);
		}
		if (! hFTP) {	
			Error (GetLastError(), "Couldn't connect to server. ");			
		} else {
			// connected OK		
			AddLog("Connected");
			// 
			// do all the appropriate upload or download work
			//
			bSuccess = m_bUpload ? DoUpload(hFTP) : DoDownload(hFTP);
		}
	}

	// close the handles
	InternetCloseHandle(hFTP);
	InternetCloseHandle(hInet);

	// Note end of process in log and status
	AddLog("Finished");
	m_textStatusMsg.SetWindowText("Transfers Completed.");

	// if uploading and logs didn't make it, prompt to wait then try again
	if (m_bLogUploadFailed) {
		int nChoice = AfxMessageBox("Failed to upload log files. Please wait a bit then press Retry to try again\n\
If failure persists, press Cancel to give up and see more instructions." ,MB_ICONEXCLAMATION| MB_RETRYCANCEL);
		if (nChoice == IDRETRY)
			goto try_again;
	}

	// get here => succeeded or gave up trying. Open instruction page. 
	if (m_bLogUploadFailed) {
		const char* szPageName = "UploadFailure.htm";
		CString strPage = g_strAndesDir + szPageName;
		HINSTANCE hInst = ShellExecute(NULL, "open", strPage, NULL, NULL, SW_SHOWNORMAL); 
 		if ((int) hInst <= 32) {
 			AfxMessageBox("Couldn't open instruction page. Save files in C:\\Temp\\Andes\\log, \
note messages in Transfer window, and notify experimenters.");
		}
	}

	// Finished: Normally end dialog automatically when done. 
	// if failure or debugging, leave it up so can view log contents.
	// Return status indicates success or failure, in case it is useful to caller.
	if (!m_bLogUploadFailed && ! m_bDebug)
		EndDialog(bSuccess ? IDOK : IDCANCEL);
}

// Local directory names
const char * szLogDirName = "Log";
const char * szSolutionPath = "Problems\\Solutions\\";
// Remote Directory names
const char * szUploadDir = "upload";
const char * szUploadSolutionDir = "Solutions";
// file patterns
const char * szLogWildcard = "*.log";
const char * szStudentWildcard = "*.dat";
const char * szSolutionWildcard = "*.fbd";

BOOL CTransferDlg::DoUpload(HINTERNET hFTP)
{
	CString strMsg;
	int nLogs = 0; int nFailed = 0;
	int nStudentFiles = 0; int nStudentFailed = 0;
	int nSols = 0; int nSolsFailed = 0;
	
	// Local log folder and Student solution folder pathnames
	CString strLogDir = g_strAndesDir + szLogDirName;
	CString strStudentSolnDir = g_strAndesDir + szSolutionPath + m_strStudent;

	// change to the designated upload directory on the server machine
	strMsg.Format("CD'ing to %s", szUploadDir);  AddLog(strMsg);	
	if (! FtpSetCurrentDirectory(hFTP, szUploadDir) ) {
		 Error (GetLastError(), "Couldn't change to upload directory. ");
	} else
		AddLog("CD OK");

	// Now run loop to upload each log data file. Should normally be only one from
	// current session, maybe a few if they ran Andes a few times.
	AddLog("Transferring session logs:");
	MPut(hFTP, strLogDir, szLogWildcard, nLogs, nFailed);
	strMsg.Format("Transferred %d of %d log files", nLogs - nFailed, nLogs); AddLog(strMsg);
	// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	// !!! If log upload failed we have to do something to avoid loss of data!!!!!!!
	///!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	m_bLogUploadFailed = (nFailed > 0);

	// Upload Student.dat, stored in local Log directory in andes2 (note may not exist.)
	AddLog("Transferring student history");
	MPut(hFTP, strLogDir, szStudentWildcard, nStudentFiles, nStudentFailed);
	strMsg.Format("Transferred %d of %d student files", nStudentFiles - nStudentFailed, nStudentFiles);
	AddLog(strMsg);
	
	// now upload student's problem set solution folders, if any. 
	
	// try to create subdirectory for this student's files. Create by studentname
	// Will fail if already exists but we barge on.
	// !!! Assuming remote Solutions directory exists -- should create if not.
	CString strStudentDir = szUploadSolutionDir;
	strStudentDir += "/" + m_strStudent;	// path relative to CWD = upload
	strMsg.Format("Creating subdirectory for %s", strStudentDir); AddLog(strMsg);
	if (! FtpCreateDirectory(hFTP, strStudentDir) ){
		Error(GetLastError(), "Couldn't create, may exist.");
	} else  {
		AddLog("MDKIR OK");
	}

	// try to change into the upload/student, the students solution directory 
	strMsg.Format("CD'ing to %s", strStudentDir); AddLog(strMsg);
	if (! FtpSetCurrentDirectory(hFTP, strStudentDir) ){
		Error(GetLastError(), "CD failed!");
		// can't proceed in this case, all the solutions will clobber one another
		AddLog("Giving up solution transfer");
		return FALSE; // ! could be OK if no local solutions saved.
	} else 
		AddLog("CD OK");

	// Now want to iterate over local problem set solution folders, creating remote dir and
	// copying all .fbd files in each
	BOOL bFinished = FALSE;
	HANDLE hSearch;
	WIN32_FIND_DATA FileData;
	hSearch = ::FindFirstFile(strStudentSolnDir + "\\" + "*.*", &FileData);
	if (hSearch == INVALID_HANDLE_VALUE) {
		AddLog("No solution folders found"); // shouldn't happen, but not an error
		::FindClose(hSearch);
		goto done;
	}
	while (! bFinished) {
		CString strFolderName = FileData.cFileName;
		if ((FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) 
			&& strFolderName != "." && strFolderName != "..") {

			// ensure remote problem set directory exists.
			// can't search with FtpFindFirstFile since name will likely contain spaces.
			// So just try and create and assume exists if failure.
			strMsg.Format("Creating Directory %s", strFolderName); AddLog(strMsg);
			if (! FtpCreateDirectory(hFTP, strFolderName) ){
				Error(GetLastError(), "(May already exist).");
			} else 
				AddLog("MDKIR OK");

			// change into problem set directory (needed by mput [?] -- maybe should change.
			strMsg.Format("CD'ing to %s", strFolderName); AddLog(strMsg);
			if (! FtpSetCurrentDirectory(hFTP, strFolderName) ){
				Error(GetLastError(), "CD failed ");
				// !!! can't proceed in this case, all the solutions will clobber one another
			} else 
				AddLog("CD OK");

			// mput *.fbd files into it
			AddLog("Transferring solution files:");
			int nSolsThisDir = 0;
			int nErrorsThisDir = 0;
			MPut(hFTP, strStudentSolnDir + "\\" + strFolderName, "*.fbd", nSolsThisDir, nErrorsThisDir);
			strMsg.Format("Transferred %d of %d solution files", nSolsThisDir - nErrorsThisDir, nSolsThisDir); 
			AddLog(strMsg);
			nSols += nSolsThisDir; nSolsFailed += nErrorsThisDir;
	
			// pop back to parent dir = student solution folder before next iter
			strMsg.Format("CD'ing to %s", ".."); AddLog(strMsg);
			if (! FtpSetCurrentDirectory(hFTP, "..") ){
				Error(GetLastError(), "CD failed!");
				// !!! can't proceed in this case, solutions in wrong place
				AddLog("Giving up solution transfer");
				break;
			} else AddLog("CD OK");
		}
	
		// done current probset folder, continue iteration
		bFinished = ! ::FindNextFile(hSearch, &FileData);
	}
	::FindClose(hSearch);  // done iterating problem set folders.
	

done:
	// if everything succeeded we are done
	BOOL bSucceeded = (!m_bQuit && (nFailed == 0) && (nStudentFailed == 0) && (nSolsFailed == 0));
	if (bSucceeded) {
		//AfxMessageBox("Data Upload completed successfully, Thank you. Press Quit to exit.");
		return TRUE;
	} else {
		//AfxMessageBox("Some error occurred");
		return FALSE;
	}
}

// put all files from local strDir matching Wildcard.
// puts into current remote directory with same filename.
void CTransferDlg::MPut(HINTERNET hFTP, CString& strDir, LPCTSTR szWildcard, int& nFiles, int& nFailed)
{
	// nFiles = 0; nFailed = 0; // accumulators:  may be non-zero coming in	
	BOOL bFinished = FALSE;
	HANDLE hSearch;
	WIN32_FIND_DATA FileData;
	CString strMsg;
	BOOL bSucceeded;

	CString strSpec = strDir + "\\" + szWildcard; 
	hSearch = ::FindFirstFile(strSpec , &FileData);
	if (hSearch == INVALID_HANDLE_VALUE) {
		Error(GetLastError(), "No files found");
		return;
	}
	
	while ( ! bFinished )
	{
		// get current filename
		m_strFileName = FileData.cFileName;
		
		// ignore special directory files matched with *.*
		if (m_strFileName.IsEmpty() || m_strFileName == "." || m_strFileName == "..")
			goto NextFile; // skips over transfer code

		// update count of files tried to transfer:
		++nFiles;

		// Get source pathname for the transfer.
		m_strPathName = strDir + "\\" + m_strFileName;
		
		strMsg.Format("Sending (%i): %s", nFiles, m_strPathName); // AddLog(strMsg); 
		m_textStatusMsg.SetWindowText(strMsg);

		// Do it:  dst name is just file name (will put it relative to current remote directory)
		// Note this call not interruptible. We pump events between files only
		{ CWaitCursor hourglass;
		   bSucceeded = FtpPutFile (hFTP, m_strPathName, m_strFileName, 
						              FTP_TRANSFER_TYPE_BINARY, 0 );
		}
		if (! bSucceeded)
		{
			strMsg.Format("Couldn't transfer %s", m_strPathName);
			Error (GetLastError(), strMsg);
			++nFailed;
		}
		if (!ProcessPendingEvents()) {
			AddLog("MPut cancelled by user.");
			return;
		}

NextFile: // advance search to next file
		if (!::FindNextFile(hSearch, &FileData)) 
		{
			if (::GetLastError() == ERROR_NO_MORE_FILES) 
				bFinished = TRUE; 
			else 
				AddLog("Couldn't find next file!"); 
		}
	}

	::FindClose(hSearch);
}

// Add given message to log.
void CTransferDlg::AddLog(LPCTSTR pszMsg)
{
	m_strLog += pszMsg;
	m_strLog += "\r\n";
	TRACE("FTP: %s\n", pszMsg);
	m_textMsg.SetWindowText(m_strLog);
	m_textMsg.LineScroll(m_textMsg.GetLineCount() - 9);   // !!! really want to scroll so last line is visible 
	// m_textMsg.UpdateWindow();
}

// Add error message for given error to log, Appending extended info if available.
void CTransferDlg::Error(DWORD dwError, LPCTSTR pszMsg)
{
	CString strMsg; 
	strMsg.Format("ERROR (%d). %s ", dwError, pszMsg);
	
	if (dwError == ERROR_INTERNET_EXTENDED_ERROR) // we have extended info
	{
		DWORD  dwIntError;
		char szBuffer[255] = "";
		DWORD dwLength = 255;

		InternetGetLastResponseInfo (&dwIntError, szBuffer, &dwLength);
		//TRACE("Error %d: %s\n", dwError, szBuffer);

		strMsg += szBuffer;
	}

	AddLog(strMsg);
}


// Poll for and dispatch any pending user inputs. 
// Sprinkle calls to this inside any time-consuming loop.
// Return True iff app should continue, i.e. m_bQuit not set by handler
BOOL CTransferDlg::ProcessPendingEvents()
{
	MSG msg;
	while (!m_bQuit && ::PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE))
	{
		AfxGetThread()->PumpMessage(); 
	}
	return ! m_bQuit; 
}

void CTransferDlg::OnClose() 
{
	m_bQuit = TRUE;	
	CDialog::OnClose();
}

void CTransferDlg::OnCancel() 
{
	m_bQuit = TRUE;
	CDialog::OnCancel();
}




BOOL CTransferDlg::DoDownload(HINTERNET hFTP)
{
	CString strMsg;
	HINTERNET hSearch;
	WIN32_FIND_DATA FileData;

	// Local log folder and Student solution folder names
	CString strLogDir = g_strAndesDir + szLogDirName;
	CString strStudentFile = m_strStudent + ".dat";
	CString strLocalStudentPath = strLogDir + "\\" + strStudentFile;
	CString strSolutionPath = g_strAndesDir + szSolutionPath;
	CString strStudentSolnDir = g_strAndesDir + szSolutionPath + m_strStudent;
	
	BOOL bStudentFolder = TRUE;		// found a student solution folder

	// change to the designated upload directory on the server machine
	strMsg.Format("CD'ing to %s", szUploadDir);  AddLog(strMsg);	
	if (! FtpSetCurrentDirectory(hFTP, szUploadDir) ) {
		 Error (GetLastError(), "Couldn't change to upload directory. ");
	} else
		AddLog("CD OK");

	// try to get student.dat file from upload directory -- may fail if doesn't exist.
	strMsg.Format("Looking for %s", strStudentFile); AddLog(strMsg);
	hSearch = ::FtpFindFirstFile(hFTP, strStudentFile, &FileData, NULL, NULL);
	::InternetCloseHandle(hSearch);	// NB: only one search may be open at a time.
	if (hSearch == NULL) 
		AddLog("No Student.dat file found");
	else {
		if (! ::FtpGetFile(hFTP, strStudentFile, strLocalStudentPath, 
						  /*FailIfExists*/ FALSE, FILE_ATTRIBUTE_NORMAL, 
		                   FTP_TRANSFER_TYPE_BINARY, NULL) ) {
			Error(GetLastError(), "Couldn't get student file.");
		} else {
			AddLog("Transferred OK");
		}
	}

	// Look for studentName subdirectory in upload/Solutions directory
	// Note Student's whole directory may not exist if new student.
	CString strStudentDir = szUploadSolutionDir;
	strStudentDir += "/" + m_strStudent;		// path relative to CWD
	
	// attempt to change into the student solution folder 
	strMsg.Format("CD'ing to %s", strStudentDir);  AddLog(strMsg);	
	if (! FtpSetCurrentDirectory(hFTP, strStudentDir) ) {
			Error (GetLastError(), "CD failed, may not exist.");
			AddLog("Assuming no saved solutions.");
			return TRUE;
	} else AddLog("CD OK");

	// Download student's solution folder tree if it exists -- each problem set folder 
	// and each solution file in each problem set folder

	// First search to list all the subdirectories we need. Collect them once because
	// only one search may be open at a time.
	CStringList strProblemFolders;
	hSearch = ::FtpFindFirstFile(hFTP, "*.*", &FileData, NULL, NULL);
	if (hSearch == NULL && GetLastError() == ERROR_NO_MORE_FILES)
		AddLog("No Problem Set Folders found in Student Dir!");
	else {
		BOOL bFilesRemaining = TRUE;
		while (bFilesRemaining) {
			// get current filename
			CString strFileName = FileData.cFileName;
			
			// Add any directories to our list
			// ignore special directory files, if returned.
			if ((FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) 
				&& m_strFileName != "." && m_strFileName != "..") {
				TRACE("Found remote directory %s\n", strFileName);
				strProblemFolders.AddTail(strFileName);
			}

			bFilesRemaining = ::InternetFindNextFile(hSearch, &FileData);
		}
	} 
	InternetCloseHandle(hSearch);
	strMsg.Format("%d Problem Set Folders found", strProblemFolders.GetCount());
	AddLog(strMsg);

	// do for each problem set folder in list:
	POSITION pos = strProblemFolders.GetHeadPosition();
	while (pos != NULL) {
		CString strFolderName = strProblemFolders.GetNext(pos);
		strMsg.Format("Getting solutions from %s", strFolderName); AddLog(strMsg);

		// ensure local student folder exists
		CString strDir = strStudentSolnDir;
		TRACE("Ensuring local folder %s\n", strDir);
		CFileStatus statDir;
		if (! CFile::GetStatus(strDir, statDir)) {
			if (!::CreateDirectory(strDir, NULL)) {
				Error(GetLastError(), "Couldn't create local folder");
			} else {
				strMsg.Format("Created %s", strDir); AddLog(strMsg);
			}
		}
		// ensure local problem set folder exists
		strDir = strStudentSolnDir + "\\" + strFolderName;
		TRACE("Ensuring local folder %s\n", strDir);
		if (! CFile::GetStatus(strDir, statDir)) {
			if (!::CreateDirectory(strDir, NULL)) {
				Error(GetLastError(), "Couldn't create local folder");
			} else {
				strMsg.Format("Created %s", strDir); AddLog(strMsg);
			}
		}
		
		// Note: no blank spaces allowed in ftpFindFirstFile spec string. Because problem
		// set folders have blanks means can't just search for, e.g. 
		// "Getting Started/*.fbd". So we CD into the directory (that should work) first.
	
		// change into subdirectory on remote system.
		strMsg.Format("CD'ing to %s", strFolderName);  AddLog(strMsg);	
		if (! FtpSetCurrentDirectory(hFTP, strFolderName) ) {
			Error (GetLastError(), "Couldn't change to Problem Set directory! ");
			AddLog("Skipping problem set");
			goto next;
		} else AddLog("CD OK");

		// mget strFolderName/*.fbd into the local folder. Note this always overwrites
		// any existing copies. Could check dates to make sure existing version is not
		// newer.
		hSearch = ::FtpFindFirstFile(hFTP, "*.fbd", &FileData, NULL, NULL);
		if (hSearch == NULL) {
			Error(GetLastError(), "No solution files found in folder.");
		} else {
			BOOL bFilesRemaining = TRUE;
			while (bFilesRemaining) {
				CString strFileName = FileData.cFileName;
				CString strLocalPath = strDir + "\\" + strFileName;
				CString strRemotePath = strFileName; // rel. to cwd
				
				strMsg.Format("Getting %s", strFileName); AddLog(strMsg);
				if (! ::FtpGetFile(hFTP, strRemotePath, strLocalPath, 
								  /*FailIfExists*/ FALSE, FILE_ATTRIBUTE_NORMAL, 
		                           FTP_TRANSFER_TYPE_BINARY, NULL) ) {
					strMsg.Format("Couldn't get solution %s", strFileName);
					Error(GetLastError(), strMsg);
				} else {
					AddLog("Transferred OK");
				}

				bFilesRemaining = ::InternetFindNextFile(hSearch, &FileData);
			}
		}
		InternetCloseHandle(hSearch); // End mget ProblemSet/*.fbd

next:
		// change back to parent before next iteration
		strMsg.Format("CD'ing to %s", "..");  AddLog(strMsg);	
		if (! FtpSetCurrentDirectory(hFTP, "..") ) {
			Error (GetLastError(), "Couldn't change to ..!");
			AddLog("Quitting.");
			break;
		} else AddLog("CD OK");
	} 

	return TRUE;
}

