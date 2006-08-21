// ProblemSet.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "mainfrm.h"
#include "FBDDoc.h"
#include "ProblemSet.h"
#include "Base64Coder.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProblemSet

IMPLEMENT_DYNCREATE(CProblemSet, CDocument)

CProblemSet::CProblemSet()
{
	m_bOli = FALSE;
	m_bViewSolution = FALSE;
}

BOOL CProblemSet::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;
	return TRUE;
}

CProblemSet::~CProblemSet()
{
	// free problem list elements
	while (!m_tasks.IsEmpty())
   		delete m_tasks.RemoveHead();
}


BEGIN_MESSAGE_MAP(CProblemSet, CDocument)
	//{{AFX_MSG_MAP(CProblemSet)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProblemSet diagnostics

#ifdef _DEBUG
void CProblemSet::AssertValid() const
{
	CDocument::AssertValid();
}

void CProblemSet::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CProblemSet serialization
static const char szHdrFmt[] = "ANDES Problem Set %d\r\n"; // one arg for version no.
#define PROBSET_VERSION 2	// latest file format version number

void CProblemSet::Serialize(CArchive& ar)
{
	int nVersion = PROBSET_VERSION; // default unless read in older version
	// Serialize the header
	CString strLine;
	if (ar.IsStoring()) {
		// write header line with file format version number
		strLine.Format(szHdrFmt, nVersion);
		ar.WriteString(strLine);
	} else /* Loading */ {
		// for OLI demo: skip any initial blank lines
		while (ar.ReadString(strLine) && strLine.IsEmpty())
			continue;

		// verify header line and get file format version number
		if (! (sscanf(strLine, szHdrFmt, &nVersion) == 1) )
			AfxThrowArchiveException(CArchiveException::badSchema, NULL);
	}
	
	// Serialize the rest based on version
	if (nVersion == 1) {   // MFC binary format
		m_opts.Serialize(ar);
		m_tasks.Serialize(ar);
	} else { // plain text format
		Serialize2(ar);
	}
}

extern int split(const CString& str, const CString& Sep, CStringArray& result);

// version two reads/writes problem ids as text lines
// now extended to allow options and problem options in text format as well.
void CProblemSet::Serialize2(CArchive& ar)
{
	POSITION pos = m_tasks.GetHeadPosition();
	if (ar.IsStoring()) {
		while (pos) {
			// !!! This doesn't write back problem set option values 
			
			CTask* pTask = m_tasks.GetNext(pos);
			ar.WriteString(pTask->m_strName + "\r\n");
			// !!! This doesn't write back task option values
		}
	} 
	else /* Loading */ 
	{
		// load tasks one to a non-empty line. 
		// Added for OLI: can also have problem set options on lines of form 
        //              option: value
		// A task line has problem name first. This may be followed by space-separated 
		// option=value setting for this particular problem:
		//   exkt1a FlagMode=1 Calculator=0 ..,.
		// For boolean options, can leave of the "=1" to turn on an option, e.g.:
		//    exkt1a  FlagMode
		// means to set the "FlagMode" option to 1 for true. 
		///  
		//
		CString strLine;
		CTask* pTask;
		while (ar.ReadString(strLine) && ! strLine.IsEmpty())
		{
			// see if it's an option:value line
			int iColon = strLine.Find(":");
			if (iColon != -1)		
			{ 
				CString strValue = strLine.Mid(iColon+1);
				strValue.TrimLeft();
				m_opts.SetAt(strLine.Left(iColon), strValue);

				// Since options now only set in OLI docs, set OLI mode flag
				m_bOli = TRUE;
				continue;
			}

			// else its a task line
			pTask = new CTask();
			pTask->m_pSet = this; 
			CStringArray strFields;
			split(strLine, " ", strFields);
			// name is first. May have optional extension after . indicating type
			CStringArray strNameParts;
			split(strFields[0], ".", strNameParts);
			pTask->m_strName = strNameParts[0];
			if (strNameParts.GetSize() > 1)
				pTask->m_strExt = strNameParts[1];

			// parse optional list of name[=val] problem option values
			for (int i = 1; i < strFields.GetSize(); i++)
			{
				CString strOpt = strFields[i];
				if (strOpt.IsEmpty()) continue;

				// option string may or may not contain equal sign
				int posEq = strOpt.Find('=');
				if (posEq == -1) {
					// no equal sign: assume boolean option to be set to 1
					pTask->m_opts.SetAt(strOpt, "1");
				} else {
					pTask->m_opts.SetAt(strOpt.Left(posEq), strOpt.Mid(posEq + 1));
				}
			}
			
			// TRACE("Loaded task %s w/options: ", pTask->m_strName); pTask->DumpOpts(); TRACE("\n");
			m_tasks.AddTail(pTask);
		}
	}
}

IMPLEMENT_SERIAL( CTask, CObject, 1 )
void CTask::Serialize(CArchive& ar) 
{
	if (ar.IsStoring())
	{
		ar << m_strName;
		ar << m_strExt;
		// work state is dynamically computed
	}
	else
	{
		m_pSet = (CProblemSet*) ar.m_pDocument; // set back pointer from archive

		ar >> m_strName;
		ar >> m_strExt;
		if (m_strExt.IsEmpty()) 
			m_strExt = ".fbd";
		// work state is dynamically computed
	}
	m_opts.Serialize(ar);
}

#ifdef _DEBUG
void CTask::DumpOpts()
{
	POSITION pos; CString strName;
	CString strValue;
	for( pos = m_opts.GetStartPosition(); pos != NULL; )
   {
	  m_opts.GetNextAssoc( pos, strName, strValue );
      afxDump << strName << " : " << strValue << "\n";
   }
}
#endif 

/////////////////////////////////////////////////////////////////////////////
// CProblemSet commands

// Note we assume tasks have unique basenames, regardless of extensions (types)
CTask* CProblemSet::FindTask(LPCTSTR pszName)
{
	POSITION pos = m_tasks.GetHeadPosition();
	while (pos != NULL) {
		CTask* pTask = m_tasks.GetNext(pos);
		if (pTask->m_strName.CompareNoCase(pszName) == 0)
			return pTask;
	}
	return NULL;
}

void CProblemSet::AddTask(LPCTSTR pszName, LPCTSTR pszExt/*=".fbd"*/)
{
	if (FindTask(pszName)) return; // don't add if it's already there.

	CTask* pTask = new CTask;
	pTask->m_strName = pszName;
	pTask->m_strExt = pszExt;

	m_tasks.AddTail(pTask);
	pTask->m_pSet = this;

	SetModifiedFlag();
	// update all views?
}

void CProblemSet::RemoveTask(LPCTSTR pszName)
{
	CTask* pTask = FindTask(pszName);
	if (pTask == NULL)
		return;

	POSITION pos = m_tasks.Find(pTask);
	if (pos != NULL) {
		m_tasks.RemoveAt(pos);
		// and free it
		delete pTask;
	}
	
	SetModifiedFlag();
	// update all views?
}

//
// Hook File open/close events on behalf of app
// 

// On opening problem set, find solution files for each problem and update status
BOOL CProblemSet::OnOpenDocument(LPCTSTR lpszPathName) 
{
	if (!CDocument::OnOpenDocument(lpszPathName))
		return FALSE;

	// Do if doc opened for student working (not author editing). Check via template.
	// !!!Currently there are different templates for each, so if opened both ways
	// (possible in author mode) we get two different documents in memory.
	CDocTemplate* pTmpl = GetDocTemplate();
	if ( !(pTmpl && pTmpl == theApp.m_ptmplProbSet) )
		return TRUE;

	// Set PS name from filename (doc's filename not set at this point in open)
	// Used below to derive solution directory name
	GetName(lpszPathName);

	// ?Ensure Problem Directory exists? (for now, defer till save).

	// Update work state for each problem for display.
	UpdateSolutionState();

	// If this is autoopen document, see if we can set student name
	if (m_bOli) {
		CString strUserId;
		// if we get a "studentid" in addition to a userid, we are viewing that
		// student's solution, so use that idea and set flag
		if (m_opts.Lookup("studentid", strUserId)) {
			m_bViewSolution = TRUE;
		} else if (!m_opts.Lookup("userid", strUserId)){
			TRACE("User id not found in atd!\n");	// SHOULDN'T HAPPEN
			return FALSE;
		}
		// set global student id in Andes. Note might also have to adjust helpsys read-student-info, 
		// or logfilename or session id after this, depending on application state when it occurs.
		// If this occurs before user initialization, as when opening an OLI .atd, should be OK.
		theApp.m_strUserName = strUserId;
	}
	
	return TRUE;
}

void CProblemSet::GetName(CString strPath)
{
	// for OLI problems, always treat problem set name as "oli" for purposes of
	// storing solutions. Default method doesn't work since internet explorer may
	// name downloaded .atd file oli[1], oli[2] 
	if (m_bOli) {
		m_strName = "oli";
		return;
	}
	int iLastDot = strPath.ReverseFind('.');
	int iFileName = strPath.ReverseFind('\\') + 1;
	m_strName = strPath.Mid(iFileName, iLastDot - iFileName);
	// in case we are opening an IE-downloaded .aps file not from OLI (as in Min
	// Chi experiment): If extracted basename contains a '[', take only up to that:
	int iLBracket = m_strName.ReverseFind('[');
	if (iLBracket != -1)
		m_strName = m_strName.Mid(iLBracket);
}

// determine solution states for all problems based on what we can find in
// Andes file system
void CProblemSet::UpdateSolutionState()
{
	POSITION pos = m_tasks.GetHeadPosition();
	while (pos != NULL) {
		CTask* pTask = m_tasks.GetNext(pos);
		// Locate solution file for this problem, if it exists
		CString strSolnPath = pTask->GetSolutionPath();

		// Update solution status. CDoc static knows how to load it from file
		pTask->m_work = CFBDDoc::LoadWorkState(strSolnPath);
	}
	UpdateAllViews(NULL);
}

// return full path to be used for solution file for given problem
// might be stored in probinfo, but for now derive from student/probset/prob name
CString CTask::GetSolutionPath(BOOL bEnsureExists/*=FALSE*/)
{
	CString strDir = m_pSet->GetStudentSolnDir();	// subdir for current student's solutions
	CFileStatus statDir;
	if (bEnsureExists && ! CFile::GetStatus(strDir, statDir)
		 && ! ::CreateDirectory(strDir, NULL)) {
		TRACE("Couldn't create student soln dir (err=%d) %s\n", GetLastError(), strDir);
		return "";
	}
	
	strDir += "\\" + m_pSet->m_strName;				// Subdir for this Problem Set 
	if (bEnsureExists && ! CFile::GetStatus(strDir, statDir)
		 && ! ::CreateDirectory(strDir, NULL)) {
		TRACE("Couldn't create probset soln dir (err=%d) %s\n", GetLastError(), strDir);
		return "";
	}
		
	return strDir + "\\" + m_strName + "-Solution" + m_strExt;
}

// Return path to subdir holding Prob set solution folders for current student
CString CProblemSet::GetStudentSolnDir()
{
	// What if student hasn't identified himself (working w/o help sys)?
	// Could prompt for student name. For now, just use ANONYMOUS
	// !!! possible some wag has already picked this name -- could include date. 
	// !!! No good way to return to this if logged in w/different name (general prob).
	CString strStudent = theApp.m_strUserName;
	if (strStudent.IsEmpty())
		strStudent = "ANONYMOUS";

	return g_strAndesDir + g_szProblemDir + "\\"  + g_szSolutionDir + "\\" + strStudent;
}

//-------------------------------------------------------------------------------------
// Support for OLI operations on opening special oli problem set document (.atd file)
//-------------------------------------------------------------------------------------

// Http* functions are generic routines to make HTTPS calls via WinInet functions:

// This calls an OLI function via a URL, with only success/failure expected back.
// though may get an error page on failure.
int HttpCallURL(LPCTSTR szURL, LPCTSTR szCallId) // CallId used to name error page file
{
	int result = 1;
	CInternetSession session("ANDES Session");
	CHttpFile* pFile = NULL;
	try
	{
		TRACE("Trying %s\n", szURL);
		// MFC Wrapper parses https: and sets secure flag in underlying WinInet call 
		pFile = (CHttpFile*) session.OpenURL(szURL, 1, 
			      INTERNET_FLAG_TRANSFER_ASCII | 
				  INTERNET_FLAG_IGNORE_CERT_DATE_INVALID | INTERNET_FLAG_IGNORE_CERT_CN_INVALID ); 
		if (pFile) {
			DWORD dwRet;
			pFile->QueryInfoStatusCode(dwRet);	
			TRACE("Response status: %d\n", dwRet);
			// save any error page to file in Andes root 
			if (! (dwRet == HTTP_STATUS_OK || dwRet == HTTP_STATUS_NO_CONTENT)) {
				CString strPath = g_strAndesDir + "/" + szCallId + "-result.html";
				CFile out(strPath, CFile::modeCreate|CFile::modeWrite);
				char szBuff[1024];
				UINT nRead ;
				while ((nRead = pFile->Read(szBuff, 1024)) > 0) 
					out.Write(szBuff, nRead);
				result = 0;
			}
		}
		delete pFile;
		// ::Sleep(1000);  // simulate delay
	}
	catch (CInternetException* pEx)		//catch errors from WinInet
	{ 
		TCHAR   szCause[255];
        CString strMsg;

        pEx->GetErrorMessage(szCause, 255);
		TRACE("HTTP request failed, error: %s\n", szCause);
		strMsg.Format("%s HTTP request failed: %s\n", szCallId, szCause);
		AfxMessageBox(strMsg);
		result = 0;
	}
	session.Close();
	return result;
}

// 
// Call one of the APIs to download a file without special encoding 
// stores at szPathName
// 1 => got it OK
// 0 => doesn't exist
// -1 => error occurred
// Note: URLS that call servlet functions will return a 0-length document if not found
// But requests to server to download problem files by their URL don't go through servlet
// functions and will just return 404 if not found.
// 
int HttpGetFile(LPCTSTR szURL, LPCTSTR szPathName, LPCTSTR szCallId)
{
	int result;
	CInternetSession session("ANDES Session");
	CHttpFile* pFile = NULL;
	try
	{
		TRACE("Trying %s\n", szURL);
		pFile = (CHttpFile*) session.OpenURL(szURL, 1, INTERNET_FLAG_TRANSFER_ASCII
			                  |INTERNET_FLAG_IGNORE_CERT_DATE_INVALID | INTERNET_FLAG_IGNORE_CERT_CN_INVALID ); 
		if (pFile) {
			DWORD dwRet;
			pFile->QueryInfoStatusCode(dwRet);
			DWORD dwContentLength  = -1;		// init to non-zero value
			DWORD dwBufLen = sizeof(dwContentLength);
			if (! pFile->QueryInfo(HTTP_QUERY_CONTENT_LENGTH|HTTP_QUERY_FLAG_NUMBER, 
					                  (void*) &dwContentLength, &dwBufLen)) {
					TRACE("Failed to get content length, errno = %d\n", GetLastError());
			}
			TRACE("Response status: %d  Content-Length: %d\n", dwRet, dwContentLength);
			if (dwRet == HTTP_STATUS_OK && dwContentLength == 0) {
				result = 0;
			} 
			else if (dwRet == HTTP_STATUS_OK) // length non-zero or unknown: copy file 
			{ 
				//CFile dbg(CString(szPathName) + ".down.txt", CFile::modeCreate|CFile::modeWrite);
				CFile file(szPathName, CFile::modeCreate|CFile::modeWrite);
				char szBuff[BUFSIZ];
				UINT nRead; BOOL gotSomething = FALSE;
				while ((nRead = pFile->Read(szBuff, BUFSIZ)) > 0) {
					gotSomething = 1;
					file.Write(szBuff, nRead);
					//dbg.Write(szBuff, nRead);
				}
				result = gotSomething? 1 : 0;
			}
			else if (dwRet == HTTP_STATUS_NOT_FOUND) {	// 404
				result = 0;
			}
			else // save any error page to file in Andes root for debugging
			{
				CString strPath = g_strAndesDir + "/" + szCallId + "-result.html";
				CFile out(strPath, CFile::modeCreate  |CFile::modeWrite);
				char szBuff[BUFSIZ];
				UINT nRead ;
				while ((nRead = pFile->Read(szBuff, BUFSIZ)) > 0) 
					out.Write(szBuff, nRead);
				result = -1;
			}
			// ::Sleep(1000);
		}
		delete pFile;
	}
	catch (CInternetException* pEx)		//catch errors from WinInet
	{ 
		TCHAR   szCause[255];
        CString strMsg;

        pEx->GetErrorMessage(szCause, 255);
		TRACE("HTTP request failed, error: %s\n", szCause);
		strMsg.Format("%s HTTP request failed: %s\n", szCallId, szCause);
		AfxMessageBox(strMsg);
		result = -1;
	}
	session.Close();
	return result;
}

// Helpers for URL encoding text files:

inline BYTE NibToHex(const BYTE &nib) // 4-bit "nibble" -> hex digit ascii code
{
	return nib > 9 ? nib + 55: nib + 48;
}

// URL Encodes string returning result
CString URLEncode(CString& sIn)
{
    CString sOut;
    const int nLen = sIn.GetLength() + 1;

    register LPBYTE pOutTmp = NULL;
    LPBYTE pOutBuf = NULL;
    register LPBYTE pInTmp = NULL;
    LPBYTE pInBuf =(LPBYTE)sIn.GetBuffer(nLen);
    BYTE b = 0;
	
    // alloc result buffer
    pOutBuf = (LPBYTE)sOut.GetBuffer(nLen*3); // max possible expansion
    if (pOutBuf)
    {
        pInTmp	= pInBuf;
		pOutTmp = pOutBuf;		
		// do encoding
		while (*pInTmp) {
			if(isalnum(*pInTmp))
				*pOutTmp++ = *pInTmp;
			else if(*pInTmp == ' ')
					*pOutTmp++ = '+';
			else {
				*pOutTmp++ = '%';
				*pOutTmp++ = NibToHex(*pInTmp>>4);
				*pOutTmp++ = NibToHex(*pInTmp%16);
			}
			pInTmp++;
		}
		*pOutTmp = '\0';		// make null terminated
		sOut.ReleaseBuffer();	// more efficient to tell length?
    }
    sIn.ReleaseBuffer();
    return sOut;
}

// get server part out of a URL, leaving object part in strObject
CString getServer(LPCSTR pszURL, CString& strObject)
{
	DWORD dwServiceType;
	CString strServer;
	INTERNET_PORT nPort;
	AfxParseURL(pszURL, dwServiceType, strServer, strObject, nPort );
	return strServer;
}

// build POST and append file text file contents, URL encoded. 
// returns 1 for success, 0 for failure
int HttpPostTextFile(LPCSTR pszURL, CString strStart, LPCSTR szPathName, LPCSTR szCallId)
{
	int result;
	TCHAR   szCause[255];
	CString strMsg;
	CString strObject;
	CString strServer = getServer(pszURL, strObject);
	CInternetSession session("ANDES Session");
	try
	{
		TRACE("Trying %s\n", pszURL);
		CString strHeaders = _T("Content-Type: application/x-www-form-urlencoded");
		// need to append file contents to strStart, URL Encoded
		// do it line by line -- try to make more efficient later if it matters
		CStdioFile file(szPathName, CFile::modeRead | CFile::typeText);
		int cbSize = file.GetLength();
		
		// URL-encoded form variables: to avoid lots of reallocations in result string
		// we pre-allocate big result string buffer, estimating a 10% inflation
		// due to escaping (experiment to find realistic factor).
		CString strFormData;
		strFormData.GetBufferSetLength(strStart.GetLength() + int(1.1*cbSize));
		strFormData.ReleaseBuffer(0);
		// init POST data with supplied prefix parameters
		strFormData += strStart;
		// append URL-encoded file contents. For now, do this line by line. Could be more
		// efficient to encode directly into result buf, avoiding copying every character.
		// Note text mode file I/O returns single newline for CRLF, and CString version of
		// CStdio::ReadString strips the trailing newline -- maybe clearer to use binary I/O.
		CString strLine;	// text line without trailing newline
		while (file.ReadString(strLine)) {
			strFormData += URLEncode(strLine);
			strFormData += "%0D%0A";
		}
/*
		// Debugging  -- save post contents to file
		{CStdioFile postFile(g_strAndesDir + szCallId + "-PostData.txt", CFile::modeCreate  |CFile::modeWrite);
			postFile.WriteString(strServer + "\n");
			postFile.WriteString(strObject + "\n");
			postFile.WriteString(strHeaders + "\n\n");
			postFile.WriteString(strFormData); 
		} */

		// OK, now do the request. First have to open a connection. Set Secure flag. 
		CHttpConnection* pConnection = session.GetHttpConnection(strServer, 
			                                           INTERNET_FLAG_SECURE,
													   INTERNET_DEFAULT_HTTPS_PORT);
		// Now create the request. Curiously, also need SECURE flag on this.
		CHttpFile* pFile =  pConnection->OpenRequest(
			                   CHttpConnection::HTTP_VERB_POST, strObject,
                              /*referrer:*/ NULL, /*context:*/ 1, /*accept type:*/ NULL, 
							  /*version*/ NULL, /*flags:*/
							  INTERNET_FLAG_SECURE | INTERNET_FLAG_EXISTING_CONNECT
							  |INTERNET_FLAG_IGNORE_CERT_DATE_INVALID | INTERNET_FLAG_IGNORE_CERT_CN_INVALID ); 
		BOOL result = pFile->SendRequest(strHeaders,
						(LPVOID)(LPCTSTR)strFormData, strFormData.GetLength());
		// TRACE("SendRequest success = %d\n", result);
		// look for error page on failure. Do we need to query status code, or does
		// result have it?
		DWORD dwRet;
		pFile->QueryInfoStatusCode(dwRet);
		TRACE("Response Status: %d\n", dwRet);
		if (dwRet != HTTP_STATUS_OK)// save any error page to file in Andes root for debugging
		{
			CString strPath = g_strAndesDir + "/" + szCallId + "-result.html";
			CFile out(strPath, CFile::modeCreate  |CFile::modeWrite);
			char szBuff[BUFSIZ];
			UINT nRead ;
			while ((nRead = pFile->Read(szBuff, BUFSIZ)) > 0) 
				out.Write(szBuff, nRead);
			result = 0;
		} else { 
			result = 1;
		}
		// ::Sleep(1000);
	}
	catch (CFileException* pEx)
	{
        pEx->GetErrorMessage(szCause, 255);
		TRACE("File operation error: %s\n", szCause);
		strMsg.Format("%s File operation error: %s\n", szCallId, szCause);
		AfxMessageBox(strMsg);
		result = 0;
	}
	catch (CInternetException* pEx)		//catch errors from WinInet
	{ 
        pEx->GetErrorMessage(szCause, 255);
		TRACE("HTTP request failed, error: %s\n", szCause);
		strMsg.Format("%s HTTP request failed: %s\n", szCallId, szCause);
		AfxMessageBox(strMsg);
		result = 0;
	}
	session.Close();
	return result;
}

// binary file upload/download

// build POST and append file contents, ascifying using variant Base64 encoding
// Variant uses other chars than +, /, and = to avoid problems cracking URL=encoded post data
int HttpPostBinaryFile(LPCSTR pszURL, LPCTSTR strStart, LPCSTR szPathName, LPCSTR szCallId)
{
	int result;
	TCHAR   szCause[255];
	CString strMsg;
	CString strObject;
	CString strServer = getServer(pszURL, strObject);
	CInternetSession session("ANDES Session");
	try
	{
		TRACE("Trying %s\n", pszURL);
		CString strHeaders = _T("Content-Type: application/x-www-form-urlencoded");
		// need to append file contents to strStart, URL Encoded
		// do it line by line -- try to make more efficient later if it matters
		CFile file(szPathName, CFile::modeRead);
		unsigned int cbSize = file.GetLength();
		
		// URL-encoded form variables, initialized with prefix parameters
		CString strFormData = strStart;
		// Gulp file contents into big buffer:
		unsigned char* pFileContents = new unsigned char[cbSize];
		if (file.Read(pFileContents, cbSize) != cbSize) {
			TRACE("HttpPostBinaryFile: read error!\n");
			return 0;
		}
		// Get contents encoded into our variant Base64 encoding.
		// !!! Efficiency note: the coder copies input into an input buffer. Then we must copy
		// its output into our strFormData as well. Could hack coder to encode directly from 
		// file buffer, and could also put its output directly into our result buffer too.
		Base64Coder coder;
		coder.Encode(pFileContents, cbSize);
		delete pFileContents;
	/*  { CFile dbg(CString(szPathName) + ".up.txt", CFile::modeWrite|CFile::modeCreate);
		  dbg.Write(coder.EncodedMessage(), strlen(coder.EncodedMessage())); } */
		strFormData += coder.EncodedMessage();	// should hold nul-terminated string

		
	/*	 // Debugging  -- save post contents to file
		{CStdioFile postFile(g_strAndesDir + szCallId + "-s.txt", CFile::modeCreate  |CFile::modeWrite);
			postFile.WriteString(strServer + "\n");
			postFile.WriteString(strObject + "\n");
			postFile.WriteString(strHeaders + "\n\n");
			postFile.WriteString(strFormData); 
		} */

		// OK, now do the request. First have to open a connection. Set Secure flag. 
		CHttpConnection* pConnection = session.GetHttpConnection(strServer, 
			                                           INTERNET_FLAG_SECURE,
													   INTERNET_DEFAULT_HTTPS_PORT);
		CHttpFile* pFile =  pConnection->OpenRequest(
			                   CHttpConnection::HTTP_VERB_POST, strObject,
                              /*referrer:*/ NULL, /*context:*/ 1, /*accept type:*/ NULL, 
							  /*version*/ NULL, /*flags:*/
							  INTERNET_FLAG_SECURE | INTERNET_FLAG_EXISTING_CONNECT
							  |INTERNET_FLAG_IGNORE_CERT_DATE_INVALID | INTERNET_FLAG_IGNORE_CERT_CN_INVALID  ); 
		BOOL result = pFile->SendRequest(strHeaders,
						(LPVOID)(LPCTSTR)strFormData, strFormData.GetLength());
		// look for error page on failure. Result just indicates request was sent.
		DWORD dwRet;
		pFile->QueryInfoStatusCode(dwRet);
		TRACE("Response Status: %d\n", dwRet);
		if (dwRet != HTTP_STATUS_OK)// save any error page to file in Andes root for debugging
		{
			CString strPath = g_strAndesDir + "/" + szCallId + "-result.html";
			CFile out(strPath, CFile::modeCreate  |CFile::modeWrite);
			char szBuff[BUFSIZ]; UINT nRead ;
			while ((nRead = pFile->Read(szBuff, BUFSIZ)) > 0) 
				out.Write(szBuff, nRead);
			result = 0;
		} else  result = 1;		// success! 
		// ::Sleep(1000);
	}
	catch (CFileException* pEx)
	{
        pEx->GetErrorMessage(szCause, 255);
		TRACE("File operation error: %s\n", szCause);
		strMsg.Format("%s File operation error: %s\n", szCallId, szCause);
		AfxMessageBox(strMsg);
		result = 0;
	}
	catch (CInternetException* pEx)		//catch errors from WinInet
	{ 
        pEx->GetErrorMessage(szCause, 255);
		TRACE("HTTP request failed, error: %s\n", szCause);
		strMsg.Format("%s HTTP request failed: %s\n", szCallId, szCause);
		AfxMessageBox(strMsg);
		result = 0;
	}
	session.Close();
	return result;
}

// Retrieve binary file posted in Base64-coded form, and decode
int HttpGetFileBinary(LPCTSTR szURL, LPCTSTR szPathName, LPCTSTR szCallId)
{
	int result;
	CInternetSession session("ANDES Session");
	CHttpFile* pFile = NULL;
	try
	{
		TRACE("Trying %s\n", szURL);
		pFile = (CHttpFile*) session.OpenURL(szURL, 1, INTERNET_FLAG_TRANSFER_ASCII
			                     |INTERNET_FLAG_IGNORE_CERT_DATE_INVALID | INTERNET_FLAG_IGNORE_CERT_CN_INVALID ); 
		if (pFile) {
			DWORD dwRet;
			pFile->QueryInfoStatusCode(dwRet);
			DWORD dwContentLength = -1;		// init to non-zero value
			DWORD dwBufLen = sizeof(dwContentLength);
			if (! pFile->QueryInfo(HTTP_QUERY_CONTENT_LENGTH|HTTP_QUERY_FLAG_NUMBER, 
					                  (void*) &dwContentLength, &dwBufLen)) {
					TRACE("Failed to get content length, errno = %d\n", GetLastError());
			}
			TRACE("Response status: %d  Content-Length: %d\n", dwRet, dwContentLength);
			if (dwRet == HTTP_STATUS_OK && dwContentLength == 0) 
				result = 0;
			else if (dwRet == HTTP_STATUS_OK) // non-zero length: decode file and save
			{ 
				// read stream contents, assembling into CString. 
				CString strContents;
				if (dwContentLength != -1) { // pre-allocate full buffer size we need, if known
					strContents.GetBufferSetLength(dwContentLength);
					strContents.ReleaseBuffer();			
				}
				// CFile dbg(CString(szPathName) + ".down.txt", CFile::modeCreate|CFile::modeWrite);
				// char szBuff[BUFSIZ];
				CString strRead;
				char * pBuff = strRead.GetBufferSetLength(BUFSIZ);
				UINT nRead = 0; BOOL gotSomething = FALSE;
				while ((nRead = pFile->Read(pBuff, BUFSIZ)) > 0) {
					gotSomething = TRUE;
					// dbg.Write(pBuff, nRead); // for debugging
					strRead.ReleaseBuffer(nRead);
					strContents += strRead;
					TRACE("Read %d chars, result length now=%d\n", nRead, strContents.GetLength());
					pBuff = strRead.GetBuffer(BUFSIZ);
				}
				strRead.ReleaseBuffer();
				TRACE("Returned file length (encoded) = %d\n", strContents.GetLength());
			
				// decode the whole file contents
				Base64Coder coder;
				coder.Decode(strContents);
				
				// Write decoded contents out to solution file
				// NB: binary data can contain NULs, so can't use strlen etc on this string
				CFile file(szPathName, CFile::modeCreate|CFile::modeWrite);
				file.Write(coder.DecodedMessage(), coder.DecodedLength());
				result = gotSomething? 1 : 0;
			}
			else // save any error page to file in Andes root for debugging
			{
				CString strPath = g_strAndesDir + "/" + szCallId + "-result.html";
				CFile out(strPath, CFile::modeCreate  |CFile::modeWrite);
				char szBuff[BUFSIZ];
				UINT nRead ;
				while ((nRead = pFile->Read(szBuff, BUFSIZ)) > 0) 
					out.Write(szBuff, nRead);
				result = -1;
			}

		}
		delete pFile;
		// ::Sleep(1000);
	}
	catch (CInternetException* pEx)		//catch errors from WinInet
	{ 
		TCHAR   szCause[255];
        CString strMsg;

        pEx->GetErrorMessage(szCause, 255);
		TRACE("HTTP request failed, error: %s\n", szCause);
		strMsg.Format("%s HTTP request failed: %s\n", szCallId, szCause);
		AfxMessageBox(strMsg);
		result = -1;
	}
	session.Close();
	return result;
}

//-------------------------------------------------------
// ANDES-specific OLI transactions in CProblemSet methods:
//-------------------------------------------------------

int CProblemSet::GetHistory()			// Get the history file for current student
{
	SetStatusMsg("Downloading student history file...");
	CString strBaseUrl, strActivity, strToken, strCmd;
	if (! ( m_opts.Lookup("baseurl", strBaseUrl) && 
		    m_opts.Lookup("authtoken", strToken)))
	        {
		TRACE("Couldn't find OLI arameters to get history!\n");
		return -1;
	}
	// token may contain ampersands and equals signs
	strToken.Replace("&", "%26");
	strToken.Replace("=", "%3D");

    strCmd = strBaseUrl + "/getHistory" +
		             "?user=" + theApp.m_strUserName + 
					 "&token=" + strToken;
	CString strPathName = g_strAndesDir + "Log/" + theApp.m_strUserName + ".dat";
	int result = HttpGetFile(strCmd, strPathName, "getHistory");

	SetStatusMsg("");
	return result;
}

// download all components of the problem
int CProblemSet::GetProblemFiles(CString strProblemId)
{
	SetStatusMsg("Downloading problem files...");
	CString strProblemUrl, strUrl;
	if (! (m_opts.Lookup("problemurl", strProblemUrl))) return -1;

	// download the .prb file in all cases. Fetch is case-sensitive,
	// relies on fact that problemid we get is all upper-case and 
	// prb name is all upper-case as well.
	CString strBaseName = strProblemId + ".prb";
	CString strProblemDir = g_strAndesDir + "Problems/";
	if (HttpGetFile(strProblemUrl + strBaseName, 
		            strProblemDir + strBaseName, "getPrb") <= 0) {
		SetStatusMsg("Failed to download .prb file!");
		return -1;
    }
	// see if a solution .fbd or a master .fbd exists. solution filenames will
	// be upper case, while by convention .fbd filenames are lower-case. 
	strBaseName = strProblemId + ".fbd";	// upper-case

	// set flag to create solution dir in case student never worked locally before
	CTask* pTask = FindTask(strProblemId);
	CString strSolnPath = pTask->GetSolutionPath(TRUE);	
	if (GetSolution(strSolnPath) == 0) 	// = 0 => file doesn't exist
	{
		if (m_bViewSolution)	// odd, should be a solution file there.
			// should cancel open. For now, just warn and continue
			AfxMessageBox("No student solution file found on OLI! Creating fresh solution file.");	

#if 0 // no longer using fbd files        
		// no solution file to open: look for .fbd file
		CString strProblemIdLC = strProblemId;	// lower-case
		strProblemIdLC.MakeLower();
		strBaseName = strProblemIdLC + ".fbd";
 
		if (HttpGetFile(strProblemUrl + strBaseName, 
		            strProblemDir + strBaseName, "getFbd") == 0) 
		{
			// no solution and no fbd => new style problem. See if graphic file exists
			// for now, assume graphic has same base name as problem (not true where
			// multiple problems share same graphic, but could be made true.)
	     /* for now, do this when importing graphic
			strBaseName = strProblemIdLC + ".gif";
			(void) HttpGetFile(strProblemUrl + strBaseName, 
				               strProblemDir + strBaseName, "getGif");
			*/
		}
#else // new
       // delete any saved solution to sync local state with OLI.
	   // [Risky! May lose work if failed to upload last time. Maybe backup?]
		::DeleteFile(strSolnPath);
		
		// note any fbd file in Problems should have been deleted by new installer
#endif // new code
	}
	SetStatusMsg("");
	return 0;
}

int CProblemSet::GetProblemGraphic(CString strFileName)
{
	CString strProblemUrl;
	if (! (m_opts.Lookup("problemurl", strProblemUrl))) return -1;
	strFileName.MakeLower();	// gif filenames all lower-case
	CString strProblemDir = g_strAndesDir + "Problems/";
	return HttpGetFile(strProblemUrl + strFileName, 
				       strProblemDir + strFileName, "getGif");
}

int CProblemSet::PutHistory(CString strPathName)
{
	SetStatusMsg("Uploading student history file...");
	CString strBaseUrl, strUserId, strActivity, strToken, strCmd;
	if (! ( m_opts.Lookup("baseurl", strBaseUrl) && 
		    m_opts.Lookup("authtoken", strToken) &&
			m_opts.Lookup("activity", strActivity) ))
	        {
		TRACE("Couldn't find OLI parameters to get history!\n");
		return -1;
	}
	// token may contain ampersands and equals signs
	strToken.Replace("&", "%26");
	strToken.Replace("=", "%3D");

    strCmd = strBaseUrl + "/putHistory"; 
	CString strStart ="user=" + theApp.m_strUserName + 
					 "&token=" + strToken +
					 "&actGuid=" + strActivity +
					 "&history=";
	int result = HttpPostTextFile(strCmd, strStart, strPathName, "putHistory");
	SetStatusMsg("");
	return result;
}

// returns 1 for success, 0 for failure
int CProblemSet::PutLog(CString strPathName)
{
	SetStatusMsg("Uploading session log...");
	CString strBaseUrl, strActivity, strToken, strSession, strCmd;
	if (! ( m_opts.Lookup("baseurl", strBaseUrl) && 
		    m_opts.Lookup("authtoken", strToken) &&
			m_opts.Lookup("jsessionid", strSession) ))
	        {
		TRACE("Couldn't find OLI parameters to put log!\n");
		return 0;
	}
	// token may contain ampersands and equals signs
	strToken.Replace("&", "%26");
	strToken.Replace("=", "%3D");

    strCmd = strBaseUrl + "/storeLog"; 
	CString strStart ="user=" + theApp.m_strUserName + 
					 "&token=" + strToken +
					 "&session=" + strSession +
					 "&log=";
	int result = HttpPostTextFile(strCmd, strStart, strPathName, "storeLog");
	SetStatusMsg("");
	return result;
}

int CProblemSet::GetSolution(CString strPathName)
{
	CString strBaseUrl, strActivity, strToken, strCmd;
	if (! ( m_opts.Lookup("baseurl", strBaseUrl) && 
		    m_opts.Lookup("authtoken", strToken) &&
			m_opts.Lookup("activity", strActivity)))
	        {
		TRACE("Couldn't find OLI parameters to get history!\n");
		return -1;
	}
	// token may contain ampersands and equals signs
	strToken.Replace("&", "%26");
	strToken.Replace("=", "%3D");
	
    strCmd = strBaseUrl + "/getWork" +
		             "?user=" + theApp.m_strUserName + 
					 "&token=" + strToken +
					 "&actGuid=" + strActivity;
	return HttpGetFileBinary(strCmd, strPathName, "getWork");
}

int CProblemSet::PutSolution(CString strPathName)
{
	SetStatusMsg("Uploading solution file ...");
	CString strBaseUrl, strActivity, strToken, strCmd;
	if (! ( m_opts.Lookup("baseurl", strBaseUrl) && 
		    m_opts.Lookup("authtoken", strToken) &&
			m_opts.Lookup("activity", strActivity)))
	        {
		TRACE("Couldn't find OLI arameters to get history!\n");
		return -1;
	}
	// token may contain ampersands and equals signs
	strToken.Replace("&", "%26");
	strToken.Replace("=", "%3D");

    strCmd = strBaseUrl + "/putWork"; 
	CString strStart ="user=" + theApp.m_strUserName + 
					 "&token=" + strToken +
					 "&actGuid=" + strActivity +
					 "&solution=";
	int result = HttpPostBinaryFile(strCmd, strStart, strPathName, "putWork");
	SetStatusMsg("");
	return result;
}

int CProblemSet::SetScore(CString strScore, WorkState workState)
{
	// empty score string means unset (maybe helpsys failed to run).
	// Don't record numerical score in this case. Might still record
	// status, e.g. partial, if there are some entries, to show some work
	if (strScore.IsEmpty())
		return TRUE;	// success, because no score to upload

	int result;
	SetStatusMsg("Recording problem score...");
	CString strBaseUrl, strActivity, strToken, strCmd;
	if (! ( m_opts.Lookup("baseurl", strBaseUrl) && 
		    m_opts.Lookup("authtoken", strToken) &&
	        m_opts.Lookup("activity", strActivity)
			)) {
		TRACE("Couldn't find OLI parameters to set score!\n");
		return 0;
	}
	// token may contain ampersands and equals signs
	strToken.Replace("&", "%26");
	strToken.Replace("=", "%3D");

	CString strCmdBase = strBaseUrl + "/score" +
		             "?user=" + theApp.m_strUserName + 
					 "&token=" + strToken + 
		             "&activity=" + strActivity;
	// indicate non-completion by showing scores in parentheses
	// Note: for now this means we must use the "status" score,
	// since that accepts strings ("score" score is typed numeric)
	CString strScoreArg = strScore;
	if (workState != workCompleted) 
		strScoreArg = "(" + strScore + ")";

	CString strCmdScore = strCmdBase +  "&scoreId=status" + 
		                        "&scoreValue=" + strScoreArg;
	
	result = HttpCallURL(strCmdScore, "setScore");
	SetStatusMsg("");
	return result;
}

//
// Update on close of problem. Should be fired from problem's OnCloseDocument
// before problem goes away. We resynch solution state display with file system
// in case new solution was saved.
// 
// Note this means if student does not save, the problem does not show up
// as done even if it was completed. Might be desirable to show a non-persistent
// per-session state for this case, so student can work through a set of 
// problems in one session without saving anything, and see what they've done.
//
void CProblemSet::PreCloseProblem(CFBDDoc* pDoc)
{
	UpdateSolutionState();

	// If not running under OLI, nothing to do here
	if (! m_bOli) return;

	// If only instructor viewing solution, nothing to do here
	if (m_bViewSolution) return;
#if 0 // move to post	
	// Else do Oli post problem stuff:
	// set the score variables
	SetScore(pDoc->m_strScore, pDoc->m_workState);

	// Upload solution, if it exists: 
	// OLI version now saves solution always on close, so its OK.
	CTask* pTask = FindTask(pDoc->m_strProblemId);
	CString strSolnPath = pTask->GetSolutionPath();
	CFileStatus statSoln;
	if (CFile::GetStatus(strSolnPath, statSoln)) {
		PutSolution(strSolnPath);
	}
#endif 0
}


// Following has to happen *after* DDE call to help system to close problem,
// so WM_CLOSE message isn't pumped inside the DDE wait state, and also so
// it occurs AFTER student history file is written out by helpsys.
void CProblemSet::PostCloseProblem(CFBDDoc* pDoc)
{
	// If not running under OLI, nothing to do here
	if (! m_bOli) return;

	// Else finished an OLI problem set

	// if only viewing solutions, don't upload info at end
	if (! m_bViewSolution)
	{
		// Else do Oli post problem stuff:
		AfxGetApp()->BeginWaitCursor();
		
		// Upload the score variables
		SetScore(pDoc->m_strScore, pDoc->m_workState);

		// Upload solution, if it exists: 
		// OLI version workbench now saves solution always on close, so its OK.
		CTask* pTask = FindTask(pDoc->m_strProblemId);
		CString strSolnPath = pTask->GetSolutionPath();
		CFileStatus statSoln;
		if (CFile::GetStatus(strSolnPath, statSoln)) {
			PutSolution(strSolnPath);
		}
/* No longer do this since it was unreliable for undiagnosed reasons
		// Upload the student history file. Must do it after helpsys writes
		// out most recent grades on close of problem
		PutHistory(g_strAndesDir + "Log/" + theApp.m_strUserName + ".dat");
*/
		// Finish log file and upload it. 
		HistoryFileEnd();
		PutLog(HistoryFileGetPath());

		AfxGetApp()->EndWaitCursor();
	}
	else // just viewed a saved solution
	{
		CString strSolnPath = FindTask(pDoc->m_strProblemId)->GetSolutionPath();
		// following gives instructor option to update from this session 
		// The delete option lets us remove a bad solution file.
promptagain:
		int nChoice =  AfxMessageBox("Update student's solution file and score from this session?\n[Select Cancel to delete saved solution file]", 
			                          MB_YESNOCANCEL|MB_DEFBUTTON2);

		if (nChoice == IDYES)		// update solution and score
		{
			AfxGetApp()->BeginWaitCursor();
			PutSolution(strSolnPath);
			SetScore(pDoc->m_strScore, pDoc->m_workState);
			AfxGetApp()->EndWaitCursor();			
		}
		else if (nChoice == IDCANCEL)  
		{
			// confirm destructive action
			if (AfxMessageBox("Really delete student's solution from gradebook?\nThis will reset score to (0).", 
							  MB_YESNO|MB_DEFBUTTON2) != IDYES)
				goto promptagain;

			// truncate solution file to zero length before calling upload routine
			CFile(strSolnPath, CFile::modeWrite|CFile::modeCreate);
			
			AfxGetApp()->BeginWaitCursor();
			PutSolution(strSolnPath);
			SetScore("0", workNone);	// reset score to incomplete 0
			AfxGetApp()->EndWaitCursor();
			
			CFile::Remove(strSolnPath);	// delete truncated local copy
		} 
	}

	// Post message to close the whole application if not already closing. 
	if (!theApp.GetMainFrame()->m_bClosing)
		::PostMessage(theApp.GetMainFrame()->m_hWnd, WM_CLOSE, 0, 0);

}


// Handle close of problem set: must make sure any open problems from it are
// saved. For now, should be enforced by file command logic that can't close 
// ProbSet with any open problems.
void CProblemSet::OnCloseDocument() 
{
	TRACE("Closing problem set\n");
	ASSERT(theApp.GetDocument() == NULL); 
		
	CDocument::OnCloseDocument();

	// Copied from CFBDDoc::OnCloseDocument:
	// Done close of problem set, have to decide what app should do next:
	// For students, we popup the task selection dialog again.
	// But don't do if the app is in process of shutting down. 
	// Note: in OLI mode, app would normally be shutting down after problem close. But we
	// also test and do nothing in OLI case just in case (e.g. if prob never opened)
	if (! (m_bOli || theApp.m_bAuthorMode || ((CMainFrame*)AfxGetMainWnd())->m_bClosing)) 
	{
		theApp.DoTaskSelect();
	}
}

CTask* CProblemSet::GetFirstIncompleteTask()
{
	POSITION pos = m_tasks.GetHeadPosition();
	while (pos != NULL) {
		CTask* pTask = m_tasks.GetNext(pos);
		if (pTask->m_work != workCompleted)
			return pTask;
	}
	return NULL;
}


// Video watching tasks identified by "wmv" extension (w/o period!)
BOOLEAN CTask::IsVideo()
{
	return m_strExt.CompareNoCase("wmv") == 0;
}

// Set the status message for this problem set
void CProblemSet::SetStatusMsg(LPCTSTR pszText)
{
	// put it on app's status bar.
	theApp.GetMainFrame()->SetAngleText(pszText);

	// also forward it to the view
	UpdateAllViews(NULL, HINT_UPDATE_STATUS_MSG, (CObject*) pszText);
}
