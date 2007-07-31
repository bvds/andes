//////////////////////////////////////////////////////////////////////////
//
// History recording/playback functions
//
// Manages writing to a logfile to record application events. 
// Provides a playback function to replay the events to the application.
//
//////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "FBD.h"
#include "history.h"
#include "LgDialog.h"
#include "LogEdit.h"
#include "FBDDoc.h"
#include "FBDObj.h"			// needed only for OBJ_NAME macro; 
#include "DrawObjDlg.h"
#include "EQView.h"
#include "FBDView.h"
#include "EXView.h"
#include "PlanView.h"
#include "VarView.h"
#include "HiLevelVw.h"
#include "TabView.h"
#include "HintView.h"
#include "ChatView.h"
#include "PrincView.h"
#include "PlayDlg.h"
#include "MainFrm.h"
#include "LoginDlg.h"
#include "CommentDlg.h"
#include "helpifc.h"
#include <stdarg.h>			// for variable arglist macros
#include <io.h>				// for _filelength

// 
// Because there is currently only one global history record and playback 
// module for the app, this module is implemented in C rather than 
// C++ style. We use the g_* prefix to mark "global" vars, which are 
// actually statics not visible outside this file. Following syntactic
// sugar macros highlight what is internal and what is externally visible. 
// !!! Still would be better stylistically to encapsulate in C++ classes
//
#define PUBLIC extern
#define PRIVATE static

// 
// Log file format:
//
// Currently we use a very simple ascii text format.
// After a fixed header, the file contain a sequence of lines of the form:
//
// time<TAB>EventType args... 
//
// with time in seconds from startup and args varying with EventType
//
// EventTypes are human readable names. An enumerated type EventID
// defined in history.h enumerates codes for all the EventTypes;
// these are currently used for dispatching events on playback. 
// To add a new EventType you must add an EventId to the list in the header and 
// an entry in the g_evmap table in this module to specify the name and handler.
//
// Object parameters are generally identified in the log by their help system Ids. 
// (OBJ_ID is now obsolete) It was originally envisaged that we would build a symbol table 
// mapping object ids used in logs to workbench objects created on playback; however, we 
// currently have no need for that. 
//
// Note we are assuming that the ids generated for created objects (e.g. drawn objects) 
// will be the same on playback as during the original run. This is normally true if the
// problem files are the same; however, since the problem files contain id generation
// counters, it is possible that the problem file used on playback could have been 
// changed by adding and deleting objects in which its id generation counters
// would have been incremented even though it contains the same objects.
//

///////////////////////////////////////////////////////////////////////
//
// History recording utilities:
//
///////////////////////////////////////////////////////////////////////

PRIVATE FILE * g_pfHist = NULL;		// Log file pointer: NULL if not logging
PRIVATE BOOL g_bKeptHist = FALSE;	// true if we kept a history log during session
PRIVATE CString strLogPath;			// Full pathname of log file we opened. Composed of:
PRIVATE CString strDirPath;			//	 Log directory path prefix (includes backslash) + 
PRIVATE CString strFileName;		//	 Log filename (includes extension) 
PRIVATE BOOL g_bAudioLog = FALSE;	// True if recorded audio log file as well
PRIVATE BOOL g_bEventOnly = FALSE;  // Send log msgs down event socket only

//
// For maintaining clock with elapsed session time:
// The CTime class encapsulates a C library time_t value, which specifies current
// date and time in seconds since 1970. We would have to use a different type
// if we needed to measure elapsed time with higher granularity, say by using system 
// clock or Windows system time. but for now seconds seems sufficient.The date is only 
// used to name the log file and in the log header. 
//
PRIVATE CTime g_timeStart;				// real time session clock started
PRIVATE BOOL g_bClockStarted = FALSE;	// g_timeStart only valid after this is set

PRIVATE void StartSessionClock()
{
	g_timeStart = CTime::GetCurrentTime();
	g_bClockStarted = TRUE;
}

// HistTimeSpan -- returns elapsed session time since start as a CTimeSpan
PUBLIC inline CTimeSpan HistTimeSpan()		
{
	if (!g_bClockStarted)				// clock not started yet, so treat
		return CTimeSpan(0, 0, 0, 0);	// as session time zero
	
	// else clock is running:
	return (CTime::GetCurrentTime() - g_timeStart);
}

// HistTime -- returns elapsed session time in seconds
PUBLIC inline int HistTime()				
{
	return HistTimeSpan().GetTotalSeconds();
}

// Identifying information included in logfile:
PRIVATE char szComputer[MAX_COMPUTERNAME_LENGTH + 1] = "Unknown";
PRIVATE char szWinUser[MAX_COMPUTERNAME_LENGTH + 1] = "User"; // windows user name, not ours

// Header prefixes and log format version number for boilerplate
PRIVATE const char szLogHdr[] = "# Log of Andes session begun";
PRIVATE const char szVersionHdr[] = "# Version";
PRIVATE const char szHelpHdr[] = "# Help-flags";
PRIVATE const int  nVersion = 1;
PRIVATE void WriteHeader();

// Log file extension:
PRIVATE const char szLogExt[] = ".log";

// Default fallback log directory hard-coded for testing:
PRIVATE const char szDefaultLogDir[] = "C:\\Program Files\\Andes\\Log";

// Use to report error during startup, i.e. when splash screen may be up and before
// main window is shown. Use instead of AfxMessageBox or theApp.DoWarningMsg,
// since that seems to parent the message box to the temporary modeless splash screen,
// (since it is the last active popup owned by the main wnd), causing assert from
// DoWarningMsg or causing msg to disappear when the splash screen is taken down.
// Safe to use at other times as well, though won't be logged.
PRIVATE int InitErrMsg(LPCTSTR pszMsg)
{
	return ::MessageBox(AfxGetMainWnd()->GetSafeHwnd(), pszMsg, "FBD", MB_ICONWARNING);
}

// For doing audio recording/playback via mci commands:
// helper to wrap our mci command string calls, printing trace output
// on any errors. Like mci, returns  FALSE for no error. TRUE for error
// Set bSilent to suppress message box on failure.
PUBLIC BOOL DoMciSendString(LPCTSTR pszCmd, BOOL bSilent /*=FALSE*/)
{
	DWORD dwError;
	char szReturn[255];
	dwError = mciSendString(pszCmd, szReturn, sizeof(szReturn), NULL);
	TRACE("MCI cmd \"%s\" => %d \"%s\" \n", pszCmd, dwError, szReturn);
	if (dwError == 0)
		return FALSE;	// no error 

	// else error
	char szError[255];
	if (! mciGetErrorString(dwError, szError, sizeof(szError)))
		strcpy(szError, "Unknown MCI Error");
	CString strMsg;
	strMsg.Format("MCI cmd failed: \"%s\"\n%s\n", pszCmd, szError);
	TRACE(strMsg);
	if (! bSilent)
		InitErrMsg(strMsg);	// may happen during startup	

	return TRUE;		// error
}
 
// Forward: Wrappers for WOZ-only event broadcasting functionality below
PRIVATE void InitEventServer();		
PRIVATE void NotifyEvent(LPCTSTR pszEvName, LPCTSTR pszArgs);
PRIVATE void EndEventServer();		

//
// HistoryFileBegin -- public function to start history recording
// Opens log file in log directory and writes header.
// returns full path to logfile
//
PUBLIC CString HistoryFileBegin(LPCTSTR pszFileTitle)		// Open and initialize log file
{
	// Start session clock now. Real time session started is used when
	// naming the file (in initial default) and also put in log header.
	// Want session time synched reasonably with time in audio recording file.
	StartSessionClock();

	// save some identifying info about the machine and windows user.
	// Note app needn't have Andes student name at this point.
	unsigned long nSize = MAX_COMPUTERNAME_LENGTH + 1;
	::GetComputerName(szComputer, &nSize);
	nSize = MAX_COMPUTERNAME_LENGTH + 1;	// must re-init, it's in-out above
	::GetUserName(szWinUser, &nSize);

	// Assemble a pathname to try to open for the log.

	// Directory: Logs normally go into InstallDirectory/Log. But can override this
	// with a special registry entry "Log Directory", mainly for developers to use. 
	CString strCustomDir = theApp.GetProfileString("Settings", "Log Directory", "");
	if (! strCustomDir.IsEmpty())
		strDirPath = strCustomDir;
	else
		strDirPath = g_strAndesDir + "Log" ;

	// Try to ensure Log directory exists.  
	::CreateDirectory(strDirPath, NULL); // ignore error, might exist.

	strDirPath += "\\";   // Add trailing backslash to make a path prefix

	// Construct log filename (includes extension) 
	if (pszFileTitle != NULL)		// file title to use was specifed in arg.
		strFileName = CString(pszFileTitle) + szLogExt;
	else	// generate a new log file name. 
	{
		// Form name from machine name and start time to try to ensure uniqueness.
		// Generates a long name, but w/no spaces for ease of transfer to Unix.
		CString strTimePart = g_timeStart.Format("%b%d-%H-%M-%S");
		strFileName.Format("%s-%s", szComputer, strTimePart);
		strFileName += szLogExt;
		// strFileName = g_timeStart.Format("%b%d-%H-%M-%S") + szLogExt;
	}

	// Concat dir prefix with filename to form full path
	strLogPath = strDirPath + strFileName;
	
	// OK, now try to open the history file. We go to some trouble to ensure we
	// create a log file *somewhere*, so we'll have something to look at in case of crash.
	if ((g_pfHist = fopen(strLogPath, "w")) == NULL) // failed to open in standard location
	{
		TRACE("Couldn't open %s. %s", strLogPath, strerror(NULL));
		// Time for plan B: try 2 alternate locations
		CString strAltPath;	// full path of alternate name we will try
		CString strMsg;		// notification message

		// First try opening in compiled-in standard location
		// This is mainly a hack for developers to use in case our file server is down. 
		// Note this fallback does not require any registry setting
		strDirPath = CString(szDefaultLogDir) + "\\";		// saved
		strAltPath = strDirPath + strFileName;
		if ((g_pfHist = fopen(strAltPath, "w")) == NULL)	// failed again at compiled-in default
		{
			// Try using a temp file path instead instead. Uses UNIX-style CRT func. Note the 
			// directory arg is only a default used just in case no TMP env var or def in library 
			// header. Also no extension can be specified without threatening name uniqueness!
			LPCTSTR pTemp = _tempnam(g_strAndesDir, "Andes");
			strAltPath = CString(pTemp);  
			free((void*)pTemp);		// _tmpnam does a malloc

			if (pTemp && (g_pfHist = fopen(strAltPath, "w")) == NULL) // failed again, quit
			{
				strMsg.Format("Couldn't create log file in  %s\n\
 Directory might not be accessible.\n\n\
 A log of this session will not be recorded", strLogPath);
				InitErrMsg(strMsg);
				return CString(""); // early exit, all attempts failed
			}
			// else succeeded but w/temp file. Better inform user of its location.
			strMsg.Format(
"Couldn't create log file in  %s\n\
 or in %s\n\
(Directories might not be accessible.)\n\n\
 The session log will be found in %s", strLogPath, szDefaultLogDir, strAltPath);
				InitErrMsg(strMsg);
		}
		// else succeeded with compiled-in fallback location, no notification necessary
		
		// if get here, have used an alternate file: save its name for later
		strLogPath = strAltPath;		
		// pull out directory prefix including backslash). [!!error checking?]
		strDirPath = strLogPath.Left(strLogPath.ReverseFind('\\') + 1); 
		// !!! strFileName not saved in this case. OK, all info in strLogPath 
	}

	// if get here, should have succeeded with some file or other
	ASSERT(g_pfHist != NULL);
	g_bKeptHist = TRUE;			// remember that a log was created, even after close

	// Write standard log file header boilerplate
	WriteHeader();

	// Optional: start recording audio input to save to a wav file in synch with log 
	if (theApp.GetProfileInt("Settings", "Audio Log", 0)) // get flag from registry
	{
		// abort any wave sound in progress, i.e. startup beep if beeps are on. 
		// This seems to interfere with opening the record device in some configs.
		PlaySound(NULL, NULL, NULL); 

		// Issue MCI commands to open wave device and start recording input
		if (! DoMciSendString("open new type waveaudio alias log"))
		{
			if (DoMciSendString("record log")){
				// failed to start recording! close device now
				DoMciSendString("close log", TRUE);
				g_bAudioLog = FALSE;	// not recording
			} else {
				g_bAudioLog = TRUE;		// succeeded, we are recording audio
			}
		}
	}

	// For Wizard of OZ version only
#if 0  // Don't normally open the event server port, only for custom WOZ builds
	// init server port so remote client can subscribe to log events
	InitEventServer();
#endif

	// return path actually used
	return strLogPath;
}

PRIVATE void WriteHeader()
{
	// Date, user, machine:
	CString strTime = g_timeStart.Format("%#c");
	fprintf(g_pfHist, "%s %s by %s on %s\n", szLogHdr, (LPCTSTR) strTime, szWinUser, szComputer);
	
	// Log file format version:
	fprintf(g_pfHist, "%s %d\n", szVersionHdr, nVersion);

	// help options
	WORD wOpt = theApp.m_wHelpFlags;
	fprintf(g_pfHist, "%s %03x %s %s %s\n", szHelpHdr, wOpt, 
		(wOpt & fProcedural) ? "Procedural" : "",
		(wOpt & fConceptual) ? "Conceptual" : "",
		(wOpt & fExample)    ? "Example"    : "");

	// Further param settings entered into log as events 
	// (should be done for above, too).

	// Log ANDES version number
	LogEventf(EV_ANDES_VERSION, theApp.m_strAndesVersion);

	// Also FBD.exe"version" = modification date of executable
	TCHAR szExePath[_MAX_PATH];
	VERIFY(::GetModuleFileName(theApp.m_hInstance, szExePath, _MAX_PATH));
	CFileStatus statExe;
	CFile::GetStatus(szExePath, statExe);
	// Format as  "mo day yr hr:min" w/2-digit yr, 24-hr clock, e.g. "8 5 98 24:30"
	CString strModifyDate = statExe.m_mtime.Format("%m %d %y %H:%M");
	LogEventf(EV_FBD_VERSION, strModifyDate);

	// Feedback mode (old default was synchronous, new is async).
	LogEventf(EV_ASYNC_MODE, "%d", theApp.m_nFeedback == FEEDBACK_ASYNC);
}

//
// SetStudent -- rename log file to incorporate Andes student name. 
//
// Returns the basename of the logfile.
//
PUBLIC CString HistoryFileSetStudent(LPCTSTR pszStudentName)
{
	if (g_pfHist == NULL) return "";	// no-op if no log file open.

	// Change file name
	// Form new name from student name and start time to try to ensure unique
	// Generates a long name, but w/no spaces (for ease of transfer to Unix)
	CString strTimePart = g_timeStart.Format("%b%d-%H-%M-%S");
	
	// Check student name for spaces and change to hyphens
	CString strFixedName(pszStudentName);
	for (int i = 0; i < strFixedName.GetLength(); i++)
		if (strFixedName[i] == ' ' || strFixedName[i] == '\t')
			strFixedName.SetAt(i, '-');

	// StudentName-StartTime functions as (hopefully) unique session Id
	CString strSessionId;
	strSessionId.Format("%s-%s", strFixedName, strTimePart);

	CString strNewPath = strDirPath + strSessionId + szLogExt;
	
	// Renaming open log file fails with EACESS on Win95 drive. 
	// Seems created logfile is inaccessible for renaming while we have it open. 
	// Work around by closing it, renaming, and opening it again for appending.

	// close current open file so we can rename it on disk.
	fclose(g_pfHist); 
	g_pfHist = NULL;		// temporarily invalid

	// attempt to rename the closed file   
	if (rename(strLogPath, strNewPath) == 0) {
		strLogPath = strNewPath;					// suceeded; strLogPath now new name 
	} else
		TRACE("Couldn't rename log %s to %s. Errno = %d\n", strLogPath, strNewPath, errno);

	// Open again under latest name. (whether rename succeeded or not). 
	// NB: this time open for appending! 
	if ((g_pfHist = fopen(strLogPath, "a")) == NULL) {  // lost it! horrible
		theApp.DoWarningMessage("Lost access to session log file while renaming for student!");
		g_bKeptHist = FALSE;		// don't count this
	}

	return strSessionId;
}

extern CString HistoryFileGetPath()
{
	return strLogPath;
}

//
// HistoryFileEnd -- terminates history file recording
//
PUBLIC void HistoryFileEnd()				// close and finish with history file
{
	if (g_pfHist != NULL)				// log file is open
	{
		// write closing data
		LogEventf(EV_END_LOG, "");		// signals normal end of event stream
		
		// finish with file:
		fclose(g_pfHist);
		g_pfHist = NULL;
	}

	if (g_bAudioLog)					// were also recording audio 
	{
		// stop recording now:
		DoMciSendString("stop log");

		// name .wav file with same path, title as log file, just diff extension
		char szFileName[_MAX_PATH];
		strcpy(szFileName, strLogPath);
		LPTSTR pszExt = strrchr(szFileName, '.');
		ASSERT(pszExt != NULL);
		*pszExt = '\0';       // clobber extension in buf to truncate string.

		// save the audio log to the specified file
		// NB: need quotes in command since pathname may contain spaces
		CString strCommand;
		strCommand.Format("save log \"%s.wav\"", szFileName); 
		DoMciSendString(strCommand);
			
		// finish recording
		DoMciSendString("close log");

		g_bAudioLog = FALSE;
	}
	// for WOZ only: terminate event server connection, if any
	EndEventServer();
}

// 
// HistoryUpload -- put up the ftp dialog to try to 
// upload the history file to a shared location
//
PUBLIC void HistoryUpload()
{
	if (!g_bKeptHist) return;
#if 0
	CFTPDlg dlg;
	dlg.m_strPathName = strLogPath;
	dlg.m_strFileName = strFileName;
	dlg.m_strComputer = szComputer;
	dlg.m_strWinUser  = szWinUser;
	// get other info out of registry
	dlg.DoModal();
#endif 0
}

// Log formats session timestamps as hour:minute:sec strings for human reader.
// hour field omitted if under an hour to save a little space in log.
PRIVATE CString TimeToHms(int nElapsed) // time in total seconds
{
	CString strHms;
	int nHours = nElapsed/3600;
	if (nHours > 0)	// over an hour, need H:M:S
		strHms.Format("%d:%02d:%02d", nHours, (nElapsed%3600)/60, nElapsed%60);
	else  // less than an hour, just use M:S
		strHms.Format("%d:%02d", nElapsed/60, nElapsed%60);
	
	return strHms;
}

// Parse timestamp string, putting total seconds into *pnTime.
// Like scanf, returns no of scanned items = 1 for success, 0 for failure, 
// Accepts old format timestamps (integer seconds) as well.
// Leading, trailing whitespace OK in arg, but should just contain one timestamp, nothing after
PRIVATE int ScanTime(const char* pszHms, int* pnTime) 
{
	const char* pFirstColon = strchr(pszHms, ':');
	if (pFirstColon == NULL) // no colon: assume old timestamp, total seconds as int
		return sscanf(pszHms, "%d", pnTime);

	// else at least one colon
	const char* pLastColon = strrchr(pszHms, ':');
	int nMinutes, nSeconds, nHours;
	if ((pFirstColon == pLastColon) // just one colon => M:S
		  && (sscanf(pszHms, "%d:%d", &nMinutes, &nSeconds) == 2)) {
		*pnTime = (60 * nMinutes) + nSeconds;
		return 1;
	} 
	// else more than one colon => H:M:S
	if (sscanf(pszHms, "%d:%d:%d", &nHours, &nMinutes, &nSeconds) == 3) {
		*pnTime = (3600 * nHours) + (60 * nMinutes) + nSeconds;
		return 1;
	}
	// else failed to scan a time:
	return 0; 
}

//
// Logf -- format and log a history file message
//
// Adds timestamp and prints a log entry. Does nothing if
// if not recording history, so safe to call even if recording is off.
// Takes a printf style variable argument list. 
// The varargs code was copied from  MFC's AfxTrace routine.
//
#define MAX_MSG 4096				// may contain long helpsys msg. ~2 pgs better be enough	
PRIVATE char msg[MAX_MSG] = "";		// current output message buffer
PRIVATE int g_lastflush = 0;		// time of last fflush call to flush buffers
		
PUBLIC void Logf(LPCTSTR lpszFormat, ...)	
{
	if (g_pfHist == NULL) return;	// no effect if not logging

	// format event msg string into our message buffer. 
	va_list args;
	va_start(args, lpszFormat);
	int nWritten = vsprintf(msg, lpszFormat, args);
	ASSERT(nWritten < (MAX_MSG - 1));
	va_end(args);
	
	// write log record with time stamp and trailing newline into log stream
	int now = HistTime();
	fprintf(g_pfHist, "%s\t%s\n", TimeToHms(now), msg);

	// Want to periodically flush the stdio buffers to system so some log will be 
	// available in case the application crashes. To avoid doing a system call
	// on each message, which can be generated as frequently as every mouse movement, 
	// we do it only if more than FLUSH_INTERVAL seconds since last flush.
#	define FLUSH_INTERVAL 0			// interval between fflush calls in seconds
	// (Actually, experiment shows acceptable  performance on our systems 
	// even if FLUSH_INTERVAL set to 0)
	if (now - g_lastflush >= FLUSH_INTERVAL) 
	{
		fflush(g_pfHist);
		g_lastflush = now;
	}
}

PRIVATE const char* GetEventName(EventID id);

// Newer variant logs given event code, arg format, and args.
// Using this one ensures string from table matches log entry (unless table changed).
PUBLIC void LogEventf(EventID id, LPCTSTR pszArgFmt, ...)
{
	if (g_pfHist == NULL) return;	// no effect if not logging
	
	const char* pszEvName = GetEventName(id);
	ASSERT(pszEvName != NULL);

	// format param string part into our message buffer. 
	va_list args;
	va_start(args, pszArgFmt);

	int nWritten = vsprintf(msg, pszArgFmt, args);
	ASSERT(nWritten < (MAX_MSG - 1));
	
	va_end(args);
	
	// emit log record with time stamp and trailing newline into log stream
	int now = HistTime();
	if (! g_bEventOnly)
		fprintf(g_pfHist, "%s\t%s %s\n", TimeToHms(now), pszEvName, msg);

	// Wizard of OZ only
	// also broadcast to any remote subscriber. For now, just broadcast all events
	NotifyEvent(pszEvName, msg);

	// flush buffer periodically 
	if (now - g_lastflush >= FLUSH_INTERVAL) {
		fflush(g_pfHist);
		g_lastflush = now;
	}
}

// Worker routine allows setting of time stamp, only used externally in
// special case.
PUBLIC void LogEventfAt(int nTime, EventID id, LPCTSTR pszArgFmt, ...)
{
	if (g_pfHist == NULL) return;	// no effect if not logging
	
	const char* pszEvName = GetEventName(id);
	ASSERT(pszEvName != NULL);

	// format param string part into our message buffer. 
	va_list args;
	va_start(args, pszArgFmt);

	int nWritten = vsprintf(msg, pszArgFmt, args);
	ASSERT(nWritten < (MAX_MSG - 1));
	
	va_end(args);
	
	// emit log record with time stamp and trailing newline into log stream
	if (! g_bEventOnly)
		fprintf(g_pfHist, "%s\t%s %s\n", TimeToHms(nTime), pszEvName, msg);	

	// Wizard of OZ only
	// also broadcast to any remote subscriber. For now, just broadcast all events
	NotifyEvent(pszEvName, msg);
}

/*
int GetHistDelta()			// time since last write, updates timestamp			
{
	CTime timeNow = CTime::GetCurrentTime();
	CTimeSpan timeDelta = timeNow - g_timePrev;
	g_timePrev = timeNow;

	return timeDelta.GetTotalSeconds();
}
*/

// Following helpers used to include references to objects in log messages.
//

/*
// OBJ_ID. Generate an integer object identifier, because some objects may not have 
// labels, and there's no guarantee user-generated labels are unique anyway.
// For now, just use the pointer value as long int; Logf calls should format in hex.
This no longer used at all since now objects get symbolic ids.
PUBLIC inline long OBJ_ID(CDrawObj* pObj) 
{
	return (long) pObj;
}
*/
// OBJ_NAME: Get human-readable label for an object or empty string if none.
//
// Use in log messages to include the object label for those that have it, for the 
// benefit of human readers and to provide a possible check on log playback. Callers 
// currently package this in vertical bars to delimit, on the assumption that it may 
// be empty or contain spaces. Now most object musts be assigned labels that cannot 
// contain spaces, but there  is still possibility name is empty as at time we popup 
// initial dialog to define label. Some objects like Axes may not have labels at all.
// (This macro now largely unnecessary or should be virtual func on DrawObj;
// it persists for historical reasons only.) 
PUBLIC inline LPCTSTR OBJ_NAME(CDrawObj* pObj)
{
	// Suppress "name" for text objects because field actually used to contain
	// arbitrary length text
	if (pObj == NULL ||
		pObj->IsKindOf(RUNTIME_CLASS(CLabel)))
		return "";

	// else can just use name field. 
	// Some objs such as axes may not have labels, but that's OK, will be empty string
	return (LPCTSTR) pObj->m_strName;
}

PUBLIC inline LPCTSTR TREE_ID(CString pos)
{
	ASSERT(!pos.IsEmpty());
	return pos;
}

PUBLIC inline LPCTSTR TREE_NAME(CPlanItem* pPlanItem)
{
	if (pPlanItem!=NULL)
	{
		return pPlanItem->GetText();
	}
	else 
		return "";
}

// Utilities for formatting text arguments that may run multiple lines:

// EscapeText: Convert possibly multi-line text string into C-style escape sequence,
// Returns result in a CString.
PUBLIC CString EscapeText(LPCTSTR pszSrc)
{
	// copy chars from src to dst, putting escape sequence for newlines
	// allocate temp buf for dst.
	int nSrcLength = strlen(pszSrc);
	TCHAR* pszBuf = new TCHAR[2 * nSrcLength + 1];	
	TCHAR* pszDst = pszBuf; 
	for (int iSrc = 0; iSrc < nSrcLength; ++iSrc) {
		if (pszSrc[iSrc] == '\r' && 
			pszSrc[iSrc+1] == '\n'){		// CR LF   -> Backslash + 'n'
			++iSrc;
			*pszDst++ = '\\';
			*pszDst++ = 'n';
		} else if (pszSrc[iSrc] == '\n') { // plain LF (shouldn't happen)
			*pszDst++ = '\\';
			*pszDst++ = 'n';
		} else if (pszSrc[iSrc] == '\\') {// Backslash -> Backslash + Backslash
			*pszDst++ = '\\';
			*pszDst++ = '\\';
		} else	// ordinary char: copy to dest
			*pszDst++ = pszSrc[iSrc];
	}
	*pszDst++ = '\0';	// append terminating NUL

	// Copy buf to CString result and free temp buf
	CString strResult = pszBuf;
	delete [] pszBuf;

	return strResult;
}

// UnescapeText: Expand C-Style escape sequence from pszSrc into multiline text string
// returning CString result
PUBLIC CString UnescapeText(LPCTSTR pszSrc)
{
	// copy chars from source to dst, copying escape sequence for newlines
	int nSrcLength = strlen(pszSrc);
	CString strDst = "";	// will build up by successive appending (inefficient?)
	for (int iSrc = 0; iSrc < nSrcLength; ++iSrc) {
		if (pszSrc[iSrc] == '\\') {	
			//  escape seq -- peek at next char
			TCHAR chNext = pszSrc[iSrc + 1];
			if (chNext == 'n') {
				++iSrc;					// advance in src
				strDst += "\r\n";
			} else if (chNext == '\\') {
				++iSrc;					// advance in src
				strDst  += '\\';
			} else	// shouldn't happen if backslashes escaped correctly
				strDst  += pszSrc[iSrc];
			
		} else	// ordinary character: copy it to dest
			strDst += pszSrc[iSrc];
	}
	
	return strDst;
}

// Utility for dealing with string valued fields in logs:
// For ease of parsing fixed size records:
// replace spaces by # and indicate empty strings by "_"
// 
PUBLIC CString ValToArg(CString& strValue)
{
	if (strValue.IsEmpty())
		return "_";

	CString strResult = strValue;
	strResult.Replace(' ', '#');
	return strResult;
}

PUBLIC CString ArgToVal(LPCTSTR szLog)
{
	CString strResult;
	if (strcmp(szLog, "_") == 0)
		return strResult;
	strResult = szLog;
	strResult.Replace('#', ' ');
	return strResult;
}

///////////////////////////////////////////////////////////////////////////////
//
// "Event server": allows remote client to connect to "subscribe" to receive
// notifications of logged events. Used for sharing interface with remote
// fbd viewer client used to play events in wizard of OZ mode. 
//
///////////////////////////////////////////////////////////////////////////////
static const char* szCRLF = "\r\n";		// our msg terminator, used in event sink

#ifdef HELPIFC_TCP						// everything here specific to WOZ version

// CEventSrvr: Listening server socket class: 
// Once started, just accepts 1 connection spawning CEventSock
class CEventSrvr : public CSocket
{
public:
	enum { MY_PORT = EVENT_SRVR_PORT };		
	void StartServer()	// starts listening for connection requests
	{
		if (! (Create(MY_PORT) && Listen(1)) ) 
		   TRACE("Failed To Init Listening Server Socket!");
		else
		   TRACE("Event server listening at port %d\n", MY_PORT);
	}

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEventSrvr)
	public:
	virtual void OnAccept(int nErrorCode);
	//}}AFX_VIRTUAL
};

// CEventSock -- Handles socket connection once established:
// Output only socket.
class CEventSock : public CSocket
{
public:
	// Send an event (command string) to the client.
	void SendEvent(LPCTSTR pszEventMsg, LPCTSTR pszArgs);

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEventSock)
	public:
	virtual void OnClose(int nErrorCode);
	//}}AFX_VIRTUAL
};

PRIVATE CEventSrvr g_sockSrvr;		// listening server.
PRIVATE CEventSock* g_pSockEvent;	// dynamically allocated to handle connection.

void CEventSrvr::OnAccept(int nErrorCode) 
{
	TRACE("Server socket: Got connection request\n");

	// ignore if already connected
	if (g_pSockEvent != NULL) return;

	// create a socket to handle the connection, linking it to main dlg window
	g_pSockEvent = new CEventSock();
	
	if (Accept(*g_pSockEvent)) 
	{
		// Stop listening, only one subscriber allowed (could change to allow more)
		g_sockSrvr.Close();

		// Never going to read on this pipe, might as well disable receives
		g_pSockEvent->ShutDown(0);
		
		// Get peer address for diagnostic.
		SOCKADDR_IN sinPeer; 
		int nSaddrSize = sizeof(sinPeer);
		g_pSockEvent->GetPeerName((SOCKADDR*)&sinPeer, &nSaddrSize);
		struct in_addr inPeer = sinPeer.sin_addr;	
		TRACE("Event server: Connected to %s\n", inet_ntoa(inPeer));
// TEMP: for debugging, note when connection was made in log
		Logf("ATTACHED %s", inet_ntoa(inPeer));
// END TEMP
		
		// Send some initial msgs using Logging funcs.
		// set flag to direct following logging msgs down event socket only
		BOOL bPrev = g_bEventOnly;
		g_bEventOnly = TRUE;
		
		// !!!Should send student name for display (in all cases)
		// !!!Should send entries for any option values as well (in all cases)
		
		// if already have problem open, joining client would needs to be brought up
		// to date on its state, by sending Open event and initial entries. Attempted
		// to do this but it's too hard to do correctly in general:
		// Normal fbd defers connecting to helpsys till just before first problem open.
		// We will usually be waiting in problem launch sequence for read-problem-info 
		// to return when remote viewer spawned from HelpDesk calls us to attach to the 
		// interface.  But syncing the remote state with ours correctly is heavily 
		// dependent on where we are in the problem launch sequence when the connection 
		// is made. On solution open we (1) recheck the initial entries with help system and 
		// then (2) log all entries after rechecking. If we are still in (1) then 
		// then remote process will get entries when we log them in (2). But if we've 
		// finished logging them, then we have to make them on remote system (could do
		// pDoc->LogInitEntries()). So we would have to know where we are  -- can't
		// do twice, since ENTRY event handlers will make duplicates. 
		// More generally, attachment could come at any time socket msgs are pumped, so
		// could be in modal loop for RPC wait or dialog, and will never work if
		// comes in middle of modal loop (DDE wait or dialog) since remote server is 
		// not in sync with our state, so won't be able to process next event.
		// We would have to send the right events to put remote fbd in same modal state.

		// So for now, rely on connection being made before any document is opened.
		CFBDDoc* pDoc;
		if (pDoc = theApp.GetCurrentProblem()) 
		{
			// Send open-problem command. OnOpenDocument normally does help sys call 
			// (read-problem-info), so remote process will be in result-wait state.
			LogEventf(EV_OPEN_PROBLEM, "%s", pDoc->m_strProblemId);
			// Send helpsys result (!assumes success -- would have to send NIL if
			// we failed to connect in our process. But we don't care much in that case.
			LogEventf(EV_DDE_RESULT, "|T|"); 

			// Do if haven't logged init entries yet:
			// pDoc->LogInitEntries()
		}
		// restore normal log output
		g_bEventOnly = bPrev;
	} 
	else {
		TRACE("Event Server: Connection Accept failed!\n");
	}
}

void CEventSock::SendEvent(LPCTSTR pszEventName, LPCTSTR pszArgs)
{
	CString strMsg;
	// send with CRLF terminator (can be shown in telnet window or saved in text file).
	strMsg.Format("%s\t%s%s", pszEventName, pszArgs, szCRLF);
	Send(strMsg, strMsg.GetLength()); 
}

void CEventSock::OnClose(int nErrorCode) 
{
	TRACE("Event sock: connection closed/aborted by client, code=%d\n", nErrorCode);
	// release socket and commit suicide
	Close();
	delete this;
	g_pSockEvent = NULL;

	// Start listening for new connection again
	InitEventServer();
}
#endif HELPIFC_TCP

// 
// common interface to WOZ only event server operations (no-ops in non-TCP ver).
//
PRIVATE void InitEventServer()
{
#ifdef HELPIFC_TCP
	g_sockSrvr.StartServer();
#endif
}

PRIVATE void NotifyEvent(LPCSTR pszEvName, LPCSTR msg)
{
#ifdef HELPIFC_TCP
	if (g_pSockEvent)
		g_pSockEvent->SendEvent(pszEvName, msg);
#endif 
}

PRIVATE void EndEventServer()	// Shutdown for good at end.
{
#ifdef HELPIFC_TCP
	if (g_pSockEvent) {			// close any open connections
		g_pSockEvent->ShutDown(2);
		g_pSockEvent->Close();
		delete g_pSockEvent;
		g_pSockEvent = NULL;
	}
#endif
}

// 
// For running as a remote ANDES viewer. This attaches to the remote ANDES event
// server, receives events on the "EventSink" socket and executes them. 
// I.e. implements the client side of the event server functionality above.
//

// CEventSink -- socket receives events from remote ANDES and posts notification
// msgs to main thread whenever a complete message is in its buffer.
// The data buffer functions as a message queue; on notification the application
// extracts and executes them (LogPlayerProcessInput).
// This is an input only socket.
class CEventSink : public CSocket
{
public:
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEventSock)
	public:
	virtual void OnReceive(int nErrorCode);
	virtual void OnClose(int nErrorCode);
	//}}AFX_VIRTUAL

	// True if have complete message in queue (buffer)
	BOOL HaveCompleteMsg()
		{ return m_strDataBuf.Find(szCRLF) != -1; }

	// Remove next message from queue (buffer), returns F if none
	BOOL GetNextMsg(CString& strMsg);

	// Empty queue (buffer) of pending messages.
	void DiscardMsgs() { m_strDataBuf.Empty(); };

	// temp for debugging:
	void TRACEBUF();				// trace change in buffer contents
protected:
	CString m_strDataBuf;			// unparsed data received
	int DoReceive();				// worker routine to fill data buf
};

PRIVATE CEventSink g_sockEvSink;

#if 0	// temporary: for debugging release view, show trace msgs in modeless dialog:
#include "TraceDlg.h"
PRIVATE CTraceDlg g_dlgTrace;
#endif 0

// Connect our sink to remote event server source
PUBLIC BOOL LogPlayerConnectRemote(LPCTSTR pszRemoteHost)
{
	AfxSocketInit();	// needed in non-TCP version!

	int nError;
	if (! g_sockEvSink.Create()) {
		nError = g_sockEvSink.GetLastError();
		TRACE("Couldn't create socket!. Error=%d\n", nError);
		return FALSE;
	}
	if (! g_sockEvSink.Connect(pszRemoteHost, EVENT_SRVR_PORT)) {
		nError = g_sockEvSink.GetLastError();
		TRACE("Couldn't connect to %s:%d. Error=%d\n", pszRemoteHost, EVENT_SRVR_PORT, nError);
		return FALSE;
	}

	TRACE("EventSink Connected to %s\n", pszRemoteHost);
#if 0
	g_dlgTrace.Create(AfxGetMainWnd());
	g_dlgTrace.AddMsg("Connected");
#endif 0	

/* seems to cause remote close of socket (get OnClose notification) not sure why
	// we don't send, so might as well shut down for that (sends TCP FIN).
	g_sockEvSink.ShutDown(SD_SEND);*/
	return TRUE;
}

// TEMP for release debugging: show pending buffer contents on interface
void CEventSink::TRACEBUF()
{
	CString strTemp(m_strDataBuf);
	strTemp.Replace(szCRLF, "/");
	strTemp.Replace('\t', ' ');
	// g_dlgTrace.AddMsg("QUEUE: " + strTemp);
	Logf("QUEUE: " + strTemp);
}
// END TEMP

// Handle incoming data
// Complete messages delimited by CRLF's, which shouldn't occur in data.
// Note data chunk received may contain several messages, or incomplete message,
// although in practice it usually contains exactly one msg.
void CEventSink::OnReceive(int nErrorCode) 
{
	DoReceive();
}

int CEventSink::DoReceive()		// returns nRead, 0 if no more, or SOCKET_ERROR (-1)
{ 
	// receive available data into temp buf, 
	char szTemp[MAX_MSG];  // holds new chunk of data
	int cbLength;
	if ((cbLength = Receive(szTemp, MAX_MSG - 1)) > 0)	// leave room to add NUL
	{
		// make C string of current chunk
		szTemp[cbLength] = '\0';

		// Trace for debugging
		CString strTemp(szTemp);
		strTemp.Replace(szCRLF, "\\r\\n");
		if (cbLength < 475) // Max TRACE msg is 512, asserts if greater
			TRACE("Eventsink got %d bytes: |%s|\n", cbLength, strTemp);

		// Append new chunk to any unparsed msgs remaining in buf from prior receives
		m_strDataBuf += szTemp;
		TRACEBUF();

		// If have complete msg, post notification to app to dispatch it. see below.
		if (HaveCompleteMsg())
			theApp.PostThreadMessage(WM_EVENT_MSG, NULL, NULL);
	}
	
	return cbLength;	// ignores read errors; "EOF" 0 return after remote close
}

// Extract next complete msg out of buffer; TRUE if have one, FALSE if not.
BOOL CEventSink::GetNextMsg(CString& strMsg)
{
	// find complete message by splitting at CRLF terminator
	int iEndMsg;
	if ((iEndMsg=m_strDataBuf.Find(szCRLF)) == -1)
		return FALSE;
	
	// have a complete line: strip out message, 
	// leaving any remainder in m_strDataBuf
	strMsg = m_strDataBuf.Left(iEndMsg);	// omits CRLF delimiter
	m_strDataBuf = m_strDataBuf.Mid(iEndMsg + 2);

	return TRUE;
}

// Handle any ready events on the input queue.
// Public so can be called from app handler for custom msg to ourselves.
PUBLIC void LogPlayerProcessInput()
{
	// Execute first complete msg in buffer.
	CString strMsg;
	if (! g_sockEvSink.GetNextMsg(strMsg))
		return;

	// dispatch the current message through command interpreter
// TEMP for debugging: log it, signalling it's from remote
	LogEventf(EV_REMOTE_CMD, "%s%s", strMsg, g_sockEvSink.HaveCompleteMsg() ? "..." : "");
	// g_dlgTrace.AddMsg("!" + strMsg);
// END TEMP
	ExecuteCmd(strMsg);

	// If any more messages left in buffer, post custom message to the app to
	// call us back again (handled in app). We do this rather than loop 
	// to execute all available msgs here because some msgs (DDE_RESULT, BTN_CLICK 
	// Ok/Cancel in dialog) set flags to end modal loops in the app. We must 
	// yield control so the modal loops can terminate and return up the subroutine
	// stack before the next message is processed, or else the app state could be wrong 
	// for the next message in the buffer. (The dialog would still be up, e.g, or
	// OnOpenDocument, say, would still be waiting on the stack for the 
	// read-problem-info helpsys call result).

	// One solution would be to yield here only in case of the special loop-ending 
	// messages. Or check app state to see if in a dialog flagged to end. 
	// But simplest just to always yield without examining message.
	if (g_sockEvSink.HaveCompleteMsg())
		theApp.PostThreadMessage(WM_EVENT_MSG, NULL, NULL);
}


PRIVATE void OnEventSinkClose(int nErrorCode);	// forward
void CEventSink::OnClose(int nErrorCode) 
{
	// g_dlgTrace.AddMsg("OnClose");
	Logf("Socket-close %d", nErrorCode);
	TRACE("Event sink: connection closed/aborted, code=%d\n", nErrorCode);
	
#if 0 // taken out since still sometimes hung at end. For now just shutdown "abortively".

	// NB! this notification can be received while the final event msgs (e,g, for 
	// initiating app close, closing final problem -- w/close-problem call to helpsys
	// and response -- then ending log) are still waiting in our 
	// message buffer. Esp likely if user closed app with problem open, so they are all 
	// generated at nearly the same time. So it may not betoken an abortive close,
	// it could just be out of sequence with our cmd queue. 
	// So we add a special socket-closed msg to the end of the 
	// input buffer to serialize the processing. In case
	// normal END-LOG is processed, this msg will never get processed itself,
	// we will close the socket and empty the queue on the normal END-LOG.
	// !! possible abortive close and we will never get to end of message stream
	// because blocked in a modal wait state for an event that never comes.

	// make sure we've received any undelivered data buffered in socket layer
	int nRead;
	do {
		nRead = DoReceive();
	} while (nRead > 0);

	// if still have commands in buffer, add our special event msg to the end
	// no need to post notification, should already be one pending for cur contents
	// (posted by DoReceive every time it receives at least one complete packet.)
	if ((nErrorCode == 0) && ! m_strDataBuf.IsEmpty()) 
	{
		const char* pszEvName = GetEventName(EV_SOCKET_CLOSE);
		ASSERT(pszEvName != NULL);
		m_strDataBuf += CString(pszEvName) + szCRLF;
		TRACEBUF();
	} else // just handle the close now
#endif 0
		OnEventSinkClose(nErrorCode);
}

PRIVATE void OnEventSinkClose(int nErrorCode)
{
	Logf("OnEventSinkClose");

	// release socket from our end.
	g_sockEvSink.ShutDown(2);
	g_sockEvSink.Close();

	// clearing this signals app that no longer remote viewing, allowing
	// user input events to get through again, just in case they are needed.
	theApp.m_strRemoteHost.Empty();

	// report abnormal close to user. (Normal end processed on EV_END_LOG).
	if (nErrorCode != 0)
		AfxMessageBox("Lost connection to remote ANDES");

	// Just Shut down process abortively if no more command input.
#if 0
	// Close any open problem to avoid prompt to save.
	// !! if we are inside a modal loop, need some way to get out of it (e.g.
	// throw exception to unwind stack -- would need to setup handler earlier)
	// !! OnCloseDocument will simulate close-problem call to helpsys, leaving
	// us blocked in wait state.
	CFBDDoc* pDoc = theApp.GetCurrentProblem();
	if (pDoc)
		pDoc->COleDocument::OnCloseDocument();	// just close, skip helpsys cleanup
#endif 0

	// purge any pending command msgs from queue, no need to execute them
	g_sockEvSink.DiscardMsgs();

	// Post end of input msg to queue. Should trigger break out of any modal loop.
	// we may be in when the close event comes.
	AfxPostQuitMessage(0);
}

///////////////////////////////////////////////////////////////////////////////
// 
// Playback mode: drives an interpreter pseudo-process for "playing back" 
// events recorded in a history file in real time.
//
///////////////////////////////////////////////////////////////////////////////
//
// Main playback state:
// 
PRIVATE FILE* g_pfPlayback = NULL;	// File pointer for log we are playing
PRIVATE CString g_strPathName;		// Full pathname of log we are playing
PRIVATE int g_nLogTimeElapsed=0;	// Log time playback counter, seconds
PRIVATE CTime g_timePlayStart;		// Real time last normal play segment began
PRIVATE int g_nLogPlayStart;		// Log time (secs) last normal play seg began
PRIVATE BOOL g_bFastForward = FALSE;// Set if playing back in Fast Forward mode
PRIVATE BOOL g_bPaused = FALSE;		// Set if playback is paused
PRIVATE WORD g_wOldHelpFlags;		// Saved help flags before playback
PRIVATE WORD g_nOldFeedbackMode;	// Saved Async mode flag before playback
PRIVATE BOOL g_bWasDisabled = FALSE;// Saved mainframe disabled state before playback
PRIVATE BOOL g_bCallHelp = FALSE;	// generate calls to help system during playback
									// FALSE => simulate helpifc from logged results
PRIVATE IPlayerUI* g_pPlayUI = NULL; // pointer to control dialog to notify
PRIVATE int g_nLogLength = -1;		// length of log in seconds. -1 if unknown
PRIVATE BOOL g_bPlayingAudio = FALSE;	// set if we are playing audio log as well
PRIVATE int g_nGotoTime = -1;		// positive => FF'ing to this time
PRIVATE BOOL g_bStepping = FALSE;	// "single-stepping" to next transcribable event
PRIVATE BOOL g_bInInitCheck;		// flags when replaying CheckInitEntries code
PUBLIC  int g_nLogAndesVersion=0;	// log's Andes version no. as decimal int e.g 7.1.12 = 7112
// 
// With pausing and fast forwarding, time segments map like like this:
//
//                                            timePlayStart
//                                              |                                        
//                Play      Pause   FF    Pause |  Play
//  Real time:  +XXXXXXXXX+-------+YYYYY+-------+ZZZZZZZZZZZZ
//                                |     |       ////////////
//                        +-------+     +----+ ////////////             
//  Log time:   +XXXXXXXXX+YYYYYYYYYYYYYYYYYY+ZZZZZZZZZZZZ
//                                           |
//                                        nLogPlayStart

//
// During playback of an event we can enter modal dialog:
//
PRIVATE CDialog* g_pDlg = NULL;		// Currently active dialog; NULL if none
PRIVATE BOOL g_bEndDialog = FALSE;	// Flag signals dialog has ended during event
//
// Structure of the playback process:
// During playback, we set a playback timer to fire a callback every second.
// This calls PumpReadyLogEvents to check if there are any log events ready to 
// run. The PumpReadyLogEvents routine loops and calls PlayEvent for all events 
// that are due to be replayed when it is called. PlayEvent finds the object
// that knows how to handle the event and calls its DispatchEvent method to
// do the work of replaying the event.
//
// During replay of an event the playback timer is disabled to avoid re-entrant
// calls during time-consuming events. Seems this can happen during replays that 
// produce message boxes. !!! Standard message boxes are currently not well handled -- 
// user must explicitly dismiss them to continue playback. Should be replaced everywhere
// by our own log-aware message box substitute on playback, at least. Might also happen 
// during long DDE calls.
//
// Replaying an event can call a handler procedure which enters CDialog::DoModal
// to process a dialog box before returning. We handle this by using dialog 
// boxes derived from our own log-aware LogDialog base class. This class is 
// instrumented to start a replay timer again to continue the periodic pumping 
// of events from the log file through the playback dispatcher from the log 
// file. It also sends a signal up the stack to the event pump when it 
// has replayed an event that will terminate the dialog.
//
// A trace of the state during playback can look like:
//
// BeginPlayback
// [Set playback alarm for main App window]
// OnPlaybackTimer
// ^   PumpReadyLogEvents(Mainwnd) 
//     [saves call time and checks against next log event. If ready:]
// |   [Disables playback alarm for mainwnd]
// |   [For each ready event calls:]
// |     PlayEvent
// |     [Maps event type to handler object]
// |        pObj->DispatchEvent 
// |         [Event-specific Handler code, which may result in call to:] 
// |          LogModalDlg (dlg1)
// |             StartModalReplay [Sets g_pDlg = dlg1, loads next event] 
// |             LogDialog::OnInitDialog [Sets Alarm for dlg1]
// |           +>OnPlaybackTimer
// |           |      PumpReadyLogEvents(Dlg1)
// |           |      [Disables playback alarm for dialog]
// |           |      [For each ready event calls:] 
// |           |           PlayEvent
// |           +-----------+.. [Series of dialog events replayed]  
// |                       [Replay of OK or Cancel event:]
// |                          LogDialog::OnOK or OnCancel
// |                            SignalEndDialog [flags end to PumpReadyLogEvents]
// |                            [Kills alarm for dlg1]
// |                  [tests flag and exits w/o advancing]
// |             EndModalReplay [Clears g_pDlg ]
// |          [in handler, Possibly have a second dialog...]
// |          LogModalDlg
// |             StartModalReplay [Sets g_pDlg, loads next event] 
// |             LogDialog::OnInitDialog [Sets Alarm for dlg 2]
// |           +>OnPlaybackTimer
// |           |      PumpReadyLogEvents(dlg2)
// |           |      [Disables playback alarm dlg2]
// |           |      [For each ready event calls:] 
// |           |           PlayEvent
// |           +-----------+.. [Series of dialog events replayed]  
// |                       [Replay of OK or Cancel event:]
// |                          LogDialog::OnOK or OnCancel
// |                            SignalEndDialog [flags end to PumpReadyLogEvents]
// |                            [Kills alarm for dlg2]
// |                  [tests flag and exits w/o advancing input]
// |             EndModalReplay [Clears pDlg ]
// |          [in handler, eventually returns]
// |   [Back to first event pump. ??? Checks time and probably exits]
// +---[Set playback timer for next event]
//
// Multiple levels of dialogs should work but have not been tested.
//  

// Timer management:           
const int TIMER_PLAYBACK = 13;				// id for our timers (arbitrary)
PRIVATE void PumpReadyLogEvents(CWnd* pWnd);	// Runs all ready events

// Alarm handler procedure: (global function, not tied to a window).
PRIVATE void CALLBACK OnPlaybackTimer(HWND hwnd, UINT, UINT, DWORD)
{
	// Should check that hwnd is reasonable given state.
	CWnd* pWnd = CWnd::FromHandle(hwnd);
	PumpReadyLogEvents(pWnd);
}

// Schedule next wakeup call of playback process.
PRIVATE void SetPlaybackTimer(CWnd* pWnd)
{
	UINT nSecs;			// seconds to wait

	// if in paused state don't set timer till resumed	
	if (g_bPaused)
		return;

	// if fast forward return immediately, else just set to one second
	// Might think we could wait longer if there are no events to play, 
	// but we need to update the log file elapsed time counter
	nSecs = g_bFastForward? 0 : 1000;

	pWnd->SetTimer(TIMER_PLAYBACK, nSecs, OnPlaybackTimer);
}

// Suspend playback alarm set by given window
PRIVATE void KillPlaybackTimer(CWnd* pWnd)
{
	// TRACE("kill playback timer\n");
	pWnd->KillTimer(TIMER_PLAYBACK);
}

//
// Log file input for playback: 
// 
// This module mangages a log file as a stream of event records. Each event 
// record is stored as a single line of ascii text.
// During playback the current event line is loaded into a global buffer 
// (internal to this module) g_line and split into timestamp and event msg (rest).
// The msg contains the event name followed by event-specific parameters; these
// last are passed to the event dispatcher routines as a NUL-terminated string argument
// to be parsed by the event handler.
//
// Note g_line is in effect a lookahead buffer into the log file (event stream). Our 
// invariant was supposed to be that it is always preloaded with the *next* ("on deck") 
// event rec to be replayed from the log file. But this is not consistently observed
// *during* the playback of an event. We normally advance the event stream pointer to
// the next event only *after* the playback of the event is completed. But if the playback 
// of an event causes entering a modal dialog, the LogDialog will advance the stream when 
// starting. The dialog keeps pumping successive events out of the stream, leaving last
// played event in the on-deck buffer when done.   
// !!! This is too hairy, should be made systematic and consistent. Should 
// probably use a PeekNextEvent and GetNextEvent pair on the model of PeekMessage and 
// GetMessage. Also better to use an Event data structure.
//
PRIVATE char g_line[512] = "";		// buffer for next event log message
PRIVATE int  g_nLine = 0;			// source line number, used for diagnostics
PRIVATE int  g_nEventTime;			// Log time of next event
PRIVATE char * g_pCmd = NULL;		// points into cmd string part of next event

PRIVATE BOOL ReadLine()			// Fill buffer w/next input line; FALSE on EOF.
{
read:	
	if (g_pfPlayback != NULL &&
		fgets(g_line, sizeof(g_line), g_pfPlayback) != NULL) 
	{
		++g_nLine;					// keep track for reporting syntax errors

		// skip empty lines (allow insertion for readability when manually editing demos)
		if (g_line[0] == '\0')
			goto read;

		// replace terminating newline with NUL, making line into C string
		int nLast = strlen(g_line) - 1;
		if (g_line[nLast] == '\n')	
			g_line[nLast] = '\0';
		

		return TRUE;
	}
	return FALSE;
}

// AbortPlayback -- internal routine stops and reports fatal error during playback.
PRIVATE void AbortPlayback();		// forward ref

// GetNextEvent: Load next event record into lookahead globals. 
// Parses timestamp into g_nEventTime and sets g_pCmd to point to rest of msg =
// string containing event-name, space, + args.
// RETURNS: FALSE if eof or error; TRUE on success.
PRIVATE BOOL GetNextEvent()		
{
	// load the next line
	if (! ReadLine() )
		return FALSE;

	// parse initial timestamp out of message:
	char * inp = g_line;
	char szTimeStamp[32];
	// 
	if (sscanf(g_line, "%s", szTimeStamp) != 1) {
		AbortPlayback();
		return FALSE;
	}
	
	if (ScanTime(szTimeStamp, &g_nEventTime) != 1){
		AbortPlayback();
		return FALSE;
	}

	// and leave g_pCmd pointing to rest of string (event-name + args)
	g_pCmd = g_line + strlen(szTimeStamp) + 1;

	// Show on-deck event line w/its time on Log Control tool
	CString msgNext; 
	msgNext.Format("%02d:%02d:%02d%s", g_nEventTime/3600, (g_nEventTime%3600)/60, g_nEventTime%60,
									strchr(g_line, '\t'));
	if (g_pPlayUI) g_pPlayUI->ShowNextEvent(msgNext);

	return TRUE;
}

// Verify the header lines in log file, and set application options accordingly,
// saving old values (help flags, feedback).
PRIVATE BOOL CheckHdr()			
{
	if ( !(ReadLine() 
		   && (strncmp(g_line, szLogHdr, strlen(szLogHdr)) == 0) 
		   &&  ReadLine() 
		   && (strncmp(g_line, szVersionHdr, strlen(szVersionHdr)) == 0)) )
		return FALSE;
	// Parse version out of version line
	int nVersion, nHelpFlags;
	CString strFmt = CString(szVersionHdr) + " %d";			// sscanf format to use
	if (! sscanf(g_line, (LPCSTR) strFmt, &nVersion) == 1)  // scanned 1 int
		return FALSE;

	// Version 1: read help flags line from header
	/* fprintf(g_pfHist, "%s %03x %s %s %s\n", szHelpHdr, wOpt, 
		(wOpt & fProcedural) ? "Procedural" : "",
		(wOpt & fConceptual) ? "Conceptual" : "",
		(wOpt & fExample)    ? "Example"    : "" ); */
	if (nVersion >= 1) {
		if (! ReadLine()
			 && (strncmp(g_line, szHelpHdr, strlen(szHelpHdr)) == 0) )
			 return FALSE;
		CString strFmt = CString(szHelpHdr) + " %x";			// sscanf format to use
		if (! sscanf(g_line, (LPCSTR) strFmt, &nHelpFlags) == 1)  // scanned 1 int
			return FALSE;
	}

	// save current flags and set help flags from log
	g_wOldHelpFlags = theApp.m_wHelpFlags;
	theApp.m_wHelpFlags = (USHORT) nHelpFlags;
	// Save async mode flag, and default it to off, the default used in older 
	// logs that didn't record it.
	// If newer log, it will be updated when statement is encountered at start of log
	g_nOldFeedbackMode = theApp.m_nFeedback;
	theApp.m_nFeedback = FEEDBACK_WAIT;

	return TRUE;
}

// GetLogLength - Determine length in seconds of the current log.
// We seek to a point before the end, read the tail of the file, and scan backwards
// in the tail to find the timestamp of the last line.

// Helper seeks to load file tail into buffer, returns nBytes read
PRIVATE int ReadTail(FILE* pf, char* pszBuf, int nBytes)
{
	if (!pf) return 0;
	int nRead = 0;
	
	// Save current position in file.
	fpos_t posOld;
	if (fgetpos(pf, &posOld) != 0)
		return FALSE;

	// Move the file pointer to given bytes before end (ignoring CRLF translation); 
	// !!!make sure this is less than file size?
	if (fseek(g_pfPlayback, -nBytes, SEEK_END) != 0) 
		return FALSE;

	// read from here into given buffer. Should have fewer than nBytes left
	// to read to EOF w/translation.
	nRead = fread(pszBuf, 1, nBytes, g_pfPlayback);
	
	// restore original file position. (clears EOF).
	if (fsetpos(g_pfPlayback, &posOld) != 0) {
		TRACE("Failed to restore log file position after seeking to end!\n");
	}

	return nRead;
}

// Read length into nLength, returns TRUE for success, 
PRIVATE BOOL GetLogLength(int& nLength)
{
	if (!g_pfPlayback) return FALSE;

	// Num should be large enough to ensure more than one line to
	// include newline before last log line. (Normal last line
	// is short END-LOG but might have crash log);
	const int TAIL_SIZE = 128;
	char szTail[TAIL_SIZE];
	int nRead = ReadTail(g_pfPlayback, szTail, TAIL_SIZE);
	if (nRead <= 0)
		return FALSE;
	szTail[nRead - 1] = '\0'; // clobber last char in log = final NL, making C String.
	// TRACE("Log file tail (%d bytes): |%s|\n", nRead, szTail);

	// scan back from end to find Newline before final line, and last TAB
	char* pPrevNL = strrchr(szTail, '\n');
	char* pLastTab = strrchr(szTail, '\t');
	if (! pPrevNL || ! pLastTab || (pPrevNL >= pLastTab)) // log no good at end!!
		return FALSE;

	// extract last line's timestamp string from between NL and TAB (whitespace OK).
	const int TIME_SIZE = 16;
	char szTimeStamp[TIME_SIZE];
	int nLen = pLastTab - pPrevNL;
	if (nLen > (TIME_SIZE - 1))
		return FALSE;
	strncpy(szTimeStamp, pPrevNL, nLen);
	szTimeStamp[nLen] = '\0';
	
	// and convert it to get time in seconds
	if (ScanTime(szTimeStamp, &nLength) != 1)
		return FALSE;
	
	TRACE("Got Log length = %d\n", nLength);
	return TRUE;
}

// Player state manipulation:

// Set following to get code to disable mainframe window during log playback.
// This is safe, but it means we get slight differences during playback. Mainly,
// can't set focus to a child edit control like an equation, answer box, or dialog
// control, so don't see any sign of where focus is, such as blinking caret.
// Clear this flag to experiment with alternate method of ignoring user input by
// filtering it in App's PreTranslateMessage. Note there are still some differences 
// with the alternate method, e.g. the caret is shown in the focus edit control
// but doesn't move from beginning on text changes, since we didn't log changes in 
// it's position. But that is still preferable since it shows focus control.
#ifndef LOG_DISABLES_MAINFRAME			// set from outside, e.g. build options
#define LOG_DISABLES_MAINFRAME	0
#endif 	 

//
// BeginPlayback: API to enter playback mode for given log file
//
// In theory recording history is independent of playing back session logs,
// and we could record a session during which other log files are played
// back. But for simplicity we just stop history recording during playback 
// of log files. In effect the app has a single global recorder which is in
// either playback or record mode. !!! Currently no way to re-enter record mode
//
PUBLIC void LogPlayerBegin(LPCTSTR pszPathName, IPlayerUI* pUI /*=NULL*/)
{
	// First make sure we can open log file
	if (pszPathName == NULL || pszPathName[0] == '\0' /* empty */ ||
		(g_pfPlayback = fopen(pszPathName, "r")) == NULL) {
		    CString strMsg;
			strMsg.Format("Couldn't open log file\"%s\"\n", (LPCTSTR) strFileName);
			AfxMessageBox(strMsg, MB_ICONERROR | MB_OK);
			return;		// early return on failure
	}
	g_strPathName = pszPathName;	// remember pathname of open file.

	// For now, we end log recording when enter playback mode, w/no way to resume. 
	// Might want to keep it on for possible trace output which might be useful
	// for release version debugging. However without further work it will not have a 
	// complete record of replayed events, rather mainly side-effect trace messages, 
	// since many events are replayed by different code from user event handlers that log.
	// We also have to end audio recording, so as not to hog the sound device.
	if (g_pfHist != NULL) {
		Logf("Play-log %s", pszPathName);
		HistoryFileEnd();
	}

#if LOG_DISABLES_MAINFRAME
	// Disable main window, so no user input interferes
	g_bWasDisabled = AfxGetMainWnd()->EnableWindow(FALSE);
#endif 
	// Initialize log playback state, but for time
	g_nLine = 0;

	// Log length is unknown until below. 
	g_nLogLength = -1;

	// save the UI to notify on changes
	g_pPlayUI = pUI;

	// Clear goto mode on start. (We leave FF flag in place)
	g_nGotoTime = -1;
	g_bStepping = FALSE;

	// Reset state flag for new problem:
	g_bInInitCheck = FALSE;

	// verify log header and get header parameters.
	if (! CheckHdr() )
		AbortPlayback();

	// try to obtain the log length
	if (! GetLogLength(g_nLogLength))
		g_nLogLength = -1;
	
	// Load up first event line 
	if (! GetNextEvent())
		AbortPlayback();

	// Build name of parallel audio log (same path with diff ext)
	CString strAudioPath = g_strPathName;
	strAudioPath.Replace(szLogExt, ".wav");

	// Test if parallel audio log exists .
	CFileStatus fileInfo;
	if (CFile::GetStatus(strAudioPath, fileInfo))
	{
		// try to open it
		CString strCommand;
		strCommand.Format("open \"%s\" type waveaudio alias playback", strAudioPath);
		if (! DoMciSendString(strCommand)){
			g_bPlayingAudio = TRUE;		// we are playing audio
			// need to set time format for seeking
			DoMciSendString("set playback time format milliseconds");
			// might be desirable to use cue command to setup for playing
		} else
			TRACE("Couldn't open audio log\n");
	}
    
	// 
	// Now marks the time we started playing a segment of the log:
	//
	g_timePlayStart = CTime::GetCurrentTime(); // real time we started playing

#if 0 /* don't do this while testing audio playback */
	// Start log playback clock at log time of current, i.e. first, event.
	// effectively skipping forward over initial empty segment of log time.
	// !!! this may skip part of the audio log !
	g_nLogPlayStart = g_nEventTime;
	// seek to same point in audio, if playing
	if (g_bPlayingAudio)
		/* send appropriate seek command */;
#else
	g_nLogPlayStart = 0;
#endif 
	// if playing normally, start playing the audio now
	if (! g_bFastForward && g_bPlayingAudio) {
		if (DoMciSendString("play playback")) {
			DoMciSendString("close playback", TRUE);
			g_bPlayingAudio = FALSE;
		}
	}

	// now we're playing: so update UI to display state change
	if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI();

	// Treat now as first timer tick: run any events that are ready
	// routine will set playback timer for subsequent checks.
	PumpReadyLogEvents(AfxGetMainWnd());
}

//
// LogPlayerInPlayback: API to test if currently in midst of a log 
// playback.
// Note this is true even if playback is paused.
//
PUBLIC BOOL LogPlayerInPlayback()
{
	return (g_pfPlayback != NULL);
}

PUBLIC void LogPlayerSetUI(IPlayerUI* pUI)
{
	g_pPlayUI = pUI;
}


// EndPlayback: worker routine to change player state.
// Fires additional UI notificiation if normal as opposed to abort termination.
// Note might be used in a deeply nested state, e.g. if a syntax error
// occurs. Should use exception handling to unwind stack, but for now
// suffices to end playback mode, which should cause everything
// to return back up the stack. 
PRIVATE void EndPlayback(BOOL bFinishedOK = FALSE)		
{ 
	if (!LogPlayerInPlayback()) return;	// no-op if not playing

	// stop audio if it was playing
	if (g_bPlayingAudio) {
		DoMciSendString("stop playback", TRUE);
		DoMciSendString("close playback", TRUE);
		g_bPlayingAudio = FALSE;
	}

	if (g_pfPlayback) 
		fclose(g_pfPlayback);
	g_pfPlayback = NULL;
	g_strPathName.Empty();

	if (g_pDlg != NULL) // ending playback mode while in modal dialog
	{
		KillPlaybackTimer(g_pDlg);
		// Must either take it down or enable it so user can ultimately dismiss it. 
		// Here we enable it.
		g_pDlg->EnableWindow(TRUE);
		// although dialog is still up, we clear g_pDlg -- var is only valid
		// while in playback mode.
		g_pDlg = NULL;
	}
	KillPlaybackTimer(AfxGetMainWnd());

	// reset player state flags
	g_bPaused = FALSE;
	g_bFastForward = FALSE;
	g_nGotoTime = -1;				// clears goto mode

	// re-enable frame 
#if LOG_DISABLES_MAINFRAME
	if (!g_bWasDisabled)			// restore mainframe enabled state
		AfxGetMainWnd()->EnableWindow(TRUE);
#endif
	// restore other saved application parameters
	theApp.m_wHelpFlags = g_wOldHelpFlags;
	theApp.m_nFeedback = g_nOldFeedbackMode;

	// Notify UI that player has changed state. (either way).
	if (g_pPlayUI) {
#if 0	// better to leave last event up so can find it if error
		// make display reset to time zero, to show can't resume at this point.
		g_pPlayUI->SetPlaybackTime("00:00:00");
		g_pPlayUI->SetProgress(0);
		g_pPlayUI->ShowNextEvent(""); 
#endif 0
		g_pPlayUI->UpdatePlayerUI();
	}

	// If completed normally, Notify UI that playback process has finished.
	if (bFinishedOK && g_pPlayUI) 
		g_pPlayUI->NotifyFinished();
}

// AbortPlayback -- internal routine to report fatal error and quit
PRIVATE void AbortPlayback()		
{
	EndPlayback(FALSE);

	// popup message box to report fatal error
	CString strMsg;
	strMsg.Format("Unrecoverable error processing log file line %d:\n    %s\nQuitting playback",
		g_nLine, g_line);
	AfxMessageBox(strMsg, MB_ICONERROR | MB_OK);
}

//
// LogPlayerStop() -- client API to end playback mode
//
// Safe to call if not in playback, just to ensure player is stopped.
PUBLIC void LogPlayerStop()
{
	EndPlayback(FALSE);	// this is an "abort" stop, not a normal end
}

// 
// LogPlayerFF(  -- gets FastForward flag state
// LogToggleFF() -- API to toggle FastForward flag
//
PUBLIC BOOL LogPlayerFF() { return g_bFastForward; }
PUBLIC void LogPlayerToggleFF()
{
	g_bFastForward = !g_bFastForward;	// toggles current value
	TRACE("Player FF = %d\n", g_bFastForward);
	
	// if going into FF mode, suspend audio playback
	if (g_bFastForward) 
	{
		if (g_bPlayingAudio)
			// Don't report mci error, probably harmless redundant pauses.
			DoMciSendString("pause playback", /*bSilent=*/ TRUE); 
	} 
	else // coming out of FF mode
	{
		// audio playback will be resumed on next tick
	}
	
	if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI();
}

//
// LogPlayerGoto -- FF to a specified time
// LogPlayerInGoto -- true if in goto time mode
//
PUBLIC void LogPlayerGoto(UINT nSecs)
{
	// make sure time is greater than current time
	if (nSecs <= UINT(g_nLogTimeElapsed))
		return;	// fails silently

	// else mark goto time and set FF mode
	g_nGotoTime = nSecs; 
	g_bFastForward = TRUE;
	TRACE("Player FF'ing to %%02d:%02d:%02d\n",
		g_nGotoTime/3600, (g_nGotoTime%3600)/60, g_nGotoTime%60);

	// suspend audio  during skip search. Will end paused, so starts on resume
	// Don't report mci error since likely due to harmless multiple pauses.
	if (g_bPlayingAudio)
		DoMciSendString("pause playback", /*bSilent=*/ TRUE);	

	// !!! should also start playing now, in case we are paused or idle haven't started
	// As is, it just sets mark at end time, still need to hit resume or play to run
	if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI(); 
}

PUBLIC BOOL LogPlayerInGoto()
{
	return g_nGotoTime >= 0;	// represented by non-neg goto time.
}

// Save a snapshot file with current state, for later printing or viewing.
PUBLIC void LogPlayerSnapshot()
{
	// We silently save file in the logged-in student's solution space with filename
	//            <logfile> at <time>
	// where colons in time are replaced by dashes. 
	CFBDDoc* pDoc = theApp.GetCurrentProblem();
	if (! pDoc) return;

	// Build filename to save under
	int iName = g_strPathName.ReverseFind('\\') + 1;
	if (iName == -1) return;
	CString strLogName = g_strPathName.Mid(iName, g_strPathName.ReverseFind('.') - iName);
	CString strTime = TimeToHms(g_nLogTimeElapsed);
	strTime.Replace(":", "-");
	CString strSnapName = strLogName + " at " + strTime + ".fbd";

	CString strSolnDir = g_strAndesDir + g_szProblemDir + "\\" + g_szSolutionDir;
	CString strSaveDir = strSolnDir + "\\" +  theApp.m_strUserName;
	::CreateDirectory(strSaveDir, NULL);	// ensure exists
	CString strSnapPath = strSaveDir + "\\" + strSnapName;
	
	// If dialog is up, ask dialog to do it. It knows how to include annotations
	// describing the dialog state in the saved file.
	if (g_pDlg && g_pDlg->IsKindOf(RUNTIME_CLASS(CDrawObjDlg))) {
		((CDrawObjDlg*)g_pDlg)->SaveSnapshot(strSnapPath);
		return;
	}
	
	// use DoSave worker routine, not DoSaveCopyAs, since latter prompts user.
	pDoc->DoSave(strSnapPath, FALSE);
}

// "Single-step": fast forward, stopping at to next transcriber event in log.
// Implemented as a special sort of GOTO/FF mode
PUBLIC void LogPlayerStep()
{
	g_bStepping = TRUE;
	g_bFastForward = TRUE;	// might want to save old state
	TRACE("Stepping to next transcript event\n");

	// start running if paused
	if (g_bPaused)
		LogPlayerPauseResume();	// will update UI itself.
	else if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI();
}

PRIVATE void EndStep()  // call at end of step
{
	g_bStepping = FALSE;
	g_bFastForward = FALSE; // might want to restore old state
	// ensure playback is paused 
	if (! g_bPaused)
		LogPlayerPauseResume();
}

// table of event names on which to break when skipping
static const char* g_BreakAfterEvents[] = 
{
	// CEQView
	"EQ-RESULT",
	"EQ-F",		
	"EQ-SUBMIT", 
	"EQ-Whatswrong", 
	"EQ-Calculate",
	"EQ-RESULT", 

	// CFBDView
	"Delete",	
	"Mark",		
	// for answer boxes:
	"Ans-enter", 
	"Ans-submit",
	// Help
	"FBD-Whatswrong",
	// App-level dialog popup events from CFBDView: 
	"System-dlg", 
	"Radius-dlg", 
	"Vector-dlg", 
	"Force-dlg", 
	"Motion-dlg",
	"Component-dlg", 
	"Axes-dlg", 
	"Text-dlg", 
	"Angle-dlg",
	"Ruler-dlg",	
	
	//CVarView
	"New-Variable",		
	"Modify-Variable",	
	"Delete-Variable",  
	"Variable-Whatswrong", 
	// App-level dialog popup events from CVarView: (usually ignored).
	"Declare-Variable-dlg", 

	// App-level events logged by CFBDDoc:
	"Open-Andes-Doc",  // open by andes-dir-relative name
	"Open",			// open file by full pathname
	"Close",		// close current document
	// App events logged by mainframe:
	"CLOSE-APP",
	// App Help request commands:
	"Help-Hint",	
	"Textbook",		// ?? needed	
	// Hint pane
	"Help-How",		
	"Help-Explain",	
	"Help-Why",		
	"Hint-Hide",	
	// App-level message boxes (actually dialogs) logged by the app
	"Instruct-MsgBox", 
	"Warning-MsgBox", 
	// App-level events logged by HelpIfc 
	"DDE-RESULT",	

	// Mini-leson-browser dialog events
	"Navigate-to",	
	"Go", 
};
#define NBREAKEVENTS (sizeof(g_BreakAfterEvents)/sizeof(g_BreakAfterEvents[0]))

// Check if current global event is a break event for FF/Step mode
PRIVATE BOOL BreakAfterEvent()
{
	// pull event name out of cmd string
	char szName[64];
	sscanf(g_pCmd, "%s", szName);

	// lookup in table
	for (register int i = 0;  i < NBREAKEVENTS; ++i) {
		const char* pszEvent = g_BreakAfterEvents[i];
		if (strcmp(szName, pszEvent) == 0)	
			return TRUE;
	}
	return FALSE;
}

// Check if need to end FF/Step mode
// This should be inserted before advancing with GetNextEvent, so as to be
// called *after* a event has just been played.
// Can't do it in DDE modal loop recreation, however, which is not interruptible.
PRIVATE void CheckForBreakAfter()
{
	if (g_bStepping && BreakAfterEvent())
		EndStep();
}

// special check of *next* on-deck event to break *before* dialog ending event
PRIVATE void CheckForBreakBefore()
{
	if (g_bStepping) {
		char szName[64];
		sscanf(g_pCmd, "%s", szName);
		if (_stricmp(szName, "BTN-CLICK") == 0) {
			char* pszBtnLabel = strrchr(g_pCmd, ' ') + 1;
			if ((_stricmp(pszBtnLabel, "Ok") == 0)
				|| (_stricmp(pszBtnLabel, "Cancel") == 0)) 
				EndStep();
		}
	}
}

// Wrapper around GetNextEvent checks for break out of step mode
PRIVATE BOOL GetNextEventCheck()
{
	CheckForBreakAfter();

	if (! GetNextEvent())	// EOF
		return FALSE;

	CheckForBreakBefore();

	return TRUE;
}

//
// LogPauseResume -- API to suspend or resume playback process
// LogPlayerPaused -- gets paused flag
//
PUBLIC void LogPlayerPauseResume()		// pause or resume playback
{
	if (!LogPlayerInPlayback()) return;

	if (! g_bPaused) // Pausing
	{
		TRACE("Player Pausing\n");
		// Setting flag suffices to suppress setting of playback timer
		// Note user can't get control to pause while pumping all events in current tick
		g_bPaused = TRUE;
		
		// Pause audio playback if playing. 
		// Don't report mci error since likely due to harmless multiple pauses.
		if (g_bPlayingAudio) {
			DoMciSendString("pause playback", /*bSilent=*/ TRUE);
		}
		if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI();
	}
	else // resuming from paused state
	{
		TRACE("Player Resuming\n");
		g_bPaused = FALSE;

		// update synch of R/T playback and log time
		// elapsed log time counter should currently be at start of last tick we 
		// actually played -- it gets incremented at start of each tick's playback,
		// which should be uninterruptible.
		// (Means in case of goto time, we really goto *end* of specified time tick ?)
		// we increment it by one here to treat now as start of next tick. 
		// We will simulate the first tick here by calling PumpReadyLogEvents, which
		// should should see zero real-time seconds elapsed since playback began, so won't 
		// increment elapsed time (but will still play any ready events).
		g_nLogPlayStart = g_nLogTimeElapsed + 1;
		g_timePlayStart = CTime::GetCurrentTime();
		
		// if playing normally (not FF), resume the audio playback as well 
		if (g_bPlayingAudio && ! g_bFastForward) {
			// synch audio stream to start of tick
			CString strCmd;
			strCmd.Format("seek playback to %d wait", g_nLogPlayStart * 1000); 
			DoMciSendString(strCmd);		// leaves device in stopped state
		
			DoMciSendString("play playback");
		}

		if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI();

		// Treat now as first timer tick of segment: run any events that are ready
		// routine will set playback timer for subsequent checks.
		PumpReadyLogEvents(g_pDlg? g_pDlg : AfxGetMainWnd());
	}
}

PUBLIC BOOL LogPlayerPaused() { return g_bPaused; }

// 
// For replaying calls to help system: bCallHelp defaults to FALSE, meaning we
// simulate help system calls from the log file. If true, the help system will actually
// be called again during log playback (may be useful for using logs as test scripts).
// HelpIfc has been instrumented to call back to these routines.
//
PUBLIC void LogPlayerToggleCallHelp() 
{ 
	g_bCallHelp = !g_bCallHelp;
	TRACE("Player Callhelp = %d\n", g_bCallHelp);
	if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI(); 
}
PUBLIC BOOL LogPlayerCallHelp() { return g_bCallHelp; }

PRIVATE BOOL PlayEvent(LPCTSTR pszCmd = g_pCmd);// plays single current event

// GetHelpCallResult -- Simulate RPC to help system from log data. 
// Returns success or failure, filling buf with result string
// !!! doesn't simulate time waiting for result
PUBLIC BOOL LogPlayerGetHelpCallResult(char* buf)
{
	// need to scan forward in the event stream till hit entry giving result
	for (;;)			// loop till hit DDE-RESULT event or EOF
	{
		// Load into global "on deck" event vars. Note updates global "current" 
		// event time, which is normally in lookahead buffer until we advance after processing
		// should leave last event read there for advancing after handler returns.
		if (! GetNextEvent() ) {
			TRACE("GetHelpResult: hit end of log without finding result!\n");
			return FALSE;			// really, should be playback exception
		}

		// pull event name out of cmd string
		char name[64];
		sscanf(g_pCmd, "%s", name);

		// !!! should verify that we have a matching DDE call in the log file,
		// else playback processing doesn't match log sequence.

		if (strcmp(name, "DDE-FAILED") == 0) {
			TRACE("Playback: reproducing DDE-FAILURE\n");
			return FALSE;	// simulate failure
		} else if (strcmp(name, "DDE-CANCEL") == 0) {
			TRACE("Playback: reproducing DDE-CANCEL as failure\n");
			return FALSE; // simulate failure
		}
		else if (strcmp(name, "DDE-RESULT") == 0) {
			// found result: extract result string from vertical-bar delimited arg
			// and copy into caller's buffer. 
			// (Buf should be large enough to hold any DDE result since helpifc allocated it).
			const char *pszArg = g_pCmd + strlen(name) + 1;
			int argLength = strlen(pszArg);
			if (pszArg[0] != '|' || pszArg[argLength - 1] != '|')
				TRACE("Log Playback: bad string arg in DDE-RESULT!\n");
			strncpy(buf, pszArg + 1, argLength - 2);
			buf[argLength - 2] = '\0';		// must add terminating NUL

			TRACE("Playback: reproducing DDE-RESULT %s\n", buf);
			return TRUE;
		} 
		else // hit logged event inside DDEML wait modal loop. 
		{
			// Play event --most ignored; mainly in case it is an asynch DDE result
			TRACE("GetHelpResult: replaying log line %s\n", g_line);
			PlayEvent(); // just ignore errors while playing in this loop
		}
	}
}

// 
// Pump user-interface events from queue and dispatch to handler.
// Use this to keep interface alive during FF loop, which doesn't yield.
// Code adapted from Prosise, p. 965 (without idle processing).
PRIVATE BOOL PeekAndPump()
{
	MSG msg;
	while (::PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE)) {
		if (!AfxGetApp()->PumpMessage()) {
			// Got WM_QUIT, needs to go to main message loop.
			::PostQuitMessage (0);
			return FALSE;
		}
	}
	return TRUE;
}

//
// PumpReadyLogEvents -- Fires all log events that are due to be run at 
//                     the current time
//
// This routine is in effect the main loop of a background task to manage
// the  playback process. Each timeout of our realtime clock wakes up the task to
// run for one tick. Normally it checks if the on-deck log event is ready to run
// and loops to fire that and all subsequent events due on this tick. Then it goes
// back to sleep till the next timeout wakes it up again. This means it attempts
// to catch up on every tick, in case it falls behind.
//
// /* In fastforward mode it just fires one event and sets the wakeup timer
// to fire immediately again. The timer is used to yield so that UI events can 
// be processed. */ Changed for greater FF speed: in FF mode it just loops firing
// events until the exit state (e.g. Goto time or break event if Stepping) is reached
// or the player is paused.
//
// Assumes on-deck event to be processed has already been pre-loaded into 
// the event lookahead buffer.
//
PRIVATE void PumpReadyLogEvents(CWnd* pWnd)	// Play all events due to play now
{
//	TRACE("PumpReadyLogEvents entered, Alarm wnd=%x\n", (int) pWnd);
top:
	if (!g_bFastForward)	// playing back normal speed
	{
		// Get real time elapsed in this normal speed playback segment
		CTime timeNow = CTime::GetCurrentTime();
		CTimeSpan spanElapsed =  timeNow - g_timePlayStart;
		
		// Update log playback time. (Should advance it one tick).
		g_nLogTimeElapsed = g_nLogPlayStart + spanElapsed.GetTotalSeconds();
		
	} 
	else // Fast forwarding:
	{	
		//TRACE("Pump/FF EventTime=%d GotoTime=%d\n", g_nEventTime, g_nGotoTime);
		// if skipping to a time, stop if next event is later than mark time. We play it 
		// if equal, since we are skipping to the *end* of the specified time tick.
		if (g_nGotoTime > 0 && g_nEventTime > g_nGotoTime) {
			
			// set current playback time to target time
			g_nLogTimeElapsed = g_nGotoTime;
			TRACE("Pausing at goto time %d (next event: %d)\n", g_nGotoTime, g_nEventTime); 
			
			// stop FF'ing and leave in paused mode
			g_bFastForward = FALSE;
			g_nGotoTime = -1;
			g_bPaused = TRUE; 
			if (g_pPlayUI) g_pPlayUI->UpdatePlayerUI();
			// can continue, should be no events to play on this tick
		} else {
			// normal FF: just set playback clock to next event
			g_nLogTimeElapsed = g_nEventTime;
		}
		// In FF/Step mode, we must exit mode *after* stop event is played. 
	}
	
	// Update playback clock on player control
	CString strTime;
	strTime.Format("%02d:%02d:%02d", g_nLogTimeElapsed/3600, (g_nLogTimeElapsed%3600)/60,g_nLogTimeElapsed%60);
	// ((CMainFrame*)theApp.m_pMainWnd)->SetPlaybackTime(strTime);
	if (g_pPlayUI) g_pPlayUI->SetPlaybackTime(strTime);

	// update progress indicator, if log length known
	if (g_nLogLength != -1 && g_pPlayUI) {
		double fRatio = double(g_nLogTimeElapsed) / g_nLogLength;
		g_pPlayUI->SetProgress(int(fRatio * 100));
	}

	// Note events can result in a message box which the user must dismiss in
	// order to continue. In this case further timeouts for future events
	// can still received by the application, which would trigger re-entrant
	// calls to this routine (with the same input event and the message box still
	// up!). To prevent this, we suspend the timer during execution and 
	// restore it when done
	KillPlaybackTimer(pWnd);
	// TRACE("PumpReadyLogEvents Disabling Alarm\n");

	// Loop to play *all* events whose time has come on this tick:
	// !!! UI events not pumped during this loop, so user can't interrupt 
	// loop through several events, e.g. with pause.
	while (g_bFastForward || g_nEventTime <= g_nLogTimeElapsed)
	{
		// Dispatch single current event to handler proc:
		// Note handler could put up a modal dialog, which will advance input stream past
		// this one, run modal loop which calls this re-entrantly to pump subequent events,
		// returning only after replaying dlg closing event, e.g BTN-CLICK OK, leaving it
		// to be consumed by our post-play processing. 
		TRACE("%s %x Pumping ev from log: %s\n", pWnd == g_pDlg ? "Dlg": "Frame", pWnd, g_line);
		if (! PlayEvent()) 
			AbortPlayback();
		
		// Check we didn't fail above and end playback mode 
		// !!! should use exception handling to unwind after failure
		if (!LogPlayerInPlayback()) 
			return;

		// If we were called from inside dialog, check if event handler terminated it.
		// if so, do no more. Leave current next event for handler on stack to consume
		if (g_pDlg != NULL && pWnd == g_pDlg && g_bEndDialog) {
			TRACE("Pump Loop: containing dialog ended -- returning\n");
			g_bEndDialog = FALSE;	// clear flag. 
			return;
		}

		// advance to load next event or quit on EOF
		// If FF/Step, pauses playback if just played BreakAfter or next is BreakBefore
		if (! GetNextEventCheck()) {
			TRACE("Pump loop: no more events, quitting\n");
			EndPlayback(TRUE); // finshed ok
			return;
		}

		// if player now paused, exit loop. !!!no way for user to pause in mid-tick.
		if (g_bPaused)
			break;

		// If in normal fast forward mode. exit loop after only one event to give
		// UI events a chance to be processed. If don't do this, playback is
		// *much* faster, but currently don't have good way to click pause in it,
		// (click is 2step process, but update on each tick redraws the button)
	/*	// In FF/Goto mode play as fast as possible to get to desired point.
		!!! last no good, loop skips goto exit test at top of routine */
		//	if (g_bFastForward /* && ! LogPlayerInGoto() */)
		//		break; 
		// In FF mode, check for UI events and just do it again if still playing
		// !! still difficult to hit pause during this loop
		if (g_bFastForward && PeekAndPump() && !g_bPaused) {
			goto top;
		}
	} // end loop over all events in tick

	// reactivate playback alarm
	SetPlaybackTimer(pWnd);		// no-op if paused
	// TRACE("PumpReadyLogEvents: reset alarm, exiting (no more ready events)\n");
}


// Dialogs call this when ending to set flag to break out of PumpReadyLogEvents
PUBLIC void LogPlayerNotifyDlgEnd()
{
	g_bEndDialog = TRUE;	// !!! could be member var of dialog class!!
	TRACE("LogDlg: flagging end of dialog to event pumper\n");
}

// Log-aware dialogs should call this on entry, saving prev dialog return val
// Even when not running from log, this is used to register "current" dialog
// so other code can find it. So must be safe if not running from log
PUBLIC CDialog* LogPlayerBeginModalDlg(CDialog* pDlg)
{
	TRACE("LogDlg::DoModal entered new dlg %x %s ",(int) pDlg, pDlg->GetRuntimeClass()->m_lpszClassName);
		
	// save old playback context 
	CDialog* pOldDlg = g_pDlg;
	if (pOldDlg != NULL) 
		TRACE("Pushed dlg %x %s\n", (int) pOldDlg, 
									pOldDlg->GetRuntimeClass()->m_lpszClassName);
	else
		TRACE("No prev dlg\n");

	// update player's current dialog pointer.
	g_pDlg = pDlg;
	// reset end dialog flag.  ?? OK in case of nested dialogs?
	g_bEndDialog = FALSE;
		
	// Urgh: must advance event stream ptr to next event so re-entrant event pump 
	// will find it. !!! Should recode to do before pumping, not after, like removal 
	// from msg queue for processing.
	if (LogPlayerInPlayback())			// only do this if actually running from log
	{
		if (! GetNextEventCheck() )
				AbortPlayback();
	}

	// return prev dialog; should be restored on end
	return pOldDlg;
}

PUBLIC CDialog* LogPlayerGetCurrentDlg()
{
	if (g_pDlg != NULL)
		ASSERT_KINDOF(CDialog, g_pDlg);
	return g_pDlg;
}

// Log-aware dialogs should call this after exit, passing saved prev dialog
// Even when not running from log should be used to register "current" dialog
// so other code can find it. So must be safe if not running from log
PUBLIC void LogPlayerEndModalDlg(CDialog* pOldDlg)
{
	TRACE("LogPlayer: exiting modal dlg, Popping to pOldDialog = %x\n",  pOldDlg);
	
	g_pDlg = pOldDlg;			// restore old playback context */
	if (g_pDlg != NULL)			// popped to a previous dialog
	{
		// TRACE("LogModalDlg: Popped dialog %x back into g_pDlg. Returning\n", (int) g_pDlg);
		// EndDialog flag will be reset on breaking out of ProcessEvents loop
		// !!!!! Also need to restore timer state? 
		// (Shouldn't need to advance input pointer on pop)
	} 
#if LOG_DISABLES_MAINFRAME
	// MFC always enables parent window after modal dialog. Reset main window
	// to disabled so user won't interfere during log playback.
	AfxGetMainWnd()->EnableWindow(FALSE);
#endif
}


//---------------------------------------------------------------------------
// Event dispatcher: hands event to right module for playback:
//--------------------------------------------------------------------------- 

// 
// g_AppHandler -- Dispatcher object for app-level events:
//
// Implemented here because some app events, esp file opens and closes, might 
// have to be treated specially in the context of log playback.
// !! Turns out most easily could be moved into app object.
class CAppEventHandler : public IEventHandler
{
	virtual BOOL DispatchEvent(EventID nEvent, LPCTSTR pszArgs);
	// top-level PointToObject dispatches to contained obj's PointToObject
	virtual void PointToObject(LPCTSTR pszArgs);
};

PRIVATE CAppEventHandler g_AppHandler;

// 
// g_evmap: for dispatching events to the proper handlers:
// follwing table maps event name strings to handler object and event codes
//

// Following codes which object provides the event handler that knows
// how to replay this event. Events requiring disambiguation are handled
// by this module, as are app-level events.
//
enum HandlerID 
{
	EVH_APP,			// APP object, implemented in this module
	EVH_MAINFRAME,		// Mainframe window
	EVH_EXVIEW,			// The example view
	EVH_EQVIEW,			// The equation view
	EVH_FBDVIEW,		// The FBD (diagram) view
	EVH_PLANVIEW,		// The Plan view
	EVH_VARVIEW,		// The Variable window
	EVH_HINTVIEW,		// The help message pane
	EVH_CHATVIEW,		// The discourse window
	EVH_DIALOG,			// The current dialog box
	EVH_IGNORE,			// for trace msgs currently ignored
	EVH_SPECIAL,		// ambiguous, require special handling to dispatch
	EVH_TABVIEW,		// The TabView
	EVH_HILEVELVW,		// The High level solution view
	EVH_PRINCVIEW,		// The view for choosing physics principles
};

typedef struct 
 {
	char *	  name;				// character string event name used in log
	HandlerID handler;			// int code for object to handle it
	EventID   id;				// event code
} EvInfo;						// Event info table record

PRIVATE const EvInfo g_evmap [] =
{
	// Generic dialog box events (most common):
	"F",		EVH_SPECIAL,	EV_FOCUS,	// amb, was used for eq focus in past
	"C",		EVH_DIALOG,     EV_CTL_CHANGE,
	"c",		EVH_DIALOG,		EV_RICHEDIT_CHANGE,	// newer short name preferred
	"Change",   EVH_DIALOG,     EV_RICHEDIT_CHANGE, // older long name accepted
	"DRP",		EVH_DIALOG,     EV_CBO_DROP,
	"SEL",		EVH_DIALOG,		EV_CBO_SEL,
	"SOK",		EVH_DIALOG,     EV_CBO_SOK,
	"CLOSE",	EVH_DIALOG,     EV_CBO_CLOSE,
	"SEL-LIST",	EVH_DIALOG,		EV_LIST_SEL,
	"BTN-CLICK",EVH_DIALOG,		EV_BTN_CLICK,
	"Mv-dlg",	EVH_DIALOG,     EV_DLG_MOVE,
	// Dialog Help events
	"WhatsWrong-dlg", EVH_DIALOG, EV_DLG_WHATSWRONG,

	// CEQView events
	"EQ-F",		EVH_EQVIEW,		EV_EQ_FOCUS,	// newer replacement for "F"
	"K",		EVH_EQVIEW,		EV_EQ_KILLFOCUS,
	"E",		EVH_EQVIEW,		EV_EQ_CHANGE,
	"EQ-SUBMIT", EVH_EQVIEW,	EV_EQ_SUBMIT,
	"EQ-DELETE", EVH_EQVIEW,	EV_EQ_DELETE,
	"EQ-Whatswrong", EVH_EQVIEW, EV_EQ_WHATSWRONG,
	"S",		EVH_EQVIEW,		EV_EQ_STATUS,
	"EQ-Calculate", EVH_EQVIEW,	EV_EQ_CALCULATE,
	"EQ-RESULT", EVH_EQVIEW,	EV_EQ_RESULT,
	"EQ-SolveFor", EVH_EQVIEW,  EV_EQ_SOLVEFOR,

	// CFBDView
	"L",		EVH_FBDVIEW,    EV_LBUTTON_DOWN,
	"M",		EVH_FBDVIEW,    EV_MOUSE_MOVE,
	"Up",		EVH_FBDVIEW,    EV_MOUSE_UP,
	"Select-tool",	EVH_FBDVIEW,	EV_SELECT_TOOL,
	"Begin-draw",	EVH_FBDVIEW,    EV_BEGIN_DRAW,
	"Begin-vector", EVH_FBDVIEW, EV_BEGIN_VECTOR,
	"Begin-Resize",	EVH_FBDVIEW,    EV_BEGIN_RESIZE,
	"Begin-move",	EVH_FBDVIEW,    EV_BEGIN_MOVE,
	"Click-bg",	EVH_FBDVIEW,    EV_CLICK_BG,
	"Vector",	EVH_FBDVIEW,    EV_OBJ_VECTOR,
	"Axes",		EVH_FBDVIEW,    EV_OBJ_AXES,
	"System",	EVH_FBDVIEW,    EV_OBJ_SYSTEM,
	"Radius",	EVH_FBDVIEW,	EV_OBJ_RADIUS,
	"Angle",	EVH_FBDVIEW,	EV_OBJ_ANGLE,
	"Object",	EVH_FBDVIEW,    EV_OBJ_OTHER,		// misc graphic object
	"Ruler",	EVH_FBDVIEW,	EV_OBJ_OTHER,		// MD ruler -- can treat as "other"
	/* "Text",  EVH_APP,		EV_OBJ_TEXT, */
	"RemSel",	EVH_FBDVIEW,    EV_REMOVE_SEL,
	"Select",	EVH_FBDVIEW,    EV_SELECT,
	"Deselect",	EVH_FBDVIEW,    EV_DESELECT,
	"Delete",	EVH_FBDVIEW,    EV_DELETE,
	"Del",		EVH_FBDVIEW,	EV_DELETE,			// alternate used in some versions
	"Deleted",	EVH_IGNORE,		EV_DELETED,			// trace msg
	"Dup",		EVH_FBDVIEW,    EV_DUPLICATE,
	"Obj",		EVH_FBDVIEW,    EV_OBJECT,			// generic, used after duplicate.
	"Menu",		EVH_FBDVIEW,    EV_FBD_MENU,
	"Mark",		EVH_FBDVIEW,    EV_MARK_VECTOR,
	"Edit-props", EVH_FBDVIEW,	EV_EDIT_PROPS,
	"Edit-vec-type", EVH_FBDVIEW, EV_EDIT_VEC_TYPE,
	"ZDir",		EVH_FBDVIEW,	EV_SET_ZDIR,
	// for answer boxes:
	"Ans-enter", EVH_FBDVIEW,	EV_ANSWER_FOCUS,
	"Ans-exit",	EVH_FBDVIEW,	EV_ANSWER_KILLFOCUS,
	"Ans",		EVH_FBDVIEW,	EV_ANSWER_CHANGE,
	"Ans-submit",EVH_FBDVIEW,	EV_ANSWER_SUBMIT,
	"Ans-status", EVH_FBDVIEW,	EV_ANSWER_STATUS,
	// for multiple choice buttons
	"Choice-click", EVH_FBDVIEW, EV_CHOICE_CLICK,
	"Choice-select", EVH_FBDVIEW, EV_CHOICE_SELECT,
	"Choice-check",	EVH_FBDVIEW, EV_CHOICE_CHECK,
	// Help
	"FBD-Whatswrong", EVH_FBDVIEW, EV_FBD_WHATSWRONG,
	// Training card input messages, currently only in FBDView
	"TCard", EVH_FBDVIEW, EV_FBD_TCARD_MSG,
		
	// property editing submissions logged by editing dialogs.
	"Vecprops", EVH_FBDVIEW,	EV_PROPS_VECTOR,
	"Forceprops", EVH_FBDVIEW,	EV_PROPS_FORCE,
	"Compoprops", EVH_FBDVIEW,	EV_PROPS_COMPO,
	"Axesprops",EVH_FBDVIEW,	EV_PROPS_AXES,
	"Sysprops", EVH_FBDVIEW,	EV_PROPS_SYSTEM,
	"Axesprops",EVH_FBDVIEW,	EV_PROPS_AXES,
	"Angprops",	EVH_FBDVIEW,	EV_PROPS_ANGLE,
	"Radprops", EVH_FBDVIEW,	EV_PROPS_RADIUS,
	"Varprops",	EVH_VARVIEW,	EV_PROPS_VARIABLE,
	
	//CVarView
	"Select-Variable",	EVH_VARVIEW,	EV_SELECT_VARIABLE,
	"New-Variable",		EVH_VARVIEW,	EV_NEW_VARIABLE,
	"Modify-Variable",	EVH_VARVIEW,	EV_MODIFY_VARIABLE,
	"Delete-Variable",  EVH_VARVIEW,	EV_DELETE_VARIABLE,
	"Variable-Menu",	EVH_VARVIEW,	EV_VARIABLE_MENU,
	"Variable-Whatswrong", EVH_VARVIEW, EV_VARIABLE_WHATSWRONG,

	// CPrincView
	"Princ-Add",	EVH_PRINCVIEW,	EV_PRINCIPLE_NEW,
	"Princ-Select", EVH_PRINCVIEW,	EV_PRINCIPLE_SELECT,
	"Princ-Modify",	EVH_PRINCVIEW,	EV_PRINCIPLE_MODIFY,
	"Princ-Delete",	EVH_PRINCVIEW,	EV_PRINCIPLE_DELETE,
	"Princ-Whatswrong", EVH_PRINCVIEW,	EV_PRINCIPLE_WHATSWRONG,
	"Princ-SetCheck",	EVH_PRINCVIEW,	EV_PRINCIPLE_SETCHECK,
	"Princ-Menu",	EVH_PRINCVIEW,	EV_PRINCIPLE_MENU,
	// Older name, for backwards compatibility:
	"Principle-Menu",	EVH_PRINCVIEW,	EV_PRINCIPLE_MENU,

	// App-level events logged by CFBDDoc:
	"Open-Problem", EVH_APP,	EV_OPEN_PROBLEM,	// open by id
	"Open-Example", EVH_APP,	EV_OPEN_EXAMPLE,	// open by id
	"Check-Entries", EVH_APP,	EV_CHECK_ENTRIES,
	"FBD-Entry",	EVH_FBDVIEW,	EV_FBD_ENTRY,	// logs an initial FBD entry
	"Var-Entry",	EVH_VARVIEW,	EV_VAR_ENTRY,	// logs an initial Variable entry
	"EQ-Entry",		EVH_EQVIEW,		EV_EQ_ENTRY,	// logs an initial equation entry
	"Answer-Entry", EVH_FBDVIEW,	EV_ANSWER_ENTRY,// logs an initial answer entry
	"Next-Id",	    EVH_APP,	EV_NEXT_ID,
	"Close",	EVH_APP,		EV_CLOSE,			// close current document
	// now obsolete:
	"Open-Andes-Doc", EVH_APP, EV_OPEN_ANDES_DOC,	// open by andes-dir-relative name
	"Open",		EVH_APP,		EV_OPEN,			// open file by full pathname (old)
	
	// App events logged by mainframe:
	"CLOSE-APP",	EVH_APP,	EV_CLOSE_APP,
	"CANCEL-CLOSE-APP", EVH_APP, EV_CANCEL_CLOSE,
	"LAUNCH-UPLOADER", EVH_IGNORE, EV_LAUNCH_UPLOADER,
	"App-activate",		EVH_APP,	EV_APP_ACTIVATE,
	"App-deactivate",	EVH_APP,	EV_APP_DEACTIVATE,

	// App Help request commands:
	"Help-Hint",	EVH_APP,	EV_HELP_HINT,
	"Help-Explain",	EVH_APP,	EV_HELP_EXPLAIN,
	"Help-How",		EVH_APP,	EV_HELP_HOW,
	"Help-Why",		EVH_APP,	EV_HELP_WHY,
	"Help-Cmd",		EVH_APP,	EV_HELP_LINK,
	"Textbook",		EVH_APP,	EV_SHOW_TEXTBOOK,
	"Comment",		EVH_APP,	EV_COMMENT,
	"Help-Review-Eqn",   EVH_APP,    EV_HELP_REVIEW_EQN,
	"Hide-Review-Eqn",     EVH_APP,    EV_HIDE_REVIEW_EQN,
	// Common to both hint panes:
	"Hint-Hide",	EVH_APP,	EV_HINT_HIDE,

	// ANDES Hint pane-specific operations
	"Hint-PopupDef",	EVH_HINTVIEW,	EV_HINT_POPUP,
	"Kill-Popup",	EVH_HINTVIEW,	EV_KILL_POPUP,

	// Atlas Discourse ("Chat") Pane-specific operations
	"Discourse",    EVH_CHATVIEW, EV_DISCOURSE_TEXT,
	"Unsent",		EVH_CHATVIEW, EV_UNSENT_CONTENTS,
	"Left-Unsent",    EVH_CHATVIEW, EV_LEFT_UNSENT,

	// Other dialog view events
	"Choose-quant", EVH_CHATVIEW, EV_QUANT_DEF,	// Chose quant type to define from menu
	"Choose-menu", EVH_CHATVIEW, EV_MENU_CHOICE, // Chose string from a menu 
	// Events in the PSM selection dialog:
	"PSM-select",  EVH_DIALOG,   EV_PSM_SELECT,
	"PSM-expand",  EVH_DIALOG,   EV_PSM_EXPAND,
		
	// App events relevant to logging:
	"Log-length",	EVH_APP,	EV_LOG_LENGTH,		// no longer used
	"Andes-Version", EVH_APP,	EV_ANDES_VERSION,
	"FBD-Version",  EVH_APP,	EV_FBD_VERSION,
	"Async-mode",	EVH_APP,	EV_ASYNC_MODE,
	"END-LOG",		EVH_APP,	EV_END_LOG,
	// For remote viewer connected to remote student workbench (trace/debug info):
	"!",	EVH_IGNORE,	EV_REMOTE_CMD,	// traces remote viewer command received
	// pseudo-event placed in cmd queue on remote close of event connection
	"SOCKET-CLOSE", EVH_APP,	EV_SOCKET_CLOSE,
	
	// App-level dialog popup events logged by CFBDView: (usually ignored).
	"System-dlg", EVH_APP,		EV_DLG_SYSTEM,
	"Radius-dlg", EVH_APP,		EV_DLG_RADIUS,
	"Vector-dlg", EVH_APP,		EV_VECTOR_DLG,
	"Force-dlg", EVH_APP,		EV_VECTOR_DLG,		// doesn't matter, ignored
	"Motion-dlg", EVH_APP,		EV_VECTOR_DLG,
	"Component-dlg", EVH_APP,	EV_VECTOR_DLG,
	"Axes-dlg", EVH_APP,		EV_DLG_AXES,
	"Text-dlg", EVH_APP,		EV_DLG_TEXT,
	"Angle-dlg", EVH_APP,		EV_DLG_ANGLE,
	"Ruler-dlg",	EVH_APP,	EV_DLG_RULER,
	"Vector-type-dlg", EVH_APP, EV_DLG_VECTYPE,
	// App-level dialog popup events logged by CVarView: (usually ignored).
	"Declare-Variable-dlg", EVH_APP, EV_VAR_DLG,
	// App-level dialog popup events logged by CHiLevleView: (usually ignored).
	"Property-dlg", EVH_APP,		EV_PROPERTY_DLG,
	"Formula-dlg", EVH_APP,		EV_FORMULA_DLG,
	"Principle-dlg", EVH_APP,	EV_DLG_PRINCIPLE,

	// App-level dialog popup event logged by app
	"Login-dlg",   EVH_APP,		EV_DLG_LOGIN,
	"Task-dlg",	   EVH_APP,		EV_DLG_TASK,
	// App-level message boxes (actually dialogs) logged by the app
	"Instruct-MsgBox", EVH_APP,	EV_DLG_INSTRUCT,
	"Warning-MsgBox", EVH_APP,	EV_DLG_WARNING,	

	// App-level events logged by HelpIfc 
	"DDE",		EVH_APP,		EV_DDE_EXEC,
	"DDE-RESULT",	EVH_APP,	EV_DDE_RESULT,
	"DDE-POST",		EVH_APP,	EV_DDE_POST,
	"DDE-disconnect", EVH_APP,	EV_DDE_DISCONNECT,
	"DDE-CANCEL",	EVH_APP,	EV_DDE_CANCEL,
	"DDE-FAILED",	EVH_APP,	EV_DDE_FAILED,
	"START-HELP",	EVH_APP,	EV_START_HELP,

	// App UI services which may be invoked via DDE from help system:
	// Note they may be fired from help system, not mainly from logs
	"DDE-COMMAND",	EVH_APP,	EV_DDE_CMD,
	"show-lesson",	EVH_APP,	EV_SHOW_LESSON,
	"show-dlg",	   EVH_APP,		EV_SHOW_RULE_QUERY,
	"msg",		   EVH_APP,		EV_MSG,
	"training-card", EVH_APP,	EV_SHOW_TCARD,
	"show-hint",	EVH_APP,	EV_SHOW_HINT,
	"show-demo",	EVH_APP,	EV_SHOW_DEMO,
	"open-browser", EVH_APP,	EV_OPEN_BROWSER,
	"close-browser", EVH_APP,	EV_CLOSE_BROWSER,
	"set-score",     EVH_APP,   EV_SET_SCORE,

	// for manipulating demo pointer "agent" in demo scripts:
	"Point-to",		EVH_APP,	EV_POINT_TO,
	"Demo-msg",		EVH_APP,	EV_DEMO_MSG,

	// Mini-leson-browser dialog events
	"Open-lesson", EVH_DIALOG,		EV_BROWSER_OPEN,
	"Close-lesson", EVH_DIALOG,		EV_BROWSER_CLOSE,
	"Navigate-to",	EVH_DIALOG,		EV_BROWSER_NAVIGATE,
	"Go", EVH_DIALOG,				EV_BROWSER_GO,	
	"Forward", EVH_DIALOG,			EV_BROWSER_FORWARD,
	"Back", EVH_DIALOG,				EV_BROWSER_BACK,

	// Dialog-specific button choices (obsolete since added LogBtn?)
	// CSystemDlg:
	"Sys-type", EVH_DIALOG,     EV_DLG_SYSTEM_TYPE,
	// CVectorDlg:
	"Vtype",	EVH_DIALOG,     EV_DLG_VECTOR_TYPE,
	// CTaskdlg
	"Task-select", EVH_DIALOG,		EV_TASK_SELECT,

	// CEXView events
	"V",		EVH_EXVIEW,		EV_VIEW_OBJ,
	"VD",		EVH_EXVIEW,		EV_VIEW_DIAGRAM,
	"ED",		EVH_EXVIEW,		EV_END_DIAGRAM,
	"VS",		EVH_EXVIEW,		EV_VIEW_SKETCH,
	"ES",		EVH_EXVIEW,		EV_END_SKETCH,
	"Sel-obj",  EVH_EXVIEW,		EV_SELECT_OBJ,
	"Desel-obj",  EVH_EXVIEW,	EV_DESELECT_OBJ,
	"Explain",	EVH_EXVIEW,		EV_EXPLAIN,
	// For logging initial objects, not needed anymore:
	"Mask",		EVH_EXVIEW,		EV_OBJ_MASK,
	"Text",		EVH_APP,		EV_OBJ_TEXT,// Also used in CFBDView

	//CPlanView
	"Select-Item",		EVH_PLANVIEW,	EV_SELECT_ITEM,
	"Edit-Item",		EVH_PLANVIEW,	EV_EDIT_ITEM,
	"Double-Click",		EVH_PLANVIEW,	EV_DBL_CLCK,
	"Insert-Nextitem",	EVH_PLANVIEW,	EV_INSERT_NEXTITEM,
	"Insert-Subitem",	EVH_PLANVIEW,	EV_INSERT_SUBITEM,
	"Plan-Menu",		EVH_PLANVIEW,	EV_PLAN_MENU,
	"Delete-Item",		EVH_PLANVIEW,	EV_DELETE,

	//CTabView
	"Switch-Tab",		EVH_TABVIEW,	EV_SWITCH_TAB,

	//CHiLevelVw
	"Click-PlanBtn",	EVH_HILEVELVW,	EV_PLANBTN_CLICK,
	"Click-PlanCell",	EVH_HILEVELVW,	EV_PLANCELL_CLICK,
	"Edit-Sought",		EVH_HILEVELVW,	EV_EDIT_SOUGHT,
	"Sel-PlanList",		EVH_HILEVELVW,	EV_PLANLIST_SEL,
	"MouseDown-Grid",	EVH_HILEVELVW,  EV_LBUTTONDOWN_GRID,
	"MouseUp-Grid",		EVH_HILEVELVW,	EV_MOUSEUP_GRID,
	"DblClk-Grid",		EVH_HILEVELVW,  EV_LBUTTONDBLCLK_GRID,
	"MouseMove-List",   EVH_HILEVELVW,  EV_MOUSEMOVE_LIST,
	"Show-Explanation", EVH_HILEVELVW,  EV_SHOW_EXP,
	"Hide-Explanation", EVH_HILEVELVW,  EV_HIDE_EXP,
	"Edit-Property",	EVH_HILEVELVW,  EV_EDIT_PROPERTY,
	"Edit-Direction",	EVH_HILEVELVW,  EV_EDIT_DIRECTION,
	"Edit-NextStep",	EVH_HILEVELVW,	EV_EDIT_NEXTSTEP,
	"HiLevelVw-Menu",	EVH_HILEVELVW,  EV_HILEVELVW_MENU,
};
#define NEVENTS (sizeof(g_evmap)/sizeof(g_evmap[0]))

// fetch info from table by name:
PRIVATE const EvInfo* LookupEvent(char* name)
{
	// linear search seems good enough
	for (register int i = 0;  i < NEVENTS; ++i) 
	{
		char * strp = g_evmap[i].name;
/*	if (_stricmp(name, strp) == 0) // case-insensitive to allow typos above */
		if (strcmp(name, strp) == 0)	// case-sensitive to distinguish Close/CLOSE
			return &g_evmap[i];
	}
	return NULL;
}

// map event code to character string name
PRIVATE const char* GetEventName(EventID id)
{
	// linear search seems good enough
	for (register int i = 0;  i < NEVENTS; ++i) {
		if (g_evmap[i].id == id)
			return g_evmap[i].name;
	}
	return NULL;
}

// map event handler code to actual object, implicitly cast to an IEventHandler*,
// returns NULL if specified handler not found.
// !!! Could be made into App-level method
PRIVATE IEventHandler* GetHandlerObj(HandlerID id, EventID event)
{
	switch (id) 
	{
	case EVH_APP: return &g_AppHandler;
		break;
	case EVH_EXVIEW: return theApp.GetEXView();
		break;
	case EVH_EQVIEW: return theApp.GetEQView();
		break;
	case EVH_FBDVIEW: return theApp.GetFBDView();
		break;
	case EVH_PLANVIEW: return theApp.GetPlanView();
		break;
	case EVH_VARVIEW: return theApp.GetVarView();
		break;
	case EVH_HINTVIEW: return theApp.GetHintView();
		break;
	case EVH_CHATVIEW: return theApp.GetChatView();
		break;
	case EVH_MAINFRAME: return theApp.GetMainFrame();
		break;
	case EVH_HILEVELVW: return theApp.GetHiLevelVw();
		break;
	case EVH_TABVIEW: return theApp.GetTabView();
		break;
	case EVH_PRINCVIEW: return theApp.GetPrincView();
		break;

	case EVH_DIALOG:
		if (g_pDlg == NULL) {
			// if modeless equation dialog is up, see if maybe this is event for it.
			if (theApp.GetMainFrame() && ::IsWindow(theApp.GetMainFrame()->m_dlgEqnReview.m_hWnd) 
				                      && theApp.GetMainFrame()->m_dlgEqnReview.IsWindowVisible()) 
			{
				if (event == EV_PSM_SELECT || event == EV_PSM_EXPAND || 
					event == EV_DLG_MOVE || event == EV_BTN_CLICK) 
					return (CLogDialog*) &(theApp.GetMainFrame()->m_dlgEqnReview);
			}
			TRACE("No dialog up to playback dlg event -- ignoring\n");
			return NULL;
		} 
		// if it's a browser dialog event, then could ensure it is open here.

		// Make sure dialog is one of ours, since we're downcasting it
		if (g_pDlg->IsKindOf(RUNTIME_CLASS(CLogDialog)) ) {
				CString strTitle; 
				g_pDlg->GetWindowText(strTitle);
				TRACE("PlayEvent: dispatching to dialog %s %x\n", strTitle, g_pDlg);
				return ((CLogDialog*)g_pDlg);
		}
		break;

	default: 
		TRACE("GetHandlerObj: Bad handler id %d\n", (int) id);
		break;
	}
	return NULL;
}

PRIVATE BOOL PlayEvent(LPCTSTR pszCmd)	// Dispatch event to object that can handle it
{
	HandlerID handler;		// code for handler obj

	// Split Command into event name + args
	char name[64];
	sscanf(pszCmd, "%s", name);
	const char * pszArgs = pszCmd + strlen(name) + 1; // just past space after name

	// Fetch event info from table by event name
	const EvInfo* evp;
	if ((evp = LookupEvent(name)) == NULL) {
		TRACE("Unknown Andes event: %s\n", pszCmd);
		// AbortPlayback();
		return FALSE;
	}

	// do nothing for trace/comment events marked for EVH_IGNORE in table.
	if (evp->handler == EVH_IGNORE)
		return TRUE;
	
	// Determine handler, disambiguating certain special cases
	if (evp->handler == EVH_SPECIAL) 
	{
		if (evp->id == EV_FOCUS)	// could be for dialog control or equation.
		{	
			if (g_pDlg != NULL)		// currently in a dialog -- assume its  a control	
				handler = EVH_DIALOG;
			else
				handler = EVH_EQVIEW;// !!! should ensure id arg is eqn index
		}
	} else
		handler = evp->handler;
	
	// Fetch object to invoke handler proc
	IEventHandler* pHandlerObj = GetHandlerObj(handler, evp->id);
	
	// and dispatch it:
	BOOL bSucceeded = TRUE;	// default true ignores errors
	if (pHandlerObj != NULL)
		bSucceeded = pHandlerObj->DispatchEvent(evp->id, pszArgs);
	else {
		TRACE("PlayEvent: Handler %d not found -- ignoring\n", int(handler));
	}

	return bSucceeded;
}

// 
// ExecuteCmd: Public routine to execute a command string at any time, used to handle
// DDE or script commands.
//
// Returns true on success, false on failure.
// !!! Do some nested failures call AbortPlayback?

PUBLIC BOOL ExecuteCmd(LPCTSTR pszCmd)
{
	TRACE("Executing command: |%s|\n", CString(pszCmd).Left(480)); // trace max = 512
	return PlayEvent(pszCmd);
}


///////////////////////////////////////////////////////////////////////////////////
// Event dispatcher for app level commands, those that affect global state
// and state of the playback environment such as open problems, dialog boxes.
///////////////////////////////////////////////////////////////////////////////////

//
// Helper for handling demo mode pointer -- must be able to
// find screen cooords of objects referenced in script command
//

//
// PointToObject: handle script command to move pointer to object
//
// First arg specifies containing object, 
// next token in arg string has contained item spec. (varies with container).
// Optional rest arg is message to show in pointer balloon.
// We have to dispatch to appropriate container object to interpret item
// spec and get screen coordinates for pointer.
// !!! Should be changed to use similar string to handler map as for events,
// Note may want to navigate down parts of some kind of composite object ref, 
// e.g. "frame.toolbar.cmd" or "fbdview.md1.pt1" 
void CAppEventHandler::PointToObject(LPCTSTR pszArgs)
{
	// split out first arg, for type of container 
	const char* pszRest = pszArgs;	// unparsed portion
	char szType[64];
	if (sscanf(pszRest, "%s", szType) != 1) return;
	pszRest +=  strlen(szType);	
	if (*pszRest != '\0')		// pts to EOS if next arg omitted
		pszRest += 1;			// skip one space to next arg

	// Special case: "Point-to nothing" is code to hide the pointer
	if (_stricmp(szType, "nothing") == 0) {
		theApp.GetMainFrame()->HidePointer();
		return;
	}

	// split out item ID before optional trailing msg arg
	char szObjID[64];
	if (sscanf(pszRest, "%s", szObjID) != 1) return;
	pszRest +=  strlen(szObjID);	
	if (*pszRest != '\0')			// pts to EOS if msg arg omitted
		pszRest += 1;				// skip one space to trailing arg
	
	// Find containing Andes object which can interpret objID (knows screen coords). 
	IEventHandler* pHandlerObj = NULL;

	if (_stricmp(szType, "diagram") == 0 ||
		_stricmp(szType, "page") == 0) // "page" synonym used for specifying pts
		pHandlerObj = theApp.GetFBDView();
	else if (_stricmp(szType, "eq") == 0)			// equation view objects 
		pHandlerObj = theApp.GetEQView();
	else if (_stricmp(szType, "dialog") == 0) // dialog controls
	{
		if (g_pDlg == NULL || 
			! g_pDlg->IsKindOf(RUNTIME_CLASS(CLogDialog)) ) {
			TRACE("Point-to: No LogDialog up to point to -- ignored");
			return;		// early exit
		}
		pHandlerObj = (CLogDialog*)g_pDlg;
	}
	else if (_stricmp(szType, "var") == 0) 
		pHandlerObj = theApp.GetVarView();
	else if (_stricmp(szType, "frame") == 0)
		pHandlerObj = theApp.GetMainFrame();
	else if (strcmp(szType, "hint") == 0)
		pHandlerObj = theApp.GetHintView(); 
	else if (_stricmp(szType, "toolbar") == 0) {
		// toolbars are children of containing frame, App's or MDIChild's
		pHandlerObj = theApp.GetMainFrame(); 
	}

	// Dispatch to container to move the pointer
	if (pHandlerObj != NULL)
		pHandlerObj->PointToObject(szObjID);

	// And show any message if attached
	if (*pszRest != '\0') {
		theApp.GetMainFrame()->DemoMsg(pszRest);
	}
}

BOOL CAppEventHandler::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{	
	// NB default is success, use early return for failure.
	// currently skipping unhandled events without error
	// includes task/login dialog events, and trace msgs
	char szArg[255] = "";
	CString strFullPath;
	switch (nEvent) 
	{
	default: // All other events currently ignored
		TRACE("AppEventHandler: ignored %s\n", GetEventName(nEvent));
		break;

	case EV_ASYNC_MODE:
		int bAsync;
		if (sscanf(pszArgs, "%d", &bAsync) == 1) {
			theApp.m_nFeedback = bAsync ? FEEDBACK_ASYNC : FEEDBACK_WAIT;
			// note log playback process should save/restore option around log
		}
		break;

	case EV_FBD_VERSION:
		// Format is  "mo day yr hr:min" w/2-digit yr, 24-hr clock, e.g. "8 5 98 24:30"
		// not used yet
		break;

	case EV_ANDES_VERSION:
		// Format of m_strVersion: N.N.N[.N] =
		// Academic Year (starting at 1) . Semester (Fall = 1, Spring =2,
		// Summer = 3) . release (starting with 0) [. minor number, if used ]
		// Save as int for easy comparison.
		if (!VersionStrToInt(pszArgs, g_nLogAndesVersion))
			TRACE("Logplayer: Bad version number in log: %s\n", pszArgs);
		break;

	case EV_OPEN_PROBLEM:
		if (sscanf(pszArgs, "%s", szArg) != 1) return FALSE;
		strFullPath = g_strAndesDir + g_szProblemDir + "\\" + szArg + ".fbd";
		TRACE("Replay: opening %s\n", (LPCTSTR) strFullPath);
		return theApp.OpenDocumentFile(strFullPath) != NULL;

	case EV_OPEN_EXAMPLE:
		if (sscanf(pszArgs, "%s", szArg) != 1) return FALSE;
		strFullPath = g_strAndesDir + g_szExampleDir + "\\" + szArg + ".fbd";
		TRACE("Replay: opening %s\n", (LPCTSTR) strFullPath);
		return theApp.OpenDocumentFile(strFullPath) != NULL;
		break;

	case EV_OPEN_ANDES_DOC:// recreate file open command, Andes-rel filename
		strFullPath =  g_strAndesDir + pszArgs;
		TRACE("Replay: opening %s\n", (LPCTSTR) strFullPath);
		return theApp.OpenDocumentFile(strFullPath) != NULL;
		break;

	case EV_OPEN:		// recreate file open command, absolute pathname (not portable)
		TRACE("Replay: opening %s\n", pszArgs);
		return theApp.OpenDocumentFile(pszArgs) != NULL;
		break;

	// Following recreate initial state. really document operators, but handled here 
	case EV_CHECK_ENTRIES: 
		// reproduce checking of initial entries w/ help system, for status results
		// theApp.GetDocument()->CheckInitEntries();
		// For now just ignore, log init state w/status *after* the resubmission.
		// But note we are in the init entry check for EV_DDE_FAILED below
		g_bInInitCheck = atoi(pszArgs);
		break;
	
	case EV_NEXT_ID:			// logged id generation counter value
		// reset counter here so when replay student entry object adding operations,
		// the same ids get generated on playback as were used when log was recorded.
		// Then these ids can still function as object references when included as 
		// args to other event records.
		// NB: Problem file loaded on playback may be different than one used at
		// log time. If problem graphics have been edited, the id counter in problem
		// file may now be higher than at log recording time, and this step on playback 
		// will decrease it down to where it was when student worked. Normally that 
		// won't cause any conflict, because  generated IDs include object type 
		// (e.g. Rectangle-45, Vector-46), and adding graphics doesn't normally add 
		// student entry object types like vectors and axes. However, in a few cases,
		// we use unnamed vectors as graphics just to display arrows in the diagram. 
		// In this case a conflict is possible. Not worrying about that for now. If
		// problems arise, just insist on replaying with same version of Andes system.
		theApp.GetDocument()->nNextID = atoi(pszArgs);
		break;

	case EV_DDE_RESULT:
		// in case we are remote viewing, must stuff the result in to break
		// helpifc out of its wait state.
		if (theApp.IsRemoteViewer() && ! g_bInInitCheck) {
			CString strResult(pszArgs);  // bracketed by vbars
			HelpIfcSetResult(strResult.Mid(1, strResult.GetLength() - 2));
		}
		break;

	case EV_DDE_FAILED:
		// in case we are remote viewing, must stuff the result in to break
		// helpifc out of its wait state.
		if (theApp.IsRemoteViewer() && ! g_bInInitCheck) {
			HelpIfcSetResult(NULL);
			break;
		}
		// ignore if happened while checking init entries. Else fatal mismatch:
		// GetHelpCallResult should have matched result to its call.
		if (! g_bInInitCheck) return FALSE;
		break;

	case EV_CLOSE: {	// reproduce file close command
		TRACE("Replay: close\n");
		CDocument* pDoc = theApp.GetDocument();
		if (pDoc == NULL) {
			TRACE("Couldn't find active document!\n");
			return FALSE;
		}
		// !!! following won't reproduce "save changes?" dialog and response,
		// which were not logged. Might want to ask user if wants to close
		pDoc->OnCloseDocument();
		} break;

	// case EV_CLOSE_APP:	// can happen before close, may be cancelled.z
	case EV_END_LOG:
		// in case running from remote log, this ends us.
		// assume problem closed on normal end. (could check).
		if (theApp.IsRemoteViewer()) {
			// empty buffer of any pending commands (may hold SOCKET-CLOSE).
			g_sockEvSink.DiscardMsgs();
			// done with the socket
			g_sockEvSink.ShutDown(2);
			g_sockEvSink.Close();
			// clearing this signals app that no longer remote viewing, allowing
			// user input events to get through again (in case they're needed)>
			theApp.m_strRemoteHost.Empty();
			// And post end of input event message to trigger app close 
			AfxPostQuitMessage(0);
		}
		break;
	case EV_DLG_LOGIN:{
#if 0
		// Student entries to fill in combo box are installation-dependent,
		// so just accepting default or choosing from list may not look the same on
		// replay. Doesn't matter since student isn't really logged in on replay.
		CLoginDlg dlg;	
		// need to init default user from registry
		dlg.DoModal();
#endif 0
		}break;

	case EV_DLG_TASK:{
		// Task dialog actually fires a command to app. Don't want to replay that,
		// since Open-Andes-Doc event will be replayed to reproduce the end result.
//		TRACE("Replay: Task dialog\n");
//		CTaskDlg dlg;
//		dlg.DoModal();
//		theApp.OnFileOpen();
		}break;

	// Following commands provide services that might be called via DDE by help system:
	case EV_MSG:
		theApp.DoInstructMessage(pszArgs);	// Style is informational message
		break;
	case EV_SHOW_LESSON:
		theApp.ShowLesson(pszArgs);
		break;
	case EV_OPEN_BROWSER:			// opens page modelessly in browser
		theApp.OpenBrowser(pszArgs);
		break;
	case EV_CLOSE_BROWSER:
		theApp.CloseBrowser();
		break;
	case EV_SHOW_RULE_QUERY:
		theApp.ShowRuleQuery(pszArgs);
		break;
	case EV_SHOW_TCARD:
		theApp.ShowTCard(pszArgs);
		break;
	case EV_SHOW_HINT:
		theApp.ShowHintDdeCmd(pszArgs);
		break;
	case EV_SHOW_DEMO:// handled by main frame worker routine
		strFullPath = g_strAndesDir + g_szDemoDir + "/" + pszArgs;
		theApp.GetMainFrame()->ShowDemo(strFullPath);
		break;
	case EV_DEMO_MSG://  handled in frame, which owns the dialog
		theApp.GetMainFrame()->DemoMsg(pszArgs);
		break;
	case EV_SET_SCORE: 
	{
		CFBDDoc * pDoc = theApp.GetCurrentProblem();
		if (pDoc == NULL) {
			TRACE("Couldn't find active document!\n");
			return FALSE;
		}
		pDoc->m_strScore = pszArgs;
	}   break;
		
	// replay logged DDE command from help system:
	case EV_DDE_CMD:
		theApp.OnDDECommand((LPTSTR)pszArgs);	// MFC doesn't declare this const
		break;

	// simple user help commands:
	case EV_HELP_HINT:	// replay simple command to frame
		AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_GET_PROC_HELP);
		break;
	case EV_SHOW_TEXTBOOK:	// replay simple command to frame
		AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_TEXTBOOK);
		break;
	case EV_HELP_HOW:	
		theApp.GetMainFrame()->SendMessage(WM_COMMAND, ID_FOLLOWUP_HOW);
		break;
	case EV_HELP_EXPLAIN: 
		theApp.GetMainFrame()->SendMessage(WM_COMMAND, ID_FOLLOWUP_EXPLAIN);
		break;
	case EV_HELP_WHY: 
		theApp.GetMainFrame()->SendMessage(WM_COMMAND, ID_FOLLOWUP_WHY);
		break;
	case EV_HINT_HIDE:
		theApp.GetMainFrame()->SendMessage(WM_COMMAND, ID_FOLLOWUP_HIDE);
		break;
	case EV_HELP_LINK:	// unused; for helpsys extensibility of link mechanism
		theApp.GetMainFrame()->DoHelpRequest(pszArgs);
		break;
	case EV_HELP_REVIEW_EQN:
		theApp.GetMainFrame()->SendMessage(WM_COMMAND, ID_PHYS_REVIEW_EQN);
		break;
	case EV_HIDE_REVIEW_EQN:
		theApp.GetMainFrame()->HideEqnReview();
		break;

	case EV_COMMENT: {		// show comment recorded
		CCommentDlg dlg;
		dlg.SetText(pszArgs);
		dlg.DoModal();
	} break;

	// for log playback module:
	case EV_LOG_LENGTH:
#if 0	// ignore, obsolete since now scan logs to determine length.
		// this is "header" info giving length of log in seconds (for demos, mainly)
		int nLength;
		if (sscanf(pszArgs, "%d", &nLength) == 1)
			g_nLogLength = nLength;
#endif 
		break;
	case EV_POINT_TO:
		PointToObject(pszArgs);
		break;

	//
	// Modal dialog appearances are logged as the dialogs are popped up with a
	// message like "system-dlg", "vector-dlg", etc. These are just trace msgs
	// since the dialog popup is always caused by some other event-handling
	// code, e.g.as a side-effect of another finishing drawing or resizing, or 
	// as a result of a user command (e.g. EditProperties) chosen from menu/toolbar/keyboard. 
	// In the first case, the replay handler for that operation will use the same
	// code and so also pop up the dialog as a side-effect, so there is no need to 
	// do anything special to startup the dialog the events. Hopefully, this
	// happens in all of the second cases as well, assuming the user command handler has 
	// logs and playbacks itself appropriately. So we ought never to have to actually
	// replay the dialog pop-up events. 	
	}
	return TRUE;
}


// comparison during log playback.
/////////////////////////////////////////////////////////////////////////////
// 
// CLogDialog: Subclass of CDialog that serves as base class for log-aware
// dialogs. Currently provides the following services on playback:
//
// + Sets up the playback timer on initialization to keep the dialog playback
// process going during popup of modal dialog
// + Sets flag to signal termination to ProcessLogEvent
// + Provides a generic DispatchEvent that knows how to replay events recorded
// by our generic logging control classes LogEdit and LogCombo. And dialog
// level events such as window moves.
//
// Specific dialogs will have to override DispatchEvent to replay specific
// dialog events. 
////////////////////////////////////////////////////////////////////////////
CLogDialog::CLogDialog(int id, CWnd* pParent /*=NULL*/)
	: CDialog(id, pParent)
{
	// Default init m_bShown flag here.
	m_bShown = FALSE;
	// Note persisting C++ obj may be re-used across Window creations/destructions for
	// repeated DoModal calls, as is done with experiment's Textbook dialog. So 
	// we also reset it to FALSE at beginning of DoModal below to be sure.  
}
	
IMPLEMENT_DYNAMIC(CLogDialog, CDialog)

BEGIN_MESSAGE_MAP(CLogDialog, CDialog)
	//{{AFX_MSG_MAP(CLogDialog)
	ON_WM_MOVE()
	ON_WM_HELPINFO()
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogDialog message handlers
//

// LogModalDialog: Public helper function used to wrap popup of modal dialog.
//
// Encapsulates CDialog::DoModal()
// During playback, notifies player to keep playback process going on entry and exit.
// (saves and restores g_pDlg pointer, resets playback timer).
//
extern int LogModalDlg(CDialog& dlg)	// Global func now obsolete.
{
	return dlg.DoModal();
}

int CLogDialog::DoModal() 
{
	int result;

	// reset first-show flag.
	m_bShown = FALSE;

	// Pre-modal: Adjust player state in case app is running from log file
	CDialog* pOldDlg = NULL;	// saves pointer to previous dialog, if any
	pOldDlg = LogPlayerBeginModalDlg(this);

	// Run the modal dialog as usual
 	result = CDialog::DoModal();
	
	// Post Modal: In case running from a log, restore playback state
	// Note might not be executed in case playback aborted inside dialog.
	LogPlayerEndModalDlg(pOldDlg);	
	
	return result;
}

BOOL CLogDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
 
	// In replay mode: start a playback timer again, linked to this dialog. 
	// (it was suspended around event dispatch to handler proc that popped us up)
	if (LogPlayerInPlayback()) 
	{
		SetPlaybackTimer(this); 
		TRACE("CLogDialog::OnInitDialog: re-enabling playback timer\n");
/*
		// This window should not be enabled for user input during playback
		// Note we must re-enable it (or take it down) it if it is still up when 
		// playback mode ends (e.g. if it aborts), else there will be no way for 
		// the user to dismiss it!
		EnableWindow(FALSE);
*/
	}
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// Note on logging of OK and Cancel operations:
// We get WM_COMMAND notifications for OK and Cancel if buttons are pushed;
// also from keyboard interface (return and Esc). Currently these are logged
// just in case we have LogBtns with the IDOK and IDCANCEL ids. They log them when 
// handling the reflected WM_COMMAND/BN_CLICK message. Evidently they get these
// reflected msgs even when the msg originates in the keyboard interface (!).
// Should probably be changed so that parent logs them, so we don't require 
// CLogBtns at all. Also, if dialog doesn't have a cancel button, then cancelling via
// escape key  won't be logged.
void CLogDialog::OnDialogEnd()
{
	if (LogPlayerInPlayback() ) // we are running from a log file playback
	{
		// set flag to make sure the PumpReadyLogEvents loop higher up on stack 
		// doesn't pump any more log msgs to the dialog object
		LogPlayerNotifyDlgEnd();
		KillPlaybackTimer(this);
	}
}

void CLogDialog::OnOK()
{
	OnDialogEnd();
	CDialog::OnOK();
}

void CLogDialog::OnCancel()
{
	OnDialogEnd();
	CDialog::OnCancel();
}

void CLogDialog::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CDialog::OnShowWindow(bShow, nStatus);

	// If first showing, record that we have been shown during this modal instance.
	// Used to avoid logging programmatic moves during initialization before shown.
	if (!m_bShown && bShow)
		m_bShown = TRUE;
	
}

void CLogDialog::OnMove(int x, int y) 
{
	CDialog::OnMove(x, y);
	
	// Suppress logging of programmatic moves during initialization (before shown).
	// One comes from Windows on creation before OnInitDialog entirely. 
	// A second comes either from our code placing dialog in OnInitDialog or
	// after OnInitDialog but before showing by MFC's auto-center feature.
	// These moves will be replayed by replaying the initialization code. 
	if (! m_bShown) 
		return;
	
    // Log dialog moves so we can tell what was visible on playback
	// Note the given point is left, top of CLIENT area (not whole window) 
	// in SCREEN coordinates. But there's no API to Set the client area to
	// a given position.
	
	// For maximum informativeness on playback, we log this position in both 
	// frame and FBDView coordinates as well. Probably most relevant is pos
	// relative to view, since frame might be in different place on playback.
	CPoint ptFrame(x, y);
	CWnd* pWndMain = AfxGetMainWnd();
	if (pWndMain) pWndMain->ScreenToClient(&ptFrame);

	CPoint ptView(x, y);
	CFBDView * pView = (CFBDView*) theApp.GetCurrentView(RUNTIME_CLASS(CFBDView));
	if (pView != NULL) {
		pView->ScreenToClient(&ptView);
	}

	LogEventf(EV_DLG_MOVE, "%d %d  V %d %d  F %d %d ", x, y, ptView.x, ptView.y,
			                          ptFrame.x, ptFrame.y);
}

//
// Log/script event support
//

// for mapping between printable names and IDs.
int CLogDialog::CtlNameToID(LPCSTR pszName)
{
	int nEntries = 0;
	const CtlInfo* pCtlTbl = GetCtlTbl(nEntries);	
	if (pCtlTbl == NULL) return -1;
		
	for (int i = 0;  i < nEntries; ++i) 
		if (_stricmp(pCtlTbl[i].szName, pszName) == 0) // be tolerant on case
			return pCtlTbl[i].nID;
	return -1;
}

LPCTSTR CLogDialog::CtlIDToName(int nID)
{
	int nEntries = 0;
	const CtlInfo* pCtlTbl = GetCtlTbl(nEntries);	
	if (pCtlTbl == NULL) return NULL;

	for (int i = 0;  i < nEntries; ++i) 
		if (pCtlTbl[i].nID == nID) 
			return pCtlTbl[i].szName;
	return NULL;
}

// Return character string name of control for use in log entries.
// This will be ctl name if it is in the dialog's control table, else formatted
// integer control id. 
// NB: returned value in integer case points into static buffer overwritten on each call,
// so cannot use more than once in an expression.
LPCSTR CLogDialog::GetCtlName(CWnd* pCtl)
{
	static char szID[32];	// buffer for returned string

	INT nID = pCtl->GetDlgCtrlID();
	// first look up control in dialog's control table
	LPCTSTR pszName = CtlIDToName(nID);
	if (pszName != NULL)
		return pszName;

	// not in table: just format integer ID;
	sprintf(szID, "%d", nID);
	return szID;
}

//
// Helper to parse ctl Id arg from parm string, 
// returns Control window ptr or NULL,
// Updates pszRest to point to rest of parm string
CWnd* CLogDialog::GetCtlArg(LPCTSTR pszArgs, const char* &pszRest)
{
	char szArg[64];
	int nCtlID;
	CWnd* pCtl = NULL;

	sscanf(pszArgs, "%s", szArg);			// first arg as string 
	pszRest = pszArgs + strlen(szArg) + 1;	// 1 past end of args
	
	// try to parse as an integer ctl id
	if (sscanf(szArg, "%d", &nCtlID) == 1)
		// pCtl = GetDlgItem(nCtlID);
        // Following MFC function recurses into subwindows, needed
		// in case of nested dialogs
		pCtl = GetDescendantWindow(m_hWnd, nCtlID, /*bOnlyPerm:*/FALSE);
	else // try parsing as control name from table
	{
		nCtlID = CtlNameToID(szArg);
		if (nCtlID != -1)
			pCtl = GetDlgItem(nCtlID);
	}
	
	if (pCtl == NULL)
		TRACE("Dlg control not found! %s\n", szArg);
	return pCtl;
}

BOOL CLogDialog::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{
	char * pszRest;	// for scanning string
	/* TCHAR lpClassName[32];  // ctl wndclass name -- not used now */
	CWnd* pCtl;
	int nItem;
	CRect dlgRect;
	CRect dlgClientRect;
	int x, y;
	int dx, dy;

	switch (nEvent)
	{
	// These are called inside a playback timeout handler for the dialog
	// box. Note we can call OnOK inside handler because it only sets flag 
	// for dialog box  to end after current message, but does not actually 
	// terminate it. So we can return success to the log event dispatcher, but
	// set a flag to communicate that the dialog is terminating, so no more
	// events should be processed in its loop (remember could be multiple
	// events in a single timeout handler.)
	

	case EV_DLG_MOVE:
		GetWindowRect(&dlgRect);//whole window of dlg
		GetClientRect(&dlgClientRect);//client area of dlg (0,0,width,height)
		ClientToScreen(&dlgClientRect);//client area in screen coordinates
		dx = dlgClientRect.left - dlgRect.left;//adjustments to be made
		dy = dlgClientRect.top - dlgRect.top;//to points passed to OnMove
		
		sscanf(pszArgs, "%d %d", &x, &y);
		MoveWindow(x-dx, y-dy, dlgRect.Width(), dlgRect.Height());
		break;
	
	case EV_FOCUS:			// also used for equations */
		if ( (pCtl = GetCtlArg(pszArgs, pszRest))  == NULL )
			return FALSE;
		pCtl->SetFocus();
		// !!! save into member as "logical focus" to retrieve when doing
		// GetFocus during log playback?
		break;

	case EV_CTL_CHANGE:
		// LogEventf(nEvent, "%d %s", GetDlgCtrlID(), (LPCTSTR) strText);
		if ( (pCtl = GetCtlArg(pszArgs, pszRest))  == NULL )
			return FALSE;
		pCtl->SetWindowText(pszRest);
		pCtl->UpdateWindow();
		break;

	case EV_RICHEDIT_CHANGE:
		// LogEventf(nEvent, "%d %s", GetDlgCtrlID(), (LPCTSTR) strText);
		if ( (pCtl = GetCtlArg(pszArgs, pszRest))  == NULL )
			return FALSE;
		((CRichEditEx*)pCtl)->SetRichEditText(CString(pszRest));
		pCtl->UpdateWindow();
		break;
// Best is to track CBO changes by string on SEL message, since index may change between
// versions. Unfortunately due to a typo old logs (versions before 2/19/99) log only 
// index and topindex on this, w/o string value. Following are possible sequences:
// If dropped: DROP SOK CLOSE SEL  if select w/mouse	(most common case)
//          or DROP SEL CLOSE      if select w/keyboard
//          or DROP CLOSE          if esc drop w/o selecting (val may be empty if unset)
// If undropped: get SOK SEL pairs if change w/keyboard  (rare, most users don't know how)
//
// So for old logs, could get string val from last CLOSE msg if dropped or last SOK msg if undropped,
// though would have to defer SEL playback till CLOSE for 2nd case above. 
// Note new logs do not log SOK at all.
	case EV_CBO_DROP:
		pCtl = GetCtlArg(pszArgs, pszRest);
		if (!pCtl || ! pCtl->IsKindOf(RUNTIME_CLASS(CComboBox)))
			return FALSE;
		// ((CComboBox*)pCtl)->SetFocus();
		((CComboBox*)pCtl)->ShowDropDown( TRUE );
		break;

	case EV_CBO_SEL: 
		// OLD: "%s %d %d", CtrlID, CurSel, TopIndex;
		// NEW: %s %d %s", CtrlID, CurSel, SelectionText,
		pCtl = GetCtlArg(pszArgs, pszRest);
		if (!pCtl || ! pCtl->IsKindOf(RUNTIME_CLASS(CComboBox)) )
			return FALSE;
		if ((sscanf(pszRest, "%d", &nItem) != 1))
			return FALSE;

		// ((CComboBox*)pCtl)->SetFocus();
		/* change to select by string, not index, since that is more robust
		((CComboBox*)pCtl)->SetCurSel( nItem ); */
		if (sscanf(pszRest, "%*d %*d") != 2) { // Not 2 indices => NEW msg (assumes cbo contents are not numbers)
			// get selection text from tail arg, since more reliable than other method
			m_strCboSel = strchr(pszRest, ' ') + 1;
		} // else will use text remembered from last CLOSE or SOK message
		// note possible odd case 2 above where m_strCboSel will be unset
		if (m_strCboSel.IsEmpty())
			// fallback to selecting by index
			((CComboBox*)pCtl)->SetCurSel( nItem );
		else if (((CLogCombo*)pCtl)->SelectStringExact(m_strCboSel) == -1) {
			TRACE("String not found in combo: %s\n",m_strCboSel);
			return FALSE;
		}
		m_strCboSel.Empty();	// reset it for next time since it's been used
		// reproduce notification msg to parent, i.e. this dialog
		SendMessage(WM_COMMAND, MAKELONG(pCtl->GetDlgCtrlID(), CBN_SELCHANGE),
								(LPARAM)pCtl->GetSafeHwnd());
		break;

	case EV_CBO_CLOSE:
		// "%s %s", CtrlID, text
		pCtl = GetCtlArg(pszArgs, pszRest);
		if (!pCtl || ! pCtl->IsKindOf(RUNTIME_CLASS(CComboBox)) )
			return FALSE;
		((CComboBox*)pCtl)->ShowDropDown(FALSE);
		// remember last selected string in case of old logs
		m_strCboSel = pszRest;
		break;

	case EV_CBO_SOK:	// nothing to do, new sel will be shown on SEL change
		// remember last selection in case of old logs in case of undropped select
		// hard to parse if selection has spaces:
		// ORIGINAL version:"%s %s %d" CtrlId, text, top-index
		// Very short-lived NEWER version: "%s %s" CtrlId, text
		// NEWEST version: deleted entirely
		pCtl = GetCtlArg(pszArgs, pszRest);
		if (!pCtl || ! pCtl->IsKindOf(RUNTIME_CLASS(CComboBox)) )
			return FALSE;
		m_strCboSel = pszRest;	// !! includes trailing top-index
		break;
	
	case EV_LIST_SEL:
		pCtl = GetCtlArg(pszArgs, pszRest);
		if (!pCtl || ! pCtl->IsKindOf(RUNTIME_CLASS(CListBox)) )
			return FALSE;
		if (sscanf(pszRest, "%d", &nItem) != 1)
			return FALSE;
		if (pCtl->GetStyle() & LBS_MULTIPLESEL){
			BOOL bSelected;
			bSelected = ((CListBox*)pCtl)->GetSel(nItem);
			((CListBox*)pCtl)->SetSel( nItem, !bSelected);
		}else
			((CListBox*)pCtl)->SetCurSel( nItem );
		// reproduce notification msg to parent, i.e. this dialog
		SendMessage(WM_COMMAND, MAKELONG(pCtl->GetDlgCtrlID(), LBN_SELCHANGE),
								(LPARAM)pCtl->GetSafeHwnd());
		break;

	case EV_BTN_CLICK:
		pCtl = GetCtlArg(pszArgs, pszRest);	// pszRest should have button text
/*		// This test works for all buttons. But should only playback if LogBtn?
		if (! pCtl || ! (::GetClassName(pCtl->GetSafeHwnd(), lpClassName, 32)
						 && _tcscmp(lpClassName, _T("Button")) == 0) )
			return FALSE; */
		//don't want... Grids and cells use bm_click
//		if (!pCtl || ! pCtl->IsKindOf(RUNTIME_CLASS(CButton)) ) // dicey, won't work if temporary used
//			return FALSE;
		// Following simulates user click, complete with notification msg back
		// to parent to fire its handler in the dialog. Push not visible, though.
		SetActiveWindow();		// Needed to ensure BM_CLICK takes effect, see docs.
		pCtl->SendMessage(BM_CLICK);
		break;

	case EV_TASK_SELECT:{
	//	pCtl = GetCtlArg(pszArgs, pszRest);
	//	if (! pCtl->IsKindOf(RUNTIME_CLASS(CButton)) ) // dicey, won't work if temporary used
	//		return FALSE;
		// Post the command to the main window
	//	GetParent()->PostMessage(WM_COMMAND, MAKELONG(pCtl, 0), (LPARAM) 0);
		}
		break;

	case EV_DLG_WHATSWRONG:{			
		char szName[128];
		char szId[128];
		sscanf(pszArgs, "%s %s", szName, szId);
		// following is wrong help api call for principle dialog. In fact it still works
		// if help results come from log because only DDE result matters for log playback.
		// Still better to let dialog itself replay the command handler, fetching arguments
		// from focus controls. (!!! windows focus may be off on playback? e.g. if switch to log player control?)
		if (this->IsKindOf(RUNTIME_CLASS(CDrawObjDlg))) {
			LPCTSTR	 pszResult = HelpSystemExecf("(Why-wrong-object |%s| %s) ", 
									STR2ARG(CString(szName)), STR2ARG(CString(szName)) );
			// Display result in hint dialog, which knows how to parse it.
			// Ask frame to show result in hint window
			theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong); 
		} else {
			// Just replay the command handler:
			SendMessage(WM_COMMAND, ID_CONTROL_WHATSWRONG);
		}
	} break;

	default:
		break;
	}
	return TRUE;
}

void CLogDialog::PointToObject(LPCTSTR pszObjID)
{
	CWnd* pWnd;
	
	if ((pszObjID[0] == '\0') 		// empty argstr => point to whole dialog
		|| (_stricmp(pszObjID, "dialog") == 0)) // special arg => point to whole dialog
	{	
		pWnd = this;
	} else {					// go from ctl name to child window
		int nID = CtlNameToID(pszObjID);
		if (nID == -1) {
			TRACE("Dialog::PointTo - unknown object %s\n", pszObjID);
			return;
		}
		pWnd = g_pDlg->GetDlgItem(nID);
		if (pWnd == NULL) {
			TRACE("Dialog::PointTo - control not found: %s ID=%d\n", pszObjID, nID);
			return;
		}
	}

	CRect rcCtl; 
	pWnd->GetWindowRect(rcCtl);
	// move the pointer to ctl mid, bottom. point rightward since dlg usually on right
	// But for combo boxes, point right to lower left corner so drop-down doesn't cover
	// pointer or msg (which goes at left if room -- still may move if no room.)
	TCHAR lpClassName[32];
	if (::GetClassName(pWnd->GetSafeHwnd(), lpClassName, 32)
		&& _tcscmp(lpClassName, _T("ComboBox")) == 0) {
		theApp.GetMainFrame()->MovePointerTo(rcCtl.left, rcCtl.bottom, CPtrWnd::UpRight);
	} else {
		theApp.GetMainFrame()->MovePointerTo(rcCtl.left + rcCtl.Width()/2, rcCtl.bottom, 
		                                    CPtrWnd::UpRight);
	}
}



BOOL CLogDialog::OnHelpInfo(HELPINFO* pHelpInfo) 
{
	if ((pHelpInfo->iContextType == HELPINFO_WINDOW)
		&& (pHelpInfo->dwContextId != 0))
	{
		AfxGetApp()->WinHelp(pHelpInfo->dwContextId, HELP_CONTEXTPOPUP);
	}
	return TRUE;
}


int CLogDialog::GetTrainerId(int ctrlId)
{
	return 0;
}

// general dialog utilities:

// transfer string list to/from multiple selection list box
void AFXAPI DDX_LBMultiSel(CDataExchange* pDX, int nIDC, CStringList& strs)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	CString value;
	if (pDX->m_bSaveAndValidate)
	{
		// empty string list of any existing entries
		strs.RemoveAll();

		// get count of selected items
		int nSelCount= (int)::SendMessage(hWndCtrl, LB_GETSELCOUNT, 0, 0);
		if (nSelCount > 0)
		{
			// fetch selected indices into array
			int* SelectSet = (int*) _alloca(nSelCount);  // holds selected indices
			::SendMessage(hWndCtrl, LB_GETSELITEMS, nSelCount, (LPARAM)SelectSet);
			for (int i=0; i<nSelCount; i++)
			{
				// fetch string for selected index
				LPTSTR pBuf = value.GetBufferSetLength(255);
				::SendMessage(hWndCtrl, LB_GETTEXT, SelectSet[i], (LPARAM)pBuf);
				// add it to result string list
				strs.AddTail(value);
			}
		}
	}
	else
	{
		POSITION pos = strs.GetHeadPosition();
		while (pos != NULL){
			value = strs.GetNext(pos);
			// must select by index. Find w/exact match, normally what we want
			int iStr = ::SendMessage(hWndCtrl, LB_FINDSTRINGEXACT, (WPARAM)-1,  (LPARAM)(LPCTSTR)value);
			if (iStr != LB_ERR)
				::SendMessage(hWndCtrl, LB_SETSEL, (WPARAM) TRUE, (LPARAM) iStr);
			else 
				TRACE("DDX_LBMultiSel: \"%s\" not found in listbox!", value);
		}
	}

}

// transfer elements of string list to populate list or combo box. 
// Loads only, presumably to set choices which don't change.
// Special case (for times): if element has equals sign, e.g. "T0 = ...", only uses
// part left of the equals sign, removing any trailing white space.
void AFXAPI DDX_FillList(CDataExchange* pDX, int nIDC, CStringList* pStrList)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	ASSERT(pStrList);
	if (! pStrList) return;

	if (!pDX->m_bSaveAndValidate)
	{
		POSITION pos = pStrList->GetHeadPosition();
		while (pos != NULL)
		{
			CString strValue = pStrList->GetNext(pos);
			//This is a cheat for the times list only want T0, T1 not rest (= to car at top of hill)
			int nPos;
			if ((nPos = strValue.Find('=')) > 0) {
				strValue = strValue.Left(nPos);
				strValue.TrimRight();
			}
			/////////////////////////////////////
			//Send meesages for both list boxes and combos --  wrong one will just be ignored
			::SendMessage(hWndCtrl, CB_ADDSTRING, 0, (LPARAM)(LPSTR)(LPCTSTR)strValue);
			::SendMessage(hWndCtrl, LB_ADDSTRING, 0, (LPARAM)(LPSTR)(LPCTSTR)strValue);
		}
	}
}

// transfer string list to/from edit box with one string per line
void AFXAPI DDX_FillEdit(CDataExchange* pDX, int nIDC, CStringList* pStrList)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);

	if (!pDX->m_bSaveAndValidate)
	{
		// join string list elements into single string.
		CString strValue;
		for (POSITION pos = pStrList->GetHeadPosition(); pos != NULL; ) {
			strValue += pStrList->GetNext(pos) + "\r\n";
		}

		::SendMessage(hWndCtrl, WM_SETTEXT, 0, (LPARAM)(LPSTR)(LPCTSTR)strValue);
		
	}
	else
	{
		pStrList->RemoveAll();
		int strlength;
		char lpszBuf[80]; 
		//sets size of the buffer
		*(WORD *)lpszBuf = sizeof(lpszBuf); 
		int nLine = 0;
		while ( (strlength = ::SendMessage(hWndCtrl, EM_GETLINE, nLine, (LPARAM)lpszBuf)) > 0)		
		{  //terminate string with NULL char
            lpszBuf[strlength] = '\0'; 
			CString str = lpszBuf;
			nLine++;
			pStrList->AddTail(str);
		}
	}

}