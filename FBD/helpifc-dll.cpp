////////////////////////////////////////////////////////////////////////////
//
// HelpIfc.cpp -- Help system interface.
// 
// Manages interprocess communication with the ANDES help system process.
// TCP/IP socket version.
//
// Socket version currently used for Wizard of Oz version with human tutor
// on a remote machine, though we expect to change to use for local IPC as 
// well, once we change helpsys to use sockets rather than local DDE msgs.
// 
// Because the helpsys interface is a singleton object -- there is only one
// in the whole application -- the API to this module is just a set of public 
// functions, not wrapped in a C++ class. The s_* prefix marks "global" vars, 
// which are really statics not visible outside this module. 
// !!! Might be good to wrap in a C++ class anyway (may someday want more 
// than one connection) but this simple method suffices for now.
//
////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "fbd.h"					// for DoWarningMessage
#include "history.h"				// for logging events, and simulating from log
#include "MainFrm.h"				// for displaying wait state during remote calls
#include "HelpIfc.h"

// "syntactic sugar": 
#define PUBLIC  extern
#define PRIVATE static
#define DLLIMPORT _declspec(dllimport)

// buffer sizes:
PRIVATE const int MAX_CMD_LEN = 512;	// Plenty, cmds normally < one line
PRIVATE const int MSG_BUF_LEN = 4096;	// ~2 pgs enough for even long hint result msgs


// APIs we use from helpifc.dll:
extern "C" {
DLLIMPORT int helpsys_initialize(int bShowConsole, const char* szImageName, int (*pCmdHandler) (char *));
DLLIMPORT int helpsys_execute ( char * cmdstr, char *resultbuf, int buf_size );
};



PRIVATE BOOL s_bConnected = FALSE;

// Following API makes real IPC connection state publicly available, if needed
PUBLIC BOOL HelpSystemReallyConnected() 
{ 
	return s_bConnected; 
}

// Public routine returns state of "virtual help system" interface when
// simulating help system while running in log mode. This way appears to rest 
// of app it is connected to help system, even if no real IPC connection when 
// playing log (this may affect command enabling and other processing). 
// Note public routines that client code might expect to test connection
// status should use this routine internally (see HelpSystemConnect),
// since they may be called in a log play context.
//
// !!! Currently don't detect if virtual help system has crashed during log play.
// Also can't tell if log was made without help system at all, which is possible, 
// though we are very unlikely to be interested in playing such a log.
PUBLIC BOOL HelpSystemIsConnected()
{
	if ((LogPlayerInPlayback() && ! LogPlayerCallHelp())
		|| theApp.IsRemoteViewer()) {
		return TRUE;
	}
	return HelpSystemReallyConnected();
}




// HelpSystemEnsureRunning -- ensure help system running.
// This is mainly for the local DDE version, so can start it early and
// defer waiting to make connection until later (first open). 
// The TCP/IP system requires the remote system to be started by
// some other means in advance; if not, we will detect it when connect fails.
// However, for WOZ version, we use this to ensure have connected before
// any problem is open, which is required by the interface sharing method.
//
PUBLIC BOOL HelpSystemEnsureRunning()
{
	return TRUE;
}

static int CommandHandlerFn (char* str)
{
	return theApp.OnHelpSysCommand(str);
}

//
// HelpSystemConnect -- establish IPC connection with help system,
// 
// Returns TRUE for success, FALSE for failure.
// Safe to call if already connected to ensure. But note s_sockMsgs socket
// should be closed after failure before retry -- we don't close here
// so GetLastError can retrieve error code.
//
PUBLIC BOOL HelpSystemConnect(LPCTSTR pszAddress)
{
	// If already connected, we're done.
	// Test virtual help system state in log mode, in case client code
	// is using this to ensure connected before working
	if (HelpSystemIsConnected()) return TRUE;

	// For debugging: Allow registry settings to configure whether to show
	// Lisp console window, and also to allow use of an alternate (i.e debuggable)
	// Lisp image file.
	BOOL bShowConsole = theApp.GetProfileInt("Settings", "ShowConsole", 0);
	CString strImageName = theApp.GetProfileString("Settings", "ImageName", "HelpSys.dxl");

	// load and initialize help system image, passing in CommandHandler callback
	if (! helpsys_initialize(bShowConsole, strImageName, CommandHandlerFn)) {
		TRACE("Couldn't initialize help system\n");
		// Caller should report failure as appropriate
	} else {
		TRACE("Initialized help system");
		s_bConnected = TRUE;
	}

	return (s_bConnected);
}


// 
// HelpSystemDisconnect -- Terminate conn and free resources on session end.
//
PUBLIC void HelpSystemDisconnect()
{
	// Post message to action interpreter to terminate. This should cause
	// close of socket from the other end followed by termination of process.
	HelpSystemSendf( "(exit-andes)" ); // note no reply expected

	// shut down connection from our side. Not strictly necessary anymore, since 
    // exit-andes causes remote end to close, but good style to do so anyway. 
	TRACE("Closing connection to help system\n");
}






//--------------------------------------------------------------------------------
//
// For Synchronous (blocking) remote procedure calls:
// DoExec implements blocking RPC using lower-level async primitives
// Only one synchronous transaction may be in process at a time.
// Runs modal event loop until completion flagged. Possible to cancel while in 
// process by calling HelpSystemCancelCall (from user input event handler, for example.) 
//
//--------------------------------------------------------------------------------

// buffer to hold returned result data 
PRIVATE char s_szResult[MSG_BUF_LEN];	// Retrieved command result string


//
// HelpSystemCancelCall - cancel current in-progress synchronous transaction
// should have no effect if not in one.
//
PUBLIC void HelpSystemCancelCall()
{
}

// Programmatically set result. For use in log event playing
PUBLIC void HelpIfcSetResult(LPCTSTR pszResult)
{
	// save data into global result buf
	if (pszResult) {
		ASSERT(strlen(pszResult) < MSG_BUF_LEN); // < MAX so room for final NUL
		strcpy(s_szResult, pszResult);
	} else
		s_szResult[0] = '\0';	// set to empty string on failure
}



///////////////////////////////////////////////////////////////////////////////////
//
// Public Help system RPC functions. All client calls should go through one of 
// these entries. All logging should go in these routines, too.
//
///////////////////////////////////////////////////////////////////////////////////

// For setting RPC call parameters:
PRIVATE int s_nTimeout = HELPIFC_TIMEOUT;	// time to wait before failure, milliseconds
PRIVATE LPCTSTR s_pszStatusMsg = NULL;		// status msg to show, NULL => use default

// Use before call to adjust parameters for special cases.
PUBLIC void HelpIfcSetCallParms(int nTimeout, LPCTSTR pszMsg /*=NULL*/)
{
	s_nTimeout = nTimeout;
	s_pszStatusMsg = pszMsg;
}

PRIVATE inline void RestoreDefaultParms() { HelpIfcSetCallParms(HELPIFC_TIMEOUT); };

//
// HelpSystemExecf -- format and execute a command, returning result string or NULL
//
// This encapsulates our use of the Help System interface for remote
// procedure calls. Takes a printf style variable argument list and
// formats into a buffer, sends the command with the current timeout, and 
// returns pointer to a result string. The return value is static data 
// overwritten each call and should be copied for long-term use. Wrapped
// in Mainframe's BeginDdeWait/EndDdeWait to adjust user interface around
// helpsys RPC's (disables main window, shows wait cursor and status bar message).
 //
// Returns NULL on errors.
//
// This routine is also now instrumented for log playback: if app is in log playback mode,
// will simulate an RPC to the help system by fetching result from the log. 
// 
LPCTSTR HelpSystemExecf(LPCTSTR lpszFormat, ...)	
{
	// format command string into our buffer.
	char szCmd[MAX_CMD_LEN];
	va_list args;
	va_start(args, lpszFormat);
	int nChars = vsprintf(szCmd, lpszFormat, args);
	// risk of buffer overflow with vsprintf:
	ASSERT(nChars >= 0 && nChars < MAX_CMD_LEN);// < MAX so room for trailing NUL
	va_end(args);

	// if we are running from log playback, and not calling help system
	// simulate the RPC using logged data 
	if (LogPlayerInPlayback() && ! LogPlayerCallHelp() )
	{
		TRACE("HelpIfc: simulating help system call: %s\n", szCmd);
		((CMainFrame*)AfxGetMainWnd())->BeginDdeWait(s_pszStatusMsg);
		BOOL bSuccess = LogPlayerGetHelpCallResult(s_szResult);
		((CMainFrame*)AfxGetMainWnd())->EndDdeWait();
		return bSuccess ? s_szResult : NULL;
	}

	// else send the command to the help system using current parms and fetch result.
	
	// quick failure if help system not connected:
	if (!s_bConnected && !theApp.IsRemoteViewer()) 
		return NULL;	

	// Log the call
	LogEventf(EV_DDE_EXEC, "%s", szCmd);
	TRACE("Calling %s\n", szCmd);

	// Update UI to show visible wait state, message
	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->BeginDdeWait(s_pszStatusMsg);
	
	// run the transaction.
	// s_szResult should contain result or empty string on return		
	BOOL bSuccess = helpsys_execute(szCmd, s_szResult, MSG_BUF_LEN);

	// Restore the normal UI
	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->EndDdeWait();

	// for convenience, revert to default parms after each call, 
	// since almost all calls just use defaults. 
	RestoreDefaultParms();

	// Log the result
	if (bSuccess) {
		TRACE("Got result %s\n", s_szResult);
		LogEventf(EV_DDE_RESULT, "|%s|", s_szResult);
	} else {
		TRACE("Exec failed\n");
		LogEventf(EV_DDE_FAILED, "%s", szCmd);
	}

	return bSuccess ?  s_szResult : NULL;
}

// 
// HelpSystemSendf -- format and post a notification msg to the help system 
//                    notifications are commands w/no result required
// 
// Returns success or failure of the post operation. Note this is not
// success or failure of the command processing.
//
// Takes printf style arglist like Execf.
//
// This is suitable for sending notifications to the help system where we
// don't have any use for the result. 
// 
// Our protocol currently can't detect if a notification call fails in the helpsys. 
// We could add ids and NACK messages for this case, but currently haven't had much 
// need for them. If helpsys error occurs on this, failure will probably show up 
// later in any case.
//       
PUBLIC BOOL HelpSystemSendf(LPCTSTR lpszFormat, ...)	
{
	// No-op if simulating help system from log file
	if (theApp.IsRemoteViewer() ||
		(LogPlayerInPlayback() && ! LogPlayerCallHelp())){
		if (theApp.GetMainFrame())
			theApp.GetMainFrame()->OnDdeSend();
		return TRUE;
	}

	// no-op if not connected
	if (!s_bConnected) return FALSE;

	// format command string into our buffer.
	char szCmd[MAX_CMD_LEN];
	va_list args;
	va_start(args, lpszFormat);
	int nChars = vsprintf(szCmd, lpszFormat, args);
	// risk of buffer overflow with vsprintf:
	ASSERT(nChars >= 0 && nChars < MAX_CMD_LEN); // < MAX so room for trailing NUL
	va_end(args);

	// Even though this doesn't put app in a wait state, we still notify mainframe 
	// since it resets certain things on every help system call.
	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->OnDdeSend();

	// Log the event
	LogEventf(EV_DDE_POST, "%s", szCmd);

	// Post the command to the receiver's queue
	BOOL bSuccess = helpsys_execute(szCmd, s_szResult, MSG_BUF_LEN);
	if (!bSuccess)
		LogEventf(EV_DDE_FAILED, "%s", szCmd);

	return bSuccess;
}

// 
// Client func to do async function calls. Takes complete command, registers 
// client-specified callback to fire on completion
//
// !!! This routine does not log the DDE call in any way. Older versions of Andes went
// through HelpSystemSendf here so logged call as a DDE_POST. That is misleading, though, since this
// is an async RPC with result, not a notify without result. Should use a distinct log event here for this, 
// different from DDE (for sync requests) and DDE_POST (for proc calls).  
// Arrival of result is also not logged in this module, though might be logged specially in the client's
// callback (as in CEqView::OnEqResult, which is the only thing that could use this currently.)
// 
PUBLIC BOOL HelpSystemExecAsync(LPCTSTR pszCmd, PROCP pfnNotify, DWORD dwCookie)
{
	
	return FALSE;
}

//-------------------------------------------------------------------------------------------------------
// Argument processing utilities:
//-------------------------------------------------------------------------------------------------------

// Ensure string value is Lisp-readable
CString LISPSTR(const CString& strInput)
{
	CString strOutput = strInput;
	// escape any embedded backslashes. (must do first, following step inserts backslashes)
	strOutput.Replace("\\", "\\\\");		// doubled for C lexical convention
	// escape any embedded quotes.
	strOutput.Replace("\"", "\\\"");
	return strOutput;
}