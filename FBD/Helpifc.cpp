////////////////////////////////////////////////////////////////////////////
//
// HelpIfc.cpp -- Help system interface.
//
//
// Manages a DDE conversation with the help system using the DDEML
//
// The API to this module currently straight C, could be made into C++ class.
//
////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"			// needs to be outside big #ifdef

#ifdef HELPIFC_TCP			// define to build helpifc with TCP interface code

#pragma message ("WARNING: DDE helpifc.cpp included in HELPIFC_TCP build -- ignored")

#else  // ! HELPIFC_TCP		// big #else for DDE code wraps whole file

#include <ddeml.h>			// DDE Management Library
#include "fbd.h"			// for DoWarningMessage, g_strAndesDir
#include "history.h"		// for logging events
#include "MainFrm.h"		// for displaying wait state during remote calls
#include "HelpIfc.h"

//
//  DDE state vars stored in g_* "global" variables are really statics 
//  private to this module. Currently no need to package into a C++ class
//  since there is only one help system conversation for the whole application.
//  !!! Would be good to do so anyway, in case we ever use more than one DDE 
//  conversation.
// 
static DWORD g_idDdeInst = 0;	// Instance handle of attachment to DDEML layer
static HCONV g_hHelpConv = NULL;// Handle on our conversation w/help system	
static HSZ g_hszServName = NULL;// DDEML string handles for remote service, 
static HSZ g_hszTopicName= NULL;//  topic, &
static HSZ g_hszCommandResult;	//  item name used for results of remote execs
static BOOL g_bHelpSysRunning = FALSE; // set if found or started up help sys
//
// String buffers for command strings and command results
//
#define MAX_DDE_LEN 512				// Max length of DDE execute strings.
static char g_szCmd[MAX_DDE_LEN];	// Last command string sent
static char g_szResult[MAX_DDE_LEN];// Last retrieved result string
//
// Flag set to defer warning msg on receipt of DDE disconnect during wait state
//
static BOOL g_bNotifyDisconnect = FALSE;	// set => notify user when wait ends

// Forward declaration of our DDE callback procedure
static HDDEDATA CALLBACK OnDdeEvent(UINT uType, UINT uFmt, HCONV hconv, 
							 HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
							 DWORD dwData1, DWORD dwData2);
// Messages
static const char szLostConn[] =
"Lost connection to Tutor\nAlthough you may continue to work, feedback and hints will not be available";
////////////////////////////////////////////////////////////////////////////
// Connection management:
////////////////////////////////////////////////////////////////////////////

static BOOL EnsureDdeInit()	// Initialize DDEML layer as needed.
{
	static BOOL bNotified = 0;	// only show failure msg once
	if (g_idDdeInst == 0)		// not already done
	{
		// Set callback filter for client-only transactions.
		UINT result = DdeInitialize(&g_idDdeInst, (PFNCALLBACK) OnDdeEvent,
						   APPCMD_CLIENTONLY, 0);
		if (result != DMLERR_NO_ERROR && ! bNotified){
			AfxMessageBox("Couldn't initialize DDEML!\n");
			bNotified = TRUE;
			return (FALSE);
		}
	}

	return TRUE;
}

// Internal macro tests if connected to real help system via DDE
static inline BOOL IsConnected()
{
	return (g_hHelpConv != NULL);
}
// Following makes real connection state publicly available
extern BOOL HelpSystemReallyConnected() { return IsConnected(); }

// Public routine returns state of "virtual help system" interface when
// simulating help system while running in log mode. This way appears to rest 
// of app it is connected to help system, even if no real DDE connection when 
// playing log (may affect command enabling and other processing). 
// Note public routines that client code might expect to test connection
// status should probably use this routine internally (see HelpSystemConnect),
// since they may be called in a log play context.
// !!! Currently don't record if virtual help system has crashed during log play,
//     or if log was made without help system (not that useful, but possible).
extern BOOL HelpSystemIsConnected()
{
	if ((LogPlayerInPlayback() && ! LogPlayerCallHelp()) 
		|| theApp.IsRemoteViewer()) {
		return TRUE;
	}
	return (IsConnected());
}


static void TryConnect()
{
	// Allocate string handles for use in Connect call.
	g_hszServName = DdeCreateStringHandle(g_idDdeInst, "ALLEGRO", CP_WINANSI);
	g_hszTopicName = DdeCreateStringHandle(g_idDdeInst, "ALGEBRA", CP_WINANSI);

	// Try to connect
	g_hHelpConv = DdeConnect(g_idDdeInst, g_hszServName, g_hszTopicName,
							 (PCONVCONTEXT) NULL);
	
	if (IsConnected()) { // sweet smell of success

		// save this string handle for frequent use later:
		g_hszCommandResult = DdeCreateStringHandle(g_idDdeInst, "COMMAND-RESULT",
											   CP_WINANSI);
	}

	// don't need these anymore:
	(void) DdeFreeStringHandle(g_idDdeInst, g_hszServName);
	(void) DdeFreeStringHandle(g_idDdeInst, g_hszTopicName);
}

//
// HelpSystemEnsureRunning -- ensure help system running.
// If it's already running, we just connect to it; else we start it up
// so as to connect at a later time after giving it time to initialize.
// (Help system already running normally only happens in our development environment).
// Returns TRUE if connected or started, FALSE if failed both. 
// Can test HelpSystemIsConnected to see if connected on return.
//
BOOL HelpSystemEnsureRunning()
{
	// no-op if we've already know its running
	if (g_bHelpSysRunning) return TRUE;

	// make sure DDE is initialized
	if (! EnsureDdeInit()) return FALSE;
	
	// Try to establish DDE conversation with running help system
	TryConnect();
	if (IsConnected()) 
		return (g_bHelpSysRunning = TRUE); // early exit
	
	// Else couldn't connect: try to start up the help system executable
	CString strExePath, strArgPath;
	// quote path arg since AndesDir may have spaces, e.g. C:\Program Files\Andes\ 
#ifndef ATLAS
	// strExePath = g_strAndesDir + "Lisp.exe";
	// strArgPath = "\"" + g_strAndesDir + "Andesrt.img" + "\""; 
	strExePath = g_strAndesDir + "Andes.exe";	// new for ACL5.0 version
#else
	strExePath = "\"" + g_strAndesDir + "Atlas.exe" + "\"";
	strArgPath = "";
#endif
	TRACE("DDE connect failed, trying to exec %s \n", strExePath);
	HINSTANCE hInst;
	hInst = ShellExecute(NULL, "open",  strExePath, strArgPath, g_strAndesDir, SW_SHOWNORMAL); 
	if ((int) hInst <= 32) {
		TRACE("Shellexec failed, code = %d\n", (int) hInst);
	} else
		return (g_bHelpSysRunning = TRUE);

	// else failed:
	return (g_bHelpSysRunning = FALSE);
}

//
// HelpSystemConnect -- establish DDE conversation with help system,
//						blocking until connected or give up.
// 
// Returns TRUE for success, FALSE for failure.
// Safe to call repeatedly to ensure connected.
//
extern BOOL HelpSystemConnect()
{
	// if already connected, we're done.
	// Test virtual help system state in log mode, in case client code
	// is using this to ensure connected before working
	if (HelpSystemIsConnected()) return TRUE;

	// Ensure help system running in case not done earlier.
	if (! HelpSystemEnsureRunning()) return FALSE;

	// "Only connect" -- E.M. Forster
	int nTries = 1;
	TryConnect();
	if (! IsConnected() ) {
		// May need to wait until remote DDE server is up and ready.
		// The sophisticated way would be ::WaitforInputIdle (would need 
		// process handle) or to retry connecting in background using timer or
		// thread. Following is cruder but adequate method
		for (nTries = 1; !IsConnected() && nTries <= 60; ++nTries) 
		{
			// Take a rest for a while
			::Sleep(500);		// = half a second
			// and try try again
			TryConnect();
		}
	}
	TRACE("DDE Connect %s, attempts=%d\n", 
		IsConnected() ?  "succeeded" :"failed", nTries);

	return (IsConnected());
}



// 
// HelpSystemDisconnect -- Terminate conn and free resources when done
//
extern void HelpSystemDisconnect()
{
	// if we started the helpserver running but never connected to it (maybe
	// user never got around to opening a problem), we should kill it. 
	// We detect by seeing if helpsys flagged as running but not currently connected.
	// This should normally be distinguishable from connect then crash, since we clear 
	// the running flag on receipt of unexpected DDE disconnect. (!!! reliable?)
	// To kill it would need process ID, which we don't have, since we started
	// with simple ShellExecute, not more elaborate CreateProcess API.
	// So we just connect in this case so as to be able to send exit-andes message.
	if (g_bHelpSysRunning)
		HelpSystemConnect(); // ensure connected

	// Post message to action interpreter to terminate. This should cause initiation
	// of shutdown from the other end, although they're supposed to ACK
	// the exec first to the DDEML.
	HelpSystemSendf( "(exit-andes)" );

	// shut down DDE connection from our side. Not strictly necessary anymore, since 
     // exit-andes causes remote end to shutdown, but good style to do so anyway. 
	TRACE("DDE Sending disconnect\n");
	(void) DdeDisconnect(g_hHelpConv);
	g_hHelpConv = NULL;
	g_bHelpSysRunning = FALSE;	// as far as we're concerned

	// clean up DDEML resources
	(void) DdeFreeStringHandle(g_idDdeInst, g_hszCommandResult);
	// !!! Should free any callback records for pending xacts. We're probably
	// terminating app, though, so doesn't much matter.
	DdeUninitialize(g_idDdeInst);
	g_idDdeInst = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// Help System DDE transactions: Routines to send DDE transactions via the DDEML. 
//
// At the lowest layer, the DDE protocol consists of two  processes exchanging special 
// windows messages, and passing arguments via a system-wide string table known as 
// the Atom table. See Petzold book or Win32 documentation for details.
//
// After the connection is set up, the messages are usually *posted*, meaning they
// are deposited in the receiver's message queue without waiting for them to be
// processed. This means that at the lowest layer the DDE protocol is "asynchronous". A
// typical DDE transaction involves two windows messages, a command and an acknowledgement:
//
//       Process1                                                  Process2
//    Posts WM_DDE_EXECUTE-----------> placed in queue                 .
//          .                                                          .  
//   (Can process other msgs)                         Retrieves message and executes command
//          .                                                          .
//          .          placed in queue<-------------- Posts WM_DDE_ACK w/success or failure
//    Retrieves ACK 
//
// The DDE Management Library (DDEML)is a subroutine library that implements the DDE
// protocol in a standard manner. It shields the user of the library from the details of 
// the windows messages and provides a function-call routine to initiate DDE actions. It
// provides for registering a callback routine to be invoked to handle relevant DDE
// events such as receipt of data change notifications. 
//
// By default, DDEML transactions are "synchronous": the DDEML waits a user-specified time 
// for the relevant ACK before returning from a transaction routine.  However, the DDEML runs 
// its own re-entrant message loop while waiting for the ack in order to allow other user 
// interface events to be processed while awaiting the reply. Note that this processing 
// could lead to attempts to begin further DDE transactions; however the DDEML prohibits 
// starting a second synchronous transaction while a first is pending and these will fail 
// with an error code indicating REENTRANCY. This possible re-entrancy during synchronous
// transactions is something users of this module need to take into account. 
//
// This also means it is possible for us to receive a DDEML callback while a synchronous 
// transaction is in process, for example if the remote process shuts down we will receive 
// the DISCONNECT callback before the (failure) return from the transaction. 
// 
// The DDEML also permits initiating transactions in "asynchronous" mode.  The DDEML returns
// immediately after posting the DDE message, providing the caller with a transaction 
// identifier. On receiving the relevant ACK, the DDEML fires the callback for transaction
// completion and passes the transaction id. Thus asynchronous mode is closer to "raw"
// DDE message exchanges. Routines for asynchronous help system interactions are below.
// We provide for our caller to register a callback routine which we fire in turn on
// completion events, so as to insulate our caller from the DDEML.
//
// The "vanilla" message loop run by DDEML during pending transactions has no knowledge 
// of MFC, and is evidently not user-modifiable. This means that the MFC idle time 
// actions, such as updating the user interface objects (e.g. toolbar buttons), will not 
// occur during this interval. This can lead to inconsistent appearance during long 
// transactions, e.g. toolbar buttons enabled even though no handler exists. !!! We could 
// try to fix this by using DDEML's asynchronous mode internally, and perhaps running an MFC 
// message loop or invoking OnIdle while awaiting the completion event.
//
// We make use of the DDE EXECUTE operation, which sends a C-style string containing
// a command to be executed. Note that the ACK only indicates whether the command was
// successfully executed or not, and contains no result data. (DDE does not define any 
// standard way of retrieving data resulting from command execution, although a proposal 
// can be found in the MSDN CD documentation.) The ALLEGRO DDE implementation makes the 
// result of the last command available in the data item COMMAND-RESULT. A further 
// DDE REQUEST transaction must be used to obtain this. 
//
// Therefore what looks to our caller like a remote procedure call really involves two DDE 
// transactions, an EXECUTE and a REQUEST for the result, each of which must be ACK'ed
// internally. If no result data is needed it is more efficient to use our API's that do 
// not fetch the result.
//
/////////////////////////////////////////////////////////////////////////////

// Lower-level worker routines for synchronous operation:
static BOOL HelpSystemSendExec(LPCTSTR lpszCommand, 
							   UINT nTimeout = HELPIFC_TIMEOUT);
static UINT HelpSystemGetResult(char* buf, UINT buflen, 
								UINT nTimeout = HELPIFC_TIMEOUT);
static UINT HelpSystemSendRequest(LPCTSTR lpszItem, char* buf, UINT buflen, 
								  UINT nTimeout = HELPIFC_TIMEOUT);
static UINT DoSendRequest(HSZ hszItem, char* buf, UINT buflen, UINT nTimeout = HELPIFC_TIMEOUT);

/* DdeClientTransaction call is as follows :
HDDEDATA DdeClientTransaction(
	LPBYTE  pData,		// pointer to data to pass to server
    DWORD  cbData,		// length of data
    HCONV  hConv,		// handle to conversation
    HSZ  hszItem,		// handle to item name string
    UINT  wFmt,			// clipboard data format, usually CF_TEXT
    UINT  wType,		// transaction type
    DWORD  dwTimeout,	// time-out duration, TIMEOUT_ASYNC for asynchronous
    LPDWORD  pdwResult 	// pointer to transaction result, xact_id for async
   );
*/

// 
// SendExec: Sends DDE Execute command string.
//
// Optional timeout parameter defaults to HELPIFC_TIMEOUT if unspecified.
// 
// This returns when the command is finished executing with success or failure
// of the exec operation. A further DDE request must be issued in order to 
// retrieve any result data. Allegro's DDE implementation makes the the result 
// of the last executed command available in the "COMMAND-RESULT" data item.
//
BOOL HelpSystemSendExec(LPCTSTR lpszCommand, UINT nTimeout)
{
	UINT	 cmd_len;
	HDDEDATA hData;
	DWORD	 dwResult;

	if (g_hHelpConv == NULL) return FALSE;

	cmd_len = (UINT) strlen(lpszCommand) + 1; // don't forget NUL terminator!
	TRACE("DDE: Sending Exec: |%s|\n", lpszCommand);
	LogEventf(EV_DDE_EXEC, "%s", lpszCommand);

	hData = DdeClientTransaction(
				(LPBYTE) lpszCommand,
				(DWORD) cmd_len,
				g_hHelpConv,
				(HSZ) NULL,			// no item handle on Exec
				0,					// no format for Exec 
				XTYP_EXECUTE,
				nTimeout,
				&dwResult);

	// No real data expected; returned hData set to TRUE for success, NULL for failure.
	if (hData != NULL) {  
		DdeFreeDataHandle(hData); // unnecessary, but apparently harmless
		return TRUE;
	} else {
		TRACE("DDE Exec failed! flags=%x, DDEML errno=%x\n", dwResult, DdeGetLastError(g_idDdeInst));
		LogEventf(EV_DDE_FAILED, "%s", lpszCommand);
		return FALSE;
	}
}

// 
// SendRequest: Sends DDE request for item, filling in caller's  buffer with 
//              result. 
//
// Returns: result length. Clears buffer string on failure.
//
// Delegates most of the job to internal worker routine DoSendRequest, which 
// takes string handle arg.
//

UINT HelpSystemSendRequest(LPCTSTR lpszItem, char* buf, UINT buflen, UINT nTimeout)
{
	if (g_hHelpConv == NULL) return 0;

	HSZ		 hszItem;
	UINT data_len = 0;

	TRACE("Sending DDE request: item = %s\n", lpszItem);
	hszItem = DdeCreateStringHandle(g_idDdeInst, lpszItem, CP_WINANSI);
	data_len = DoSendRequest(hszItem, buf, buflen, nTimeout);
	if (data_len == 0) {
		TRACE("DDE Request failed, item = %s \n", lpszItem);
	}
	DdeFreeStringHandle(g_idDdeInst, hszItem);
	return (data_len);
}

// DoSendRequest: does work of DDE request operation, given string handle
static UINT DoSendRequest(HSZ hszItem, char* buf, UINT buflen, UINT nTimeout)
{
	if (g_hHelpConv == NULL) { buf[0] = '\0'; return 0; }
	
	HDDEDATA hData;
	DWORD	 result;
	UINT	 data_len = 0;

	hData = DdeClientTransaction(
				(LPBYTE)NULL,
				(DWORD) 0,
				g_hHelpConv,
				hszItem,
				CF_TEXT,	// text format
				XTYP_REQUEST,
				nTimeout,
				&result);
	if (hData != NULL) // Success: Copy returned data into caller's buffer.
	{
		data_len = DdeGetData(hData, (LPBYTE) buf, (DWORD) buflen, 0);
		// release data handle we got
		DdeFreeDataHandle(hData);

		if (data_len == 0) // possible? success w/zero-length data block
			buf[0] = '\0'; // clear string; will appear same as error.
		// !! could make sure data block is NUL-terminated for use as C String.
	} 
	else	// failure: clear buffer string.
	{
		buf[0] = '\0';
		data_len = 0;
		TRACE("DDE Request failed! flags=%x, DDEML errno=%x\n", result, DdeGetLastError(g_idDdeInst));
		// LogEventf(EV_DDE_FAILED, ""); // Logged in HelpSystemGetResult
	}
	// !!! Note this return value means caller can't distinguish failure 
	// from success w return value = empty string.
	return (data_len);
}

//		
// GetResult: Send request for result of last command, filling in caller's 
//            string buffer with result string.
// Returns length of result. Clears buffer string on failure and returns 0.
//
UINT HelpSystemGetResult(char* buf, UINT buflen, UINT timeout) 
{
	// This just translates into a REQUEST transaction for the 
	// special item "COMMAND-RESULT" used by ALLEGRO.
	// We already have a saved item handle for this string.

	// In case of remote viewing, result should already be in right buffer
	if (theApp.IsRemoteViewer())
		return strlen(g_szResult);

	UINT cbResult = DoSendRequest(g_hszCommandResult, buf, buflen, timeout);

	// trace and log the DDE RESULT event
	// ?Do we need log failure of containing two-part RPC? Note this routine only
	// used for SYNC transactions and is only called if EXEC succeeded. So failure 
	// here is very rare. Still, following log entry doesn't let us distinguish DDE 
	// failure from success with return of empty string, possible in some case.
	TRACE("DDE: Got Result: |%s|\n", buf);
	LogEventf(EV_DDE_RESULT, "|%s|", buf);	// Failure coded as empty string -- OK?

	return cbResult;
}

/////////////////////////////////////////////////////////////////////////
// Routines for asynchronous operations:
//
// These queue a DDE operation and return immediately. Our caller can specify
// a notify procedure with a single DWORD argument to be called when
// the transaction completes. 
//
// A remote procedure call and return involves *two* DDE transactions, the 
// exec and the result-request. We simplify a bit by not firing client 
// callbacks for the asynch exec completion. Presumably the caller will put a 
// result-request into the queue immediately after the exec and handle the 
// result of interest -- the return value -- when it completes. 
//
// !!! Might it be possible for async exec to fail yet subsequent request for
// COMMAND-RESULT succeed with an old value? For us, async exec failure 
// usually means crash on other end. Also, asynchronous mode is probably 
// going away anyway, so no need to worry about all the subtleties. 
//
///////////////////////////////////////////////////////////////////////////

// Little record for saving async notification callback info:
typedef struct 
{
	PROCP pfnProc;			// ptr to client's procedure to call
	DWORD dwArg;			// argument to pass
} NOTIFY, *NOTIFYP; 

//
// QueueExec -- post an Execute command message to the receiver's queue
//  
// Returns: TRUE for success, FALSE otherwise. Note this does not
// mean the command was executed, just posted successfully.
//
BOOL HelpSystemQueueExec(LPCTSTR lpszCommand)
{
	UINT	 cmd_len;
	HDDEDATA hData;
	DWORD	 xact_id;

	if (g_hHelpConv == NULL) return FALSE;

	cmd_len = (UINT) strlen(lpszCommand) + 1; // don't forget NUL terminator!
	hData = DdeClientTransaction(
				(LPBYTE) lpszCommand,
				(DWORD) cmd_len,
				g_hHelpConv,
				(HSZ) NULL,			// no item handle on Exec
				0,					// no format for Exec 
				XTYP_EXECUTE,
				TIMEOUT_ASYNC,		// signals ASYNC transaction
				&xact_id);			// receives transaction id
	TRACE("DDE: Queued EXEC (%#x) |%s| \n", xact_id, lpszCommand);
	LogEventf(EV_DDE_POST, "%s", lpszCommand);

	// No notify routine for exec completions, just attach NULL to xact_id
	DdeSetUserHandle(g_hHelpConv, xact_id, (DWORD) NULL);
	if (hData != NULL) {  
		DdeFreeDataHandle(hData);
		return TRUE;
	} else {
		TRACE("DDE QueueExec failed!\n");
		LogEventf(EV_DDE_FAILED, "%s", lpszCommand);
		return FALSE;
	}
}


// 
// QueueRequest: Post a DDE request to the receiver's queue.
//
// As above, delegates most of the job to common worker routine DoQueueRequest
//
static void DoQueueRequest(HSZ hszItem, PROCP pfnNotify, DWORD dwNotifyArg);

static void HelpSystemQueueRequest(LPCTSTR lpszItem, PROCP pfnNotify, DWORD dwNotifyArg)
{
	if (g_hHelpConv == NULL) return ;

	HSZ		 hszItem;
	hszItem = DdeCreateStringHandle(g_idDdeInst, lpszItem, CP_WINANSI); 
	DoQueueRequest(hszItem, pfnNotify, dwNotifyArg);
	DdeFreeStringHandle(g_idDdeInst, hszItem);
}

static void DoQueueRequest(HSZ hszItem, PROCP pfnNotify, DWORD dwNotifyArg)
{
	if (g_hHelpConv == NULL) return ;

	HDDEDATA hData;
	DWORD	 xact_id;

	hData = DdeClientTransaction(
				(LPBYTE)NULL,
				(DWORD) 0,
				g_hHelpConv,
				hszItem,
				CF_TEXT,		// text format
				XTYP_REQUEST,
				TIMEOUT_ASYNC,	// signals ASYNC transaction
				&xact_id);
	DdeFreeStringHandle(g_idDdeInst, hszItem);
	if (hData == NULL) {
		TRACE("DDE QueueRequest failed!\n");
		return;
	} else
		TRACE("Queued REQUEST: ID= %#lx\n", xact_id);

	// allocate notify record and attach it to transaction id in DDEML
	NOTIFYP pNotify = (NOTIFYP) malloc(sizeof(NOTIFY));
	pNotify->pfnProc = pfnNotify;
	pNotify->dwArg   = dwNotifyArg;
	DdeSetUserHandle(g_hHelpConv, xact_id, (DWORD) pNotify);
}

// 
// QueueResultRequest: Queue request for last command result
// Fire caller's notify function on arg on completion
//
static void HelpSystemQueueResultReq(PROCP pfnNotify, DWORD dwNotifyArg)
{
	// This is just a REQUEST transaction with the item handle 
	// for the special item "COMMAND-RESULT"
	DoQueueRequest(g_hszCommandResult, pfnNotify, dwNotifyArg);
}

// 
// OnXactComplete -- Handler for async transaction completion event
// dwData1 = transaction id
// hdata = handle to returned data; NULL for failure.
//
static void OnXactComplete(HCONV hconv, DWORD dwID, HDDEDATA hdata)
{	
	TRACE("DDE: XACT completed (%x) %s ", dwID, hdata ? "OK..." : "Failed");
	 	
	// Retrieve pointer to callback info we stashed under xact ID in ConvInf
	CONVINFO ConvInfo;
	NOTIFYP pNotify = NULL;
	ConvInfo.cb = sizeof(CONVINFO);		
	if (! DdeQueryConvInfo(hconv, dwID, &ConvInfo) ){
		TRACE("On XACT_COMPLETE: Couldn't get Conv Info!!\n");
		return;
	}
	// Log error code for failure:
	if (hdata == NULL) TRACE("DDEML errno=%x... ", ConvInfo.wLastError);

	pNotify = (NOTIFYP) ConvInfo.hUser;
	if (pNotify == NULL) {	
		TRACE("No handler, event ignored\n");
		return;
	}
			
	// Fire client's completion notification handler, passing returned
	// data and saved cookie for context arg. Manual says we
	// are NOT to free the data handle during this transaction.
	// Note hData is NULL for failure, non-NULL for success, even if no data.
	if (pNotify->pfnProc != NULL) 
	{
		// Get returned string arg to pass to callback. NULL arg codes failure.
		// Success w/no data can happen (e.g. on EXEC complete)
		// In that case pass ptr to empty string, not NULL ptr.
		// (NB: old async handling coded failure w/zero-length str -- now changed).
		LPCTSTR pszResultArg = NULL;
		if (hdata != NULL) {
			// Successful transaction: Copy returned data into result buf.
			DWORD len = DdeGetData(hdata, (LPBYTE) g_szResult, MAX_DDE_LEN, 0);
			if (len == 0)	
				g_szResult[0] = '\0';	// make zero-length str for success		
			pszResultArg = g_szResult;
		} 
		TRACE("Calling handler\n");
		(*pNotify->pfnProc)(pNotify->dwArg, pszResultArg);
	} else 
		TRACE("NULL handler specified; event ignored\n"); 

	// Now done with notify record:
	(void) free (pNotify); 
}
	
////////////////////////////////////////////////////////////////////////////
// OnDdeEvent -- DDE callback proc
//			  
// This is the event handler procedure we register to be fired by DDEML on 
// receipt of DDE events. It determines what events DDEML can send to us.
////////////////////////////////////////////////////////////////////////////
static BOOL CheckXactDisconnected();	// forward ref to sync xact code

static HDDEDATA CALLBACK OnDdeEvent(UINT uType, UINT uFmt, HCONV hconv, 
							 HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
							 DWORD dwData1, DWORD dwData2)
{
	switch (uType) 
	{
	case XTYP_REGISTER:			// notifies of new DDE server coming online
		// could use this to tell when Lisp server is up and ready to connect
		// show session time to indicate how long Lisp takes to start up
		TRACE("DDE: new server registered, session time=%d\n", HistTime());
		return (HDDEDATA) NULL;

	case XTYP_UNREGISTER:		// notifies of DDE server shutting down.
		TRACE("DDE: got unregister transaction\n");
		// could check if Lisp server has shut down
		return (HDDEDATA) NULL;

	case XTYP_ADVDATA:			// receive data update asked for earlier
		TRACE("DDE: got advise data update\n");
		// will only get these if we ever request links 
		return (HDDEDATA) DDE_FACK;

	case XTYP_XACT_COMPLETE:	// asynch transaction has completed
		OnXactComplete(hconv, dwData1, hdata);
		return (HDDEDATA) NULL;
	
	case XTYP_DISCONNECT:		// conversation disconnect
		TRACE("Received DDE disconnection\n");
		// If we were in process of disconnecting, g_hHelpConv would be NULL.
		// else is unexpected shutdown of peer.
		if (hconv == g_hHelpConv)	// 
		{
			LogEventf(EV_DDE_DISCONNECT, "");
			g_hHelpConv = NULL;
			g_bHelpSysRunning = FALSE; // as far as we're concerned

			// if we are inside modal loop awaiting completion of pending synchronous 
			// transaction, set completion status flag to abort it immediately.
			CheckXactDisconnected();

			// Let users know ASAP their tutor is dead. If app currently in DDE wait 
			// state UI is frozen, so defer till wait ends.
			if (! (theApp.GetMainFrame() && theApp.GetMainFrame()->InDdeWait()))
				theApp.DoWarningMessage(szLostConn);
			else // Set flag to notify user when wait is done
				g_bNotifyDisconnect = TRUE;
		}
		return (HDDEDATA) NULL;

	default: // All other XACTs ignored
		return (HDDEDATA) NULL;
	}
}

//////////////////////////////////////////////////////////////////////////////////
//
// DoExec: implements synchronous Exec transaction using async DDE internally 
// Runs modal loop until completion flagged. Possible to cancel from user event 
//
//////////////////////////////////////////////////////////////////////////////////

// state vars for the one and only pending synchronous transaction:
static DWORD g_CurSyncXact = NULL;	// id of current synchronous executing in process
static enum XActStatus 			// pending transaction states
{
	PENDING = 0,
	COMPLETED_OK,
	COMPLETED_FAIL,
	TIMEDOUT,
	CANCELLED,
	DISCONNECTED,
	NONE,
} 
g_CurSyncStatus = NONE;			// state flag for pending transaction
static UINT g_TimerId = NULL;   // id of failure timer for pending transaction

static void KillFailTimer()		
{
	if (g_TimerId) {
		::KillTimer(NULL, g_TimerId);
		g_TimerId = NULL;
	}
}

// Handlers for events related to current pending transaction:

// Completion. pszResult is the returned data, NULL if error  
static void OnSyncXactComplete(DWORD dwID, LPCTSTR pszResult)
{
	if (dwID != g_CurSyncXact) {
		TRACE("DDE OnSyncXactComplete: %x not pending transaction (=%x)!\n", dwID, g_CurSyncXact);
		return;
	}

	KillFailTimer();
	// flag success or failure of transaction
	g_CurSyncStatus = (pszResult != NULL) ? COMPLETED_OK : COMPLETED_FAIL;
}

// Failure timeout handler procedure: (not tied to a window).        
static void CALLBACK OnFailureTimeout(HWND hwnd, UINT idMsg, UINT idEvent, DWORD dwTime)
{
	if (idEvent != g_TimerId) {
		TRACE("OnFailureTimeout: Not current timer id!\n");
		::KillTimer(NULL, idEvent); // can't use it, might as well get rid of it
		return;
	}
	TRACE("DDE xact timeout %x\n", g_CurSyncXact);
	KillFailTimer();
	g_CurSyncStatus = TIMEDOUT;
}

// Handle disconnect; returns true if it aborted a sync transaction.
static BOOL CheckXactDisconnected()
{
	if (g_CurSyncXact) {
		KillFailTimer();
		g_CurSyncStatus = DISCONNECTED;
		return TRUE;
	}
	return FALSE;
}

//
// HelpSystemCancelCall - cancel current in-progress synchronous transaction
// should have no effect if not in one.
//
void HelpSystemCancelCall()
{
	if (! g_CurSyncXact) return; // ignore if no pending transaction

	KillFailTimer();

	// retrieve callback record for this transaction and free it
	CONVINFO ConvInfo;
	ConvInfo.cb = sizeof(CONVINFO);		
	if (! DdeQueryConvInfo(g_hHelpConv, g_CurSyncXact, &ConvInfo) ){
		TRACE("Cancel Call: Couldn't get Conv Info!!\n");
	} else {
		NOTIFYP pNotify = (NOTIFYP) ConvInfo.hUser;
		if (pNotify) free(pNotify);
	}

	// tell DDEML to discard the transaction.
	TRACE("DDE cancelling xact %x\n", g_CurSyncXact);
	DdeAbandonTransaction(g_idDdeInst, g_hHelpConv, g_CurSyncXact);

	g_CurSyncStatus  = CANCELLED;
	LogEventf(EV_DDE_CANCEL, "");

	// Post cancel notification to help system, so it can recover when finished
	HelpSystemQueueExec("(Cancel-Help-Request)");
}

// Programmatically set result. For use in remote event playing
// Send NULL for failure.
extern void HelpIfcSetResult(LPCTSTR pszResult)
{
	if (! g_CurSyncXact) {
		TRACE("HelpIfc -- result while not waiting!\n");
		return;
	}
	TRACE("Helpifc setting result %s\n", pszResult);

	// Save the result into the global result buf.
	strcpy(g_szResult, pszResult);

	// Flag the pending EXEC has complete
	OnSyncXactComplete(g_CurSyncXact, pszResult);

	// Must also instrument HelpSysGetResult to just return
	// in this case (since buffer is already set).
}

// In case we are remote viewing, we don't actually send the transaction
// or start the timer, but we do enter the modal loop wait state until result
// event is dispatched. (!!! no way to break out of this loop currently).
BOOL HelpSystemDoExec(LPCTSTR lpszCommand, UINT nTimeout)
{
	UINT	 cmd_len;
	HDDEDATA hData;
	DWORD	 xact_id;

	if (g_hHelpConv == NULL && !theApp.IsRemoteViewer()) 
		return FALSE;

	// only one synchronous transaction at a time allowed
	if (g_CurSyncXact != NULL) {
		TRACE("DDE: re-entrant sync transaction attempt\n");
		LogEventf(EV_DDE_FAILED, "%s", lpszCommand);
		return FALSE;
	}
	
	// Initiate the transaction in DDEML
	if (theApp.IsRemoteViewer())	// just simulate it
		TRACE("Helpifc awaiting result %s\n", lpszCommand);
	else
	{
		cmd_len = (UINT) strlen(lpszCommand) + 1; // don't forget NUL terminator!
		LogEventf(EV_DDE_EXEC, "%s", lpszCommand); // log as synchronous EXEC
		// Note: if helpsys crashed earlier, it is possible DDEML will deliver 
		// Disconnect callback to us inside the following call. Need to make sure
		// we don't popup message in this case since interface is frozen, even though
		// we haven't yet set the "in-transaction" global state variables. 
		// Transaction will fail in this case.
		hData = DdeClientTransaction(
					(LPBYTE) lpszCommand,
					(DWORD) cmd_len,
					g_hHelpConv,
					(HSZ) NULL,			// no item handle on Exec
					0,					// no format for Exec 
					XTYP_EXECUTE,
					TIMEOUT_ASYNC,		// signals ASYNC transaction
					&xact_id);			// receives transaction id
		if (hData == NULL) {
			TRACE("DDE: Failed to post Exec! |%s|\n", lpszCommand);
			LogEventf(EV_DDE_FAILED, "%s", lpszCommand);
			return FALSE;
		} else TRACE("DDE: Started Exec (%x) |%s|\n", xact_id, lpszCommand);
		
		// fill out notify record w/completion func and attach it to transaction id in DDEML
		// will be dispatched by our XACT_COMPLETE callback handler and freed there.
		NOTIFYP pNotify = (NOTIFYP) malloc(sizeof(NOTIFY));
		pNotify->pfnProc = OnSyncXactComplete;
		pNotify->dwArg   = xact_id;
		DdeSetUserHandle(g_hHelpConv, xact_id, (DWORD) pNotify);
	}

	// set global state vars for pending xact
	g_CurSyncXact = xact_id;
	g_CurSyncStatus = PENDING;

	// start failure timer
	if (! theApp.IsRemoteViewer()) 
		g_TimerId = ::SetTimer(NULL, NULL, nTimeout, OnFailureTimeout);

	// Run modal loop until status changes (by completion, timeout, or cancel).
	// Loop blocks w/o idle processing (App's clock timer handler forces idle update every sec)
	do {
		// MFC routine does Get/Translate/DispatchMessage, calling PreTranslateMsg filters
		AfxGetThread()->PumpMessage(); 
		
	} while (g_CurSyncStatus == PENDING);
	ASSERT(g_CurSyncStatus != PENDING);
	
	// save result status code
	enum XActStatus result = g_CurSyncStatus;

	// reset state, no pending xact anymore
	g_CurSyncXact = NULL;
	g_CurSyncStatus = NONE;
	KillFailTimer();//  should have been done on completion event, but can't hurt to make sure
	
	// Return success/failure, logging failure returns (including cancels).
	if (result != COMPLETED_OK) {
		TRACE("DDE Exec failed! reason=%d\n", result);
		LogEventf(EV_DDE_FAILED, "%s", lpszCommand);

		return FALSE;
	}
	return TRUE;
}

///////////////////////////////////////////////////////////////////////////////////
//
// Help system RPC functions. All client calls should go through one of these entries.
//
///////////////////////////////////////////////////////////////////////////////////

// For setting RPC call parameters:
static int g_nTimeout = HELPIFC_TIMEOUT; // time to wait before failure, milliseconds
static LPCTSTR g_pszStatusMsg = NULL;	// status msg to show, NULL => use default

extern void HelpIfcSetCallParms(int nTimeout, LPCTSTR pszMsg /*=NULL*/)
{
	g_nTimeout = nTimeout;
	g_pszStatusMsg = pszMsg;
}

static inline void RestoreDefaultParms() { HelpIfcSetCallParms(HELPIFC_TIMEOUT); };

//
// HelpSystemExecf -- format and execute a command, returning result string or NULL
//
// This encapsulates our use of the Help System interface for remote
// procedure calls. Takes a printf style variable argument list and
// formats into a buffer, sends the command with the current timeout, and 
// returns pointer to a result string. The return value is static data 
// overwritten each call and should be copied for long-term use. Wrapped
// in Mainframe's BeginDdeWait/EndDdeWait to disable, show wait cursor and standard
// message during call (could be moved into Mainfrm?).  
//
// Returns NULL on errors.
//
// This routine is also now instrumented for log playback: if app is in log playback mode,
// will normally simulate an RPC to the help system by fetching result from the log. 
//       
LPCTSTR HelpSystemExecf(LPCTSTR lpszFormat, ...)	
{
	// format command string into our buffer.
	// !!! Danger of overflow 
	va_list args;
	va_start(args, lpszFormat);
	vsprintf(g_szCmd, lpszFormat, args);
	va_end(args);

	// if we are running from log playback, and not calling help system
	// simulate the RPC using logged data 
	if (LogPlayerInPlayback() && ! LogPlayerCallHelp() )
	{
		TRACE("HelpIfc: simulating help system call: %s\n", g_szCmd);
		((CMainFrame*)AfxGetMainWnd())->BeginDdeWait(g_pszStatusMsg);
		BOOL bSuccess = LogPlayerGetHelpCallResult(g_szResult);
		((CMainFrame*)AfxGetMainWnd())->EndDdeWait();
		return bSuccess ? g_szResult : NULL;
	}

	// else send the command to the help system using current parms and fetch result.
	// note default timeout OK for fetching result, should be fast even if cmd slow.
	if (g_hHelpConv == NULL && ! theApp.IsRemoteViewer()) 
		return NULL;	// quick failure if help system not connected.

	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->BeginDdeWait(g_pszStatusMsg);
					
	BOOL bSuccess = HelpSystemDoExec(g_szCmd, g_nTimeout) && /* then */
					// use SendExec instead of DoExec for old-style sync transactions
					// HelpSystemSendExec(g_szCmd, g_nTimeout) && /* then */
					(HelpSystemGetResult(g_szResult, MAX_DDE_LEN) != 0);

	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->EndDdeWait();

	// revert to default parms after each call, for convenience.
	RestoreDefaultParms();

	// If got disconnect notification during wait, let them know their tutor is dead.
	if (g_bNotifyDisconnect) {
		theApp.DoWarningMessage(szLostConn);
		g_bNotifyDisconnect = FALSE;	// reset so don't warn again
	}

	return bSuccess ?  g_szResult : NULL;
}

// 
// HelpSystemSendf -- format and post a command to the help system w/no result 
//                    required 
// Returns success or failure of the post operation. Note this is not
// success or failure of the command processing.
//
// Arguments like Execf but doesn't fetch result, so more efficient if result not 
// needed. Also uses async mode transactions, so returns without wait for ack or 
// message pumping in the process. Suitable for notification messages to the help 
// system. 
//       
BOOL HelpSystemSendf(LPCTSTR lpszFormat, ...)	
{
	// If simulating help system from log file, no result needed, but still
	// must recreate notification to mainframe since may clear hint window
	if (theApp.IsRemoteViewer() ||
		(LogPlayerInPlayback() && ! LogPlayerCallHelp()) ) {
		if (theApp.GetMainFrame())
			theApp.GetMainFrame()->OnDdeSend();
		return TRUE;
	}

	// else send to real help system
	if (g_hHelpConv == NULL) return FALSE;

	// format string into our buffer.
	// !!! Danger of overflow 
	va_list args;
	va_start(args, lpszFormat);
	vsprintf(g_szCmd, lpszFormat, args);
	va_end(args);

	// Notify mainframe of these too -- used to reset hint on deletes, async eq checks
	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->OnDdeSend();

	return (HelpSystemQueueExec(g_szCmd));
}

// 
// HelpSystemExecAsync -- post an asynchronous command to the help system
//						  registering callback to fire on completion.
//
// Notify func is called as (*pfnNotify)(dwCookie, pszResult)
// where pszResult points to returned string or NULL for error.
//
// Returns success or failure of the post operation. Note this is not
// success or failure of the command processing
//       
BOOL HelpSystemExecAsync(LPCTSTR pszCmd, PROCP pfnNotify, DWORD dwCookie)
{
	if (! HelpSystemSendf(pszCmd))
		return FALSE;
	
	// !!! This pumps result request into the pipe right after the exec, but
	// if async exec failed, fetched result may not be valid -- could be one lying
	// around from earlier exec !! For now we are assuming help system will do the 
	// right thing and trap the earlier error and either fail the result request 
	// in this case, or at least return a non-useful value like the empty string 
	// in this case. 
	HelpSystemQueueResultReq(pfnNotify, dwCookie); // ! no status return
	return TRUE; 
}


// last line: close big #else
#endif	// ! HELPIFC_TCP



