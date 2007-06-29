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

// buffer sizes:
PRIVATE const int MAX_CMD_LEN = 512;	// Plenty, cmds normally < one line
PRIVATE const int MSG_BUF_LEN = 4096;	// ~2 pgs enough for even long hint result msgs

//
// CMsgSock -- custom socket class to handle events on our socket
//
// Provides functions for sending and receiving messages in our little 
// protocol over TCP/IP stream, and handles other network events, in some
// cases relaying them to other functions in this module. 
//
class CMsgSock : public CSocket
{
// Construction
public:
	CMsgSock(): m_bConnected(FALSE), m_nMsgCounter(0) {};
	virtual ~CMsgSock() {};

// Attributes
public:
	enum { PORT = 12345 };		// easy to remember default port
	BOOL IsConnected()			// true if connection is established
		{ return m_bConnected; };
protected:
	BOOL m_bConnected;			// remembers whether connected
	friend class CServerSock;   // can set m_bConnected flag

// Operations
public:
	// Use DoConnect since Connect is not virtual.
	BOOL DoConnect( LPCTSTR lpszHostAddress, UINT nHostPort )
		{ return (m_bConnected = Connect(lpszHostAddress, nHostPort)); };

	virtual void Close()
	{ 
	  CSocket::Close();
	  m_bConnected = FALSE;
	}

	BOOL CMsgSock::SendExecMsg(LPCTSTR pszCmd, int& nId);
	BOOL CMsgSock::SendNotifyMsg(LPCTSTR pszCmd);

protected:	
	BOOL SendMsg(const CString& strMsg);
	// counter for unique message ids, used to match replies to commands
	WORD m_nMsgCounter;		// rolls over at 16-bit maxint (plenty for us!)

// Overrides
public:
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMsgSock)
	public:
	virtual void OnReceive(int nErrorCode);
	virtual void OnClose(int nErrorCode);
	//}}AFX_VIRTUAL

	// Generated message map functions
	//{{AFX_MSG(CMsgSock)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

// Implementation
protected:
	CString m_strBuf;			// collects unparsed data received
	void DispatchMsg(LPCTSTR pszMsg);
};

// the socket used by this module for msgs over TCP/IP 
PRIVATE CMsgSock s_sockMsgs;

// Handle to the help system process if we started it.
PRIVATE HANDLE hHelpSysProcess = NULL;					

//
// Connection management
//

// Internal macro tests if our socket is connected to real help system 
PRIVATE inline BOOL SockIsConnected()
{
	return s_sockMsgs.IsConnected();	
}

// Following API makes real IPC connection state publicly available, if needed
PUBLIC BOOL HelpSystemReallyConnected() 
{ 
	return SockIsConnected(); 
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
	return (SockIsConnected());
}


// CServerSocket: Listening server socket class: 
// Once started, just accepts 1 connection into CMsgSock
//
class CServerSock : public CSocket
{
public:
	UINT m_nPort;
	UINT m_nTimerID;

	BOOL ListenForConnection()	// starts listening for connection requests
	{
		m_nPort = 0;
		if (! (Create() && Listen(1)) ) {
		   TRACE("Failed To Init Listening Server Socket!");
		    return FALSE;
		} // else success:
		CString strAddr; ;
		GetSockName(strAddr, m_nPort);
		TRACE("Listening for connection at port :%d\n", m_nPort);
		return TRUE;
	}

	void DoAccept();

protected:
     virtual BOOL OnMessagePending();

};

PRIVATE CServerSock s_sockServer;		// listening server socket

void CServerSock::DoAccept()
{
	TRACE("Server socket: Waiting to accept connection request\n");

	// make sure this is free to accept connection into.
	s_sockMsgs.Close();

	// Set timer to give up waiting for connection after certain time (ms)
	m_nTimerID = ::SetTimer(NULL, 0, 20000, NULL);
	TRACE("Set accept timer %d\n",m_nTimerID);

	// set timeout around this.
	if (Accept(s_sockMsgs)) 
	{
		// flag msg sock as connected
		s_sockMsgs.m_bConnected = TRUE;

		// Stop listening, only one subscriber allowed (but might want to 
		// allow help re-attaching while developing)
		Close();
	
		// Get peer address for diagnostic.
		CString strAddr; 
		s_sockMsgs.GetPeerName(strAddr, m_nPort);
		TRACE("Server sock: connected to %s at %d\n", strAddr, m_nPort);
// TEMP: for debugging, note when connection was made in log
		//Logf("ATTACHED %s", inet_ntoa(inPeer));
// END TEMP
	} 
	else {
		TRACE("Server Socket: Connection Accept failed!\n");
	}
	::KillTimer(NULL, m_nTimerID);
}

// CSocket pumps windows messages during blocking calls. By default it does not
// process any messages but WM_PAINT's to redraw user interface when needed, plus its
// own internal timer set to keep processing going. We have to override this function 
// in order to process the WM_TIMER message scheduled by our failure timeout and set 
// flag to break out of the model loop and fail the accept. See MS KB article 138692
//
// NOTE: Two other WM_TIMER messages also arise in the queue at this point: Since the
// connection is made while the splash screen is up, there is its dismissal timer 
// (id = 1). There is also the mainframe update timer (id = 0x1000) set to trigger 
// periodic UI updates for the sake of the status bar clock. Since the default
// OnMessagePending in the base class doesn't process these, we have to make sure 
// these other timer msgs are consumed from the queue in order to get our failure 
// timeout processed.
// We could remove and dispatch other timer messages with DispatchMessage(&msg), but 
// we do want to suppress the splash screen dismissal while waiting for connection. 
// And there is no need to update the mainframe UI before mainframe is shown. So we 
// just eat the other application timers during this loop. The code diverges from
// from the KB 138692 example code by setting the PM_REMOVE flag when peeking 
// for timer messages, which removes them from the queue.
// 
// Note also: default does not call PreTranslateMessage either, which means modeless
// splash screen does not receive keyboard or mouse message through its PreTranslate
// hook. So can't dismiss it with a click during this wait either, which is also
// what we want.
BOOL CServerSock::OnMessagePending()
{
	 // Remove all timer messages during blocking call
     MSG msg;
     if(::PeekMessage(&msg, NULL, WM_TIMER, WM_TIMER, PM_REMOVE))
     {
	   TRACE("Peeking at WM_TIMER id=%d\n", msg.wParam);
       if (msg.wParam == (UINT) m_nTimerID) // our failure timeout
       {     
         CancelBlockingCall();
         return FALSE;  // No need for idle time processing.
       }
	 };

     return CSocket::OnMessagePending();
}



// Helper to launch application given command line
// RETURNS process handle, NULL on failure
// Arg is passed as command line to StartProcess: Best if program name has 
// quoted full path in case path has spaces, e.g as follows: 
//		"C:\Program Files\Andes.exe"
PUBLIC HANDLE StartProcess(LPCTSTR pszCmdLine, LPCTSTR pszWorkingDir=NULL)
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

//
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
	CString strExeDir = g_strAndesDir; // path of helpys exe's directory
	CString strExePath;				   // full path of helpsys exe
	
	int nTries;

	// if we can connect or are already connected, do nothing.
	if (HelpSystemConnect()) 
		return TRUE;

	// Else initial connect failed. Try to start local helpsys and connect
	switch (s_sockMsgs.GetLastError()) {
		// Errors that are most likely due to the help system not running.
		case WSAETIMEDOUT:
		case WSAEINVAL:
		case WSAECONNREFUSED:
		case WSAEADDRNOTAVAIL:
		case WSAENETDOWN:
		{
			// Failed to connect, try starting local copy help system.

			// NEW: Create server socket for it to call us back on
			s_sockServer.ListenForConnection();
			// NEW: form callback port argument part for lisp command line
			CString strPortArg;
			strPortArg.Format(" -- %d", s_sockServer.m_nPort);
			
			// quote path arg since AndesDir may have spaces, e.g. C:\Program Files\Andes\ 
#ifndef ATLAS // ANDES VERSION
			// For Andes2: start Andes2.exe 
			// Note we must find and run helpsys in C:\Andes2 no matter 
			// what is in registry entry for existing Andes installation.
			// strExeDir = "C:\\Andes2\\";   // for Andes2 demo only!!
			strExePath = "\"" + strExeDir + "Andes2.exe" + "\"";	
#else  // ATLAS VERSION
			strExePath = "\"" + strExeDir + "AndesAtlas.exe" + "\"";
#endif // ATLAS VERSION
			// allow registry setting to modify Lisp cmdline args for showing console 
			// default if unset or empty is to suppress console with +c
			CString strLispArgs = theApp.GetProfileString("Settings", "LispArgs", "");
			// if unset or empty string, default is to not suppress console
			if (strLispArgs.IsEmpty()) strLispArgs = "+c";
			if (! strLispArgs.IsEmpty())
				strExePath += " " + strLispArgs;
			// NEW: add callback port arg
			strExePath += strPortArg;

			TRACE("Helpsys connect failed, trying to exec %s \n", strExePath);
			hHelpSysProcess =  StartProcess(strExePath, strExeDir);
			if (hHelpSysProcess == NULL) {
				TRACE("StartProcess failed, code = %d\n", (int) ::GetLastError());
				return FALSE;
			}

			// Else launched it OK. Note in log:
			LogEventf(EV_START_HELP, strExePath); // added Andes 7.0.0.2.
		
			
			// Wait until child process is ready for input to give it time to init.
			//::WaitForInputIdle(hHelpSysProcess, 1000 ); 
#if 0
			// try connecting again on local machine. Loop makes multiple attempts 
			// with pauses in case helpsys needs more time before ready to accept
			int nTimeStart = HistTime();	// session time we started trying (secs)
			for (nTries = 1; (!SockIsConnected()) && ((HistTime() - nTimeStart) < 30); nTries++) 
			{
				// pause between tries (fifth of a second)
				if (nTries > 1)		 // no pause for first try
					::Sleep(200);

				// and try to connect again
				s_sockMsgs.Close();  // since HelpSysConnect creates again.
				TRACE("Local helpsys connection attempt %d\n", nTries);
				HelpSystemConnect("localhost");
			}
#else
			s_sockServer.DoAccept();
#endif 
			
			// Logf("END_ATTEMPT_CONNECT %d %d", nTries, SockIsConnected());
 			return SockIsConnected();
			break; 
		}

		default:
			s_sockMsgs.Close();
			return FALSE;
	}

// WOZ only -- make connection early
//	return HelpSystemConnect();
//
// return TRUE;
//
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

	// host is specified in registry entry, port now always default
	// Use localhost as default if value not in reg, or value is empty string.
	CString strHelpAddr = pszAddress;
	if (pszAddress == NULL)
		strHelpAddr = theApp.GetProfileString("Settings", "HelpAddress", "");
	if (strHelpAddr.IsEmpty()) 
		strHelpAddr = "localhost";
	
	// !!! could accept port number with host:port notation
	int nPort = CMsgSock::PORT;
	
	// create socket as needed (may be called again for second attempt)
	int nError;
	if (! s_sockMsgs.Create()) {
		nError = s_sockMsgs.GetLastError();
		TRACE("Couldn't create socket!. Error=%d\n", nError);
	}
	// try to make connection on socket
	if (! s_sockMsgs.DoConnect(strHelpAddr, nPort)) {
		nError = s_sockMsgs.GetLastError();
		TRACE("Couldn't connect to %s:%d. Error=%d\n", strHelpAddr, CMsgSock::PORT, nError);
		// Caller should report failure as appropriate
	} else
		TRACE("Connected to helpsys at %s:%d\n", strHelpAddr, nPort);

	return (SockIsConnected());
}

PRIVATE void OnDisconnect();	// fwd: handler in xact management code
PRIVATE void CancelAllXacts();	// fwd: xact management cleanup func

// 
// OnClose -- handle remote close event on socket
//
void CMsgSock::OnClose(int nErrorCode)
{
	TRACE("Helpsys connection closed by peer\n");
	LogEventf(EV_DDE_DISCONNECT, "");
	
	// Close socket from our end
	Close();

	// Notify transaction manager -- if disconnected while inside a synchronous 
	// message, must break out of loop and notify afterwards
	OnDisconnect();
}

// 
// HelpSystemDisconnect -- Terminate conn and free resources on session end.
//
PUBLIC void HelpSystemDisconnect()
{
	// Discard all pending transaction records.
	CancelAllXacts();

	// Post message to action interpreter to terminate. This should cause
	// close of socket from the other end followed by termination of process.
	HelpSystemSendf( "(exit-andes)" ); // note no reply expected

	// shut down connection from our side. Not strictly necessary anymore, since 
    // exit-andes causes remote end to close, but good style to do so anyway. 
	TRACE("Closing connection to help system\n");
#if 1
	// Shutdown our end of the pipe before releasing the socket. This sends a FIN which can be read as EOF by the other
	// end. We shutdown for receiving as well as sending since no more data is expected from the helpsys. 
	s_sockMsgs.ShutDown(2);
#else	
	// Do an abortive close of the socket (sends reset message, normally used for error),
	// rather than the normal TCP close sequence. This is OK since we know the session
	// is done and no more data can come to us. Although it's considered inelegant to "slam the connection
	// shut" this way, it's acutally more efficient for us since this way the socket won't persist at the TCP layer
	// for some time in the FIN_WAIT states. That means socket addresses can be immediately reused in a subsequent session 
	// without problems. One might worry that help system could fall behind in  processing requests queued 
	// in the pipe, but our interactions are now all synchronous -- we wait for a reply -- so this shouldn't happen. 
	// The only outstanding message is the somewhat unnecessary final exit-andes call (though it is nice to see in the 
	// logs that it took place.) The helpsys will exit in the same way on receiving a reset error. 

	 // following option settings make the Close call do a "hard" close (resets connection).
	// Don't do shutdown in this case.
	{ struct linger l = { /*l_onoff:*/ 1, /*l_linger:*/ 0};
		s_sockMsgs.SetSockOpt(SO_LINGER, &l, sizeof(l)); }
#endif 0		
	s_sockMsgs.Close();

	// if we started helpsys, ensure process is terminated when we're done.
	// We loop polling and waiting to allow it to shut down itself normally after 
	// exit-andes. If it's still running after certain number of tries, it may be stuck
	// in a loop, so we terminate it with extreme prejudice to ensure its gone.
	// Its final cleanup won't take place in this case but that's probably acceptable.
	// Note this would have to change if we expected helpsys to do time-consuming actions on close.
	if (hHelpSysProcess != NULL)  // we started it
	{
		int nPolls = 0;
		DWORD dwExitCode;
		BOOL bStillRunning = FALSE;
		while (++nPolls <= 10		// should be quick, so 2 sec max should be plenty
			   && (bStillRunning = (::GetExitCodeProcess(hHelpSysProcess, &dwExitCode)
								      && dwExitCode == STILL_ACTIVE))){ 
			::Sleep(200);			// wait 1/5 sec between polls		
		}
		if (bStillRunning) {
			TRACE("HelpSys still running -- killing\n");
			::TerminateProcess(hHelpSysProcess, 1);
		}
		
		// Release handle in any case
		::CloseHandle(hHelpSysProcess);
	}
}


/////////////////////////////////////////////////////////////////////////////
//
// Transaction management functions: maintains info on asynchronous transactions
//
/////////////////////////////////////////////////////////////////////////////

// 
// Info on list of async EXEC transactions awaiting replies:
// 
typedef struct Callback		// completion callback information
{
	PROCP pfnProc;			// client's procedure to call
	DWORD dwArg;			// client's argument to pass
	Callback() : pfnProc(NULL), dwArg(0) {};
} 
NOTIFY, *NOTIFYP;			// "CALLBACK" already defined in windows

struct CXact				// pending transaction info
{
	int		nId;			// transaction id
	NOTIFY  m_callback;		// completion routine to call

	CXact(int nIdArg) : nId(nIdArg) {};
};

// List of pending transactions:
typedef CTypedPtrList<CPtrList, CXact*> CXactList;
PRIVATE CXactList s_xactsPending;

PRIVATE CXact* AddPendingXact(int nId) 
{	
	CXact* pXact = new CXact(nId);
	s_xactsPending.AddTail(pXact);

	return pXact;
}

PRIVATE CXact* FindPendingXact(int nId)
{
	for (POSITION pos = s_xactsPending.GetHeadPosition(); pos != NULL; ) {
		CXact* pXact = s_xactsPending.GetNext(pos);
		if (pXact->nId == nId)
			return pXact;
	}
	return NULL;
}

PRIVATE void RemovePendingXact(int nId)
{
	CXact* pXact = FindPendingXact(nId);
	if (pXact == NULL) {
		TRACE("RemoveXAct: %d not found\n", nId);
		return;
	}

	POSITION pos = s_xactsPending.Find(pXact);
	s_xactsPending.RemoveAt(pos);

	// free all resources for this transaction
	delete pXact;
}

PRIVATE void CancelAllXacts()	// for cleanup at end
{
	while (! s_xactsPending.IsEmpty())
		delete s_xactsPending.RemoveHead();
}

PRIVATE void SetCallback(int nId, PROCP pfnNotify, DWORD dwArg)
{
	CXact* pXact = FindPendingXact(nId);
	if (pXact == NULL) {
		TRACE("SetCallback: %d not found\n", nId);
		return;
	}

	pXact->m_callback.pfnProc = pfnNotify;
	pXact->m_callback.dwArg = dwArg;
}

PRIVATE NOTIFYP GetCallback(int nId)
{
	CXact* pXact = FindPendingXact(nId);
	if (pXact == NULL) return NULL;

	return &pXact->m_callback;
}

PRIVATE void AbandonTransaction(int nId)
{
	RemovePendingXact(nId);
}

// Handle completion of an async transaction. 
// Dispatched here from OnReceive in case msg is reply.
// NULL data means command failed.
// We fire registered completion callback if any.
PRIVATE void OnXactComplete(int nId, LPCTSTR pszData)
{
	TRACE("EXEC %d completed %s ", nId, pszData ? "OK..." : "Failed");
	 	
	// Retrieve pointer to callback info we stashed under xact ID 
	NOTIFYP pNotify = GetCallback(nId);
	if (pNotify == NULL) {	
		TRACE("Handler not found, event ignored\n");
		return;
	}
			
	// Fire client's completion notification handler, passing ptr to returned
	// data and saved cookie for context arg. Data ptr points into a temp
	// receive buf, callee must copy to keep.
	if (pNotify->pfnProc != NULL) 
	{
		TRACE("Calling handler\n");
		(*pNotify->pfnProc)(pNotify->dwArg, pszData);
	} else 
		TRACE("NULL handler, event ignored\n"); 

	// Now done with transaction:
	RemovePendingXact(nId);
}

//--------------------------------------------------------------------------
// For sending transaction messages.
//
// All msgs in our simple protocol are character strings begining with
// at least one "header" character and terminated by a single newline char.
// The first character of the msg indicates the message type. 
// Newlines are not allowed in the message body. NUL characters should never
// be included either, to allow processing by C string routines.
//
// EXEC For commmands requiring a result client sends msg of form 
//			?ID:command-text\n
// where ID is a numeric command id. The body text is an application-defined
// command string to be executed by the remote server. 
// Ex. The following illustrates a complete EXEC message:
//			?23:(read-problem-info P5-1 1)\n
// The "\n" indicates the single NL terminator.
// 
// REPLY msg returns successful command execution result from server as
//          <ID:result-text\n
// where the command id from the EXEC is included in the header. We don't
// interpret the result text in any way in this module. It is possible the
// result text is empty -- caller will see an empty string in this case.
//
// NACK In case command execution failed server may return
//			*ID:optional-error-msg-text\n
// with command id. Error-msg if included should be suitable for display.
//
// NOTIFY For commands where no result is needed client sends just
//			!command-text\n
// i.e. no command id or colon. No reply should be sent for these.
//	
// CMD We also accept unsolicited commands from server of form 
//			!command-text\n
// and pass them to the application's command string interpreter.
//
// The protocol is asymmetrical in that we as clients don't accept commands
// that return result data to the server. (Could be changed, but no need.)
//
// At the lowest level the protocol is asynchronous in that multiple
// outstanding msgs may be pumped to the server without waiting for
// a reply to the first. However, replies must be returned in order 
// to ensure correctness (should work like a message queue.) 
// Higher-level code below implements synchronous (blocking) calls for 
// the app on top of the msg layer, so client code is free to use either 
// as appropriate. This layering parallels our alternate code using the 
// asynchronous DDE msg passing protocol on the local machine.
// 
//---------------------------------------------------------------------------

// message type prefixes (opcodes):
#define chEXEC	 '?'				// client -> server, want result
#define chREPLY  '<'				// client <- server EXEC reply
#define chNACK   '*'				// client <- server EXEC failure reply
#define chNOTIFY '!'				// client -> server, no result
#define chCMD	 '!'				// client <- server, no result (same as NOTIFY)

// SendMsg -- Ship complete msg to the receiver's queue
// 
// Returns: TRUE for success, FALSE otherwise, and traces. 
// Success just means that data written successfully.
BOOL CMsgSock::SendMsg(const CString& strMsg)
{
	// verify NL terminator already included
	ASSERT(strMsg.GetLength() > 0);
	ASSERT(strMsg[strMsg.GetLength()-1] == '\n');
	
	// Ship it down the pipe
	TRACE("TCP Sending msg: %s", strMsg);
	int result = Send(strMsg, strMsg.GetLength()); // don't send NUL	
	return result != SOCKET_ERROR;
}

// Format and send remote function call message ("EXEC")
// Fills in transaction id out parameter; Caller can use to attach callback
BOOL CMsgSock::SendExecMsg(LPCTSTR pszCmd, int& nId)
{
	// prepend header with msg id for matching replies
	CString strMsg;
	strMsg.Format("%c%d:%s\n", chEXEC, ++m_nMsgCounter, pszCmd);

	// return id via out parameter
	nId = m_nMsgCounter;

	// Post the message
	BOOL bSuccess = SendMsg(strMsg); 

	// Save record on list of outstanding msgs, so we know if there is any
	// when data arrives and can remember whether our client is awaiting a reply
	// when that happens. Uses default NULL callback, can be set by client.
	if (bSuccess) 
		AddPendingXact(nId);

	return bSuccess;
}

// format and send notification msg, where no return value expected ("NOTIFY")
BOOL CMsgSock::SendNotifyMsg(LPCTSTR pszCmd)
{
	// send w/distinct prefix but w/o msg counter header 
	// to indicate no result wanted
	CString strMsg;
	strMsg.Format("%c%s\n", chNOTIFY, pszCmd);
	return SendMsg(strMsg);
}

//
// For handling incoming data
//

// OnReceive: Handle incoming data from peer aggregating into buffer.
// Dispatch complete msgs to handlers as soon as they are received.
//
// In practice always get one complete message on one receive with
// short messages over LAN, but following code should work in general case
// in which received data may span message boundaries. 
//
void CMsgSock::OnReceive(int nErrorCode)	
{
	// Read data into temp buf, large enough to hold one complete msg.
	char szTemp[MSG_BUF_LEN];
	int nRead = s_sockMsgs.Receive((void*)szTemp, MSG_BUF_LEN - 1); // leave room for NUL
	if (nRead == SOCKET_ERROR) return;

	// append NUL to make current chunk into C string
	szTemp[nRead] = '\0';
	TRACE("TCP got %d bytes: %s", nRead, CString(szTemp).Left(480)); // TRACE limit = 512 ch

	// Append current chunk to any unparsed data left in buf from prior reads
	m_strBuf += szTemp;

	// Parse messages out of buffer by splitting at NL terminator,
	// and dispatch them for processing. Usually only one.
	// We quit loop if connection was aborted inside msg handler
	int iMsgEnd;
	while (m_bConnected && (iMsgEnd = m_strBuf.Find('\n')) != -1)
	{
		// extract msg string left of NL, leaving buffer holding remainder
		CString strMsg = m_strBuf.Left(iMsgEnd); // omits NL terminator
		m_strBuf = m_strBuf.Mid(iMsgEnd + 1);

		// dispatch this message
		DispatchMsg(strMsg);
	}
}

// Dispatch msg to handler based on type.
// Arg should have been converted to C style string.
void CMsgSock::DispatchMsg(LPCTSTR pszMsg)
{
	// switch on msg type code in first char
	int nType = pszMsg[0];			// msg type code
	LPCTSTR pszRest = pszMsg + 1;	// points at rest in buffer

	// check for command reply header with sequence number
	int nId;
	if (nType == chREPLY && sscanf(pszRest, "%d:", &nId) == 1)
	{
		// pass result data string to async completion handler
		LPCTSTR pszData = strchr(pszRest, ':') + 1;
		OnXactComplete(nId, pszData);
		return;
	}

	// check for command failure reply header with sequence number
	if (nType == chNACK && sscanf(pszRest, "%d:", &nId) == 1) 
	{
		// pass NULL to async completion handler to indicate failure
		// currently we don't do anything with any error message.
		// (there are no expected failures, it's usually a Lisp error.)
		OnXactComplete(nId, NULL);
		return;
	}

	// Else look for unsolicited command string from server 
	if (nType == chCMD) 
	{
		// Forward to app's command string handler (it will do the logging)
		// note some commands start extended modes, e.g. showing mini-lesson.
		theApp.OnHelpSysCommand(pszRest);	
		return;
	}

	// get here means bad message
	TRACE("Malformed msg: |%s| ignored\n", pszMsg);
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

// state vars for the one and only pending synchronous transaction:
PRIVATE DWORD s_CurSyncXact = NULL;	// id of current synchronous exec in process

PRIVATE enum XActStatus 			// pending transaction states
{
	NONE = 0,						// used if no pending transaction
	PENDING,						// awaiting reply
	COMPLETED_OK,					// got success reply
	COMPLETED_FAIL,					// got NACK indicating remote failure
	TIMEDOUT,						// no reply by timeout interval
	CANCELLED,						// user cancelled
	DISCONNECTED,					// connection went down while waiting
} 
s_CurSyncStatus = NONE;				// state flag for pending transaction
PRIVATE XActStatus s_LastResult=NONE;// final result of last completed xact.
PRIVATE UINT s_TimerId = NULL;		// id of failure timer for pending transaction
 
PRIVATE void KillFailTimer()		
{
	if (s_TimerId) {
		::KillTimer(NULL, s_TimerId);
		s_TimerId = NULL;
	}
}

// Handlers for events related to current pending sync transaction:

// Completion. pszResult is the returned data (points to temp buf) or NULL if error 
// Copy returned data into s_szResult. 
PRIVATE void OnSyncXactComplete(DWORD dwID, LPCTSTR pszResult)
{
	if (dwID != s_CurSyncXact) {
		TRACE("DDE OnSyncXactComplete: %x not pending transaction (=%x)!\n", dwID, s_CurSyncXact);
		return;
	}

	KillFailTimer();

	// save data into global result buf
	if (pszResult) {
		ASSERT(strlen(pszResult) < MSG_BUF_LEN); // < MAX so room for final NUL
		strcpy(s_szResult, pszResult);
	} else
		s_szResult[0] = '\0';	// set to empty string on failure

	// flag success or failure of transaction
	s_CurSyncStatus = pszResult ? COMPLETED_OK : COMPLETED_FAIL;
}

// Failure timeout handler procedure: (not tied to a window).        
PRIVATE void CALLBACK OnFailureTimeout(HWND hwnd, UINT idMsg, UINT idEvent, DWORD dwTime)
{
	if (idEvent != s_TimerId) {
		TRACE("OnFailureTimeout: Not current timer id!\n");
		return;
	}
	TRACE("DDE xact timeout %x\n", s_CurSyncXact);
	KillFailTimer();
	s_CurSyncStatus = TIMEDOUT;
}

// 
// Handle unexpected disconnect:
// Socket close handler calls this to notify in case of unexpected close.
// Checks if disconnect interrupted synchronous transaction:
// if not, just alerts user to failure.
// error message on disconnect
PRIVATE const char szLostConn[] =
"Lost connection to Tutor\nAlthough you may continue to work, feedback and hints will not be available";

PRIVATE void OnDisconnect()
{
	// if in a sync transaction, flag it to break. Will notify
	// user after break out of modal loop
	if (s_CurSyncXact) {
		KillFailTimer();
		s_CurSyncStatus = DISCONNECTED;
	} 
	else // otherwise, let students know their tutor is dead.
	{
		theApp.DoWarningMessage(szLostConn);
		// make sure app is not hung in tutor control mode
		theApp.SetTutorMode(FALSE);
	}
}

//
// HelpSystemCancelCall - cancel current in-progress synchronous transaction
// should have no effect if not in one.
//
PUBLIC void HelpSystemCancelCall()
{
	// ignore if no pending transaction
	if (! s_CurSyncXact) return; 

	KillFailTimer();

	// Discard the transaction.
	TRACE("DDE cancelling xact %x\n", s_CurSyncXact);
	AbandonTransaction(s_CurSyncXact);

	s_CurSyncStatus = CANCELLED;
	LogEventf(EV_DDE_CANCEL, "");

	// Post cancel notification to help system. In certain cases it 
	// can use this to recover after finished processing cancelled transaction.
	HelpSystemSendf("(Cancel-Help-Request)");
}

// Programmatically set result. For use in log event playing
PUBLIC void HelpIfcSetResult(LPCTSTR pszResult)
{
	if (! s_CurSyncXact) {
		TRACE("HelpIfc -- RPC result event when not waiting!\n");
		return;
	}
	TRACE("Helpifc setting result %s\n", pszResult);

	// Fire the completion handler.
	OnSyncXactComplete(s_CurSyncXact, pszResult);
}

// 
// DoExec -- worker routine to run a synchronous transaction, running
// modal message loop while waiting for completion or failure. 
// Returns TRUE for success, leaving result in s_szResult
//         FALSE for failure
//
PRIVATE BOOL DoExec(LPCTSTR lpszCommand, UINT nTimeout)
{
	// no-op if not connected. (Not used when running from log).
	if (!SockIsConnected() && !theApp.IsRemoteViewer()) 
		return FALSE;

	// only one synchronous transaction at a time allowed
	if (s_CurSyncXact != NULL) {
		TRACE("HelpIFC: re-entrant sync transaction attempt\n");
		return FALSE;
	}
	
	// Initiate the transaction
	int xact_id = -1;
	if (theApp.IsRemoteViewer()) {	// just simulate it
		TRACE("Helpifc awaiting result %s\n", lpszCommand);
		static int nSimulated = 0;
		xact_id = ++nSimulated;	// any id will do
	} 
	else {
		if (!s_sockMsgs.SendExecMsg(lpszCommand, xact_id))
			return FALSE;

		// set completion callback for the pending transaction 
		SetCallback(xact_id, OnSyncXactComplete, xact_id);
	}

	// set state vars for pending xact
	s_CurSyncXact = xact_id;
	s_CurSyncStatus = PENDING;

	// start failure timer
	if (! theApp.IsRemoteViewer())
		s_TimerId = ::SetTimer(NULL, NULL, nTimeout, OnFailureTimeout);

	// Run modal loop until status changes (by completion, timeout, or cancel).
	// Loop blocks w/o idle processing (App's clock timer handler forces idle update every sec)
	BOOL bQuit = FALSE;
	do {
		// MFC routine does Get/Translate/DispatchMessage, calling PreTranslateMsg filters
		bQuit = !AfxGetThread()->PumpMessage(); 
		
	} while (s_CurSyncStatus == PENDING && !bQuit);
	if (bQuit) {
		s_CurSyncStatus = CANCELLED;
		AfxPostQuitMessage(0);		// repost to containing msg loop
	}
	ASSERT(s_CurSyncStatus != PENDING);
	
	// timer should have been killed in completion handler, but can't hurt to make sure
	KillFailTimer();
	
	// save result status code. Status code is important in case disconnected. 
	s_LastResult = s_CurSyncStatus;

	// reset state vars, no pending xact anymore
	s_CurSyncXact = NULL;
	s_CurSyncStatus = NONE;

	// Note failure returns (including cancels).
	if (s_LastResult != COMPLETED_OK) 
		TRACE("DDE Exec failed! reason=%d\n", s_LastResult);

	// Return success/failure
	return (s_LastResult == COMPLETED_OK);
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
	if (!SockIsConnected() && !theApp.IsRemoteViewer()) 
		return NULL;	

	// Log the call
	LogEventf(EV_DDE_EXEC, "%s", szCmd);

	// Update UI to show visible wait state, message
	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->BeginDdeWait(s_pszStatusMsg);
	
	// run the transaction.
	// s_szResult should contain result or empty string on return		
	BOOL bSuccess = DoExec(szCmd, s_nTimeout);

	// Restore the normal UI
	if (theApp.GetMainFrame())
		theApp.GetMainFrame()->EndDdeWait();

	// for convenience, revert to default parms after each call, 
	// since almost all calls just use defaults. 
	RestoreDefaultParms();

	// Log the result
	if (bSuccess)
		LogEventf(EV_DDE_RESULT, "|%s|", s_szResult);
	else
		LogEventf(EV_DDE_FAILED, "%s", szCmd);

	// If transaction failed due to disconnect, let them know their tutor is dead.
	if (!bSuccess && s_LastResult == DISCONNECTED) {
		OnDisconnect(); // should show message this time.
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
	if (!SockIsConnected()) return FALSE;

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
	BOOL bSuccess = s_sockMsgs.SendNotifyMsg(szCmd);
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
	// initiate the command, saving xact id
	int nId = -1;
	BOOL bSuccess = s_sockMsgs.SendExecMsg(pszCmd, nId);

	// and register client's callback to fire on completion
	if (bSuccess) {
		SetCallback(nId, pfnNotify, dwCookie);
	}
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