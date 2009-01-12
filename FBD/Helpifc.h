///////////////////////////////////////////////////////////////////////
//
// HelpIfc.h -- Help System Interface module declarations.
//
// Because there is only one help system interface for the whole application,
// the API to this module is currently a set of global functions. 
// 
///////////////////////////////////////////////////////////////////////

//
// Initialization/Termination of connection.
//
extern BOOL HelpSystemEnsureRunning();
#ifdef HELPIFC_TCP
extern BOOL HelpSystemConnect(LPCTSTR = NULL);
#else
extern BOOL HelpSystemConnect();
#endif
extern void HelpSystemDisconnect();
extern BOOL HelpSystemIsConnected();
//
// In LogPlayback mode, HelpSystemIsConnected returns state of "virtual
// help system" connection simulated from log. That way it looks to callers as 
// if the help system is running even if we are not in fact connected to the Lisp
// process. Following returns state of real connection, if needed.
//
extern BOOL HelpSystemReallyConnected();

// 
// Execf: main client routine to do a synchronous (blocking) remote function call to the
// help system. Takes a printf-style argument list, formats into a command string, 
// sends it to help system and retrieves result. Internally logs call,
// and fires callback to mainframe's Begin/EndDdeWait wrappers to adjust UI.
// Uses current timeout and message, see below to customize.
// RETURNS: a pointer to a static buffer with the returned result string.
//          NULL if call failed.
// 
extern LPCTSTR HelpSystemExecf(LPCTSTR lpszFormat, ...);
// 
// To cancel current synchronous transaction in progress:
//
extern void HelpSystemCancelCall();

//
// Sendf: Post command as a notification, no result used (returns immediately).
// Much more efficient if no result is needed (uses async ipc internally).
//
extern BOOL HelpSystemSendf(LPCTSTR lpszFormat, ...);

//
// For asynchronous operations:
// Caller specifies a callback function of type PROCP to fire on completion.
// PROCP is called with cookie and returned result data string or NULL for failure.
//
typedef void (*PROCP) (DWORD dwCookie, LPCTSTR pszResult);		
//
// To execute command asynchronously, with callback on completion.
// 
extern BOOL HelpSystemExecAsync(LPCTSTR pszCmd, PROCP pfnNotify, DWORD dwNotifyArg);

//
// For setting custom parameters before an RPC call: timeout to wait before
// giving up and optional status message to display during call.
// These are automatically reset to defaults when the call completes
// 
extern void HelpIfcSetCallParms(int nTimeout, LPCTSTR pszMsg = NULL);

// 
// HELPIFC_TIMEOUT: default timeout for helpsys calls (milliseconds):
//
#if defined(HELPIFC_TCP) && defined(ATLAS)	
// chat interface with remote tutor => WOZ version

// use long timeout so human tutor can type possibly long response. 
// !! maybe bad to use for non-hint requests in case of Andes error.
#define HELPIFC_TIMEOUT		(5*60*1000)	 // 5 minutes

#else // ! WOZ			

// shorter timeout to local computer tutor
#define HELPIFC_TIMEOUT		(30*1000)	 // 30 sec should be plenty for Andes2	

#endif // ! WOZ

//
// Helper macro for formatting possibly-empty CString arguments passed
// to Lisp Help System API
//
inline LPCTSTR STR2ARG(const CString& str) 
	{ return str.IsEmpty() ? "NIL" : (LPCTSTR) str; }

// Convert string contents to Lisp-readable form.
// escapes any Lisp special characters
extern CString LISPSTR(const CString& str);

#if 0 // code for possible C++ interface class yet to be incorporated
class IHelpIfc			
{
public:
	// optional optimization, use to start DDE server running early for later connect
	virtual BOOL	EnsureRunning();	

	virtual BOOL	Connect();		// note: starts DDE server if not done earlier
	virtual BOOL	IsConnected();
	virtual BOOL	ReallyConnected();
	virtual void	Disconnect();

	virtual void	SetCallParms(int nTimeout, LPCTSTR pszMsg = NULL);
	
	// synchronous EXEC call, blocks until returns result or NULL for failure
	virtual LPCTSTR Execf(LPCTSTR pszFormat, ...);
	virtual void	CancelCall();	
	/* virtual BOOL	CallInProgress(); // not implemented yet */

	// NOTIFY: Post command, no result used (returns immediately).
	virtual BOOL    Sendf(LPCTSTR pszFormat, ...);

	// for async EXEC call, firing callback with result on completion 
	virtual void	ExecAsync(LPCTSTR pszCmd, PROCP pfnNotify, DWORD dwCookie);	
};
#endif 0 

// For use by log player: stuff RPC result in programmatically.
void HelpIfcSetResult(LPCSTR pszResult);

