///////////////////////////////////////////////////////////////////////
//
// History logging interface
// 
// API currently straight C, could be wrapped in C++ class.
//
///////////////////////////////////////////////////////////////////////
#ifndef HISTORY_INCLUDED
#define HISTORY_INCLUDED 1		// prevent multiple includes in dlg hdrs

class CDrawObj;					// forward reference
class CPlanItem;

extern CString HistoryFileBegin(LPCTSTR pszFileName = NULL);
extern CString HistoryFileSetStudent(LPCTSTR pszStudentName);
extern void HistoryFileEnd();
extern void HistoryUpload();
extern CString HistoryFileGetPath();

extern int HistTime();			// gets history time in seconds:
extern CTimeSpan HistTimeSpan();// gets history time as a time span:

//
// Use the following to format and write a log file entry of form
//
//     EVENT_NAME arg1 arg2 ... 
// Routine does nothing if history file not open.
// Adds timestamp and newline
//
extern void Logf(LPCTSTR lpszFormat, ...);

/* Obsolete since we added symbolic ids:
// Get a unique object id for use in log entries
extern long OBJ_ID(CDrawObj* pObj); */

// Return name for those objects that have labels, else empty string
extern LPCTSTR OBJ_NAME(CDrawObj* pObj); 

extern LPCTSTR TREE_ID(CString pos);

extern LPCTSTR TREE_NAME(CPlanItem* pPlanItem);

// For converting multi-line text strings to/from C-style escape sequences
// for use as args in log entries:
extern CString EscapeText(LPCTSTR pszSrc);
extern CString UnescapeText(LPCTSTR pszSrc);

// For formatting string values that may be empty or contain spaces
extern CString ValToArg(CString& strValue);
extern CString ArgToVal(LPCTSTR szLog);


//
// Helper for wrapping modal dialog boxes and logging result:
//
extern int LogModalDlg(CDialog& dlg);

extern CDialog* LogPlayerBeginModalDlg(CDialog* pOldDlg);
extern void LogPlayerEndModalDlg(CDialog* pOldDlg);

// can be used to find current dialog, if it's log-aware
extern CDialog* LogPlayerGetCurrentDlg();


//
// For controlling the playback task:
//
class IPlayerUI;				// forward reference

extern void LogPlayerBegin(LPCTSTR pszPathName, IPlayerUI* pUI = NULL);
extern BOOL LogPlayerInPlayback();
extern void LogPlayerToggleFF();
extern BOOL LogPlayerFF();
extern void LogPlayerPauseResume();
extern BOOL LogPlayerPaused();
extern void LogPlayerStop();
extern void LogPlayerToggleCallHelp();
extern BOOL LogPlayerCallHelp();
extern void LogPlayerSetUI(IPlayerUI* pUI);
extern void LogPlayerGoto(UINT m_nSecs);
extern BOOL LogPlayerInGoto();
extern void	LogPlayerStep();
extern void LogPlayerSnapshot();
extern int  g_nLogAndesVersion;

#if 0 // Following C++ class could replace global functions. 
class CLogPlayer	// Unique global object, so all methods static.
{
public:
	static void Begin(LPCTSTR pszPathName, IPlayerUI* pUI = NULL);
	static BOOL InPlayback();
	static void ToggleFF();
	static BOOL FF();
	static void PauseResume();
	static BOOL Paused();
	static void Stop();
	static void ToggleCallHelp();
	static BOOL CallHelp();
	static void SetUI(IPlayerUI* pUI);
private:
};
#endif 0

// implementation func exported for use by helpifc:
extern BOOL LogPlayerGetHelpCallResult(LPSTR pBuf);


// IPlayerUI: Interface class defining notifications sent from player to its UI.
//
// Methods are "outgoing" notification callbacks fired by playback process,
// a la COM connection points. Can mix-in this class with other MFC objects, e.g. 
// CDialog, to implement a notification "sink" to provide ui display services for 
// the log playback task. Caller that starts the player attaches interface 
// implementation to player via LogPlayerSetUI.
//
// This method used so we can easily set one UI for log playing, another for demos.
class IPlayerUI
{
public:
	// Methods are not pure virtual so concrete UI classes don't have to 
	// implement them all. Non-overridden ones just default to no-ops.

	// On opening of new logfile for playing:
	virtual void SetFileName(LPCTSTR pszFileName) {};
	// On change in log playback time:
	virtual void SetPlaybackTime(LPCTSTR pszTime) {};
	// On change in percentage of log file played
	virtual void SetProgress(int nPercent) {};
	// On change in "on-deck" event (next one to be played):
	virtual void ShowNextEvent(LPCTSTR pszMsg) {}; 
	// On state change due to play/pause/continue/stop operations.
	// Can process by triggering cmdUI update for player commands -- handlers in 
	// frame query current state of the player to enable/check command buttons. 
	virtual void UpdatePlayerUI() {};
	// On normal completion of playback:
	virtual void NotifyFinished() {};
};


//////////////////////////////////////////////////////////////////////////////////
//	ANDES Events
//
// EventID: enum constants for all events currently in use by logging.
// !!! Should be changed so that logging objects define their own events and
// symbols, by registering an event table with the log module, say.
// Some log messages are really trace output that need not be replayed.
// They might also be used for validity checking during replay.
//
enum EventID 
{
	// 
	// logged by CEQView:
	//
	EV_EQ_FOCUS,		// set focus to EQ 
	EV_EQ_CHANGE,		// change text in EQ
	EV_EQ_KILLFOCUS,	// lose focus on eq
	EV_EQ_SUBMIT,		// submit eq for checking (was logged as KILLFOCUS in older logs)
	EV_EQ_DELETE,		// lost focus on blank equation field = "delete"
	EV_EQ_WHATSWRONG,	// "whats wrong" help on an equation.
	EV_EQ_STATUS,		// set status of EQ. (trace)
	EV_EQ_CALCULATE,	// simplify current equation
	EV_EQ_RESULT,		// got asynch EQ status query result
	EV_EQ_SOLVEFOR,		// Solve for selected variable
	// 
	// logged by CEXView:
	//
	EV_VIEW_OBJ,		// make object currently visible one
	EV_VIEW_DIAGRAM,	// start peek at diagram
	EV_END_DIAGRAM,		// end peek at diagram
	EV_VIEW_SKETCH,		// start peek at sketch
	EV_END_SKETCH,		// end peek at sketch
	EV_SELECT_OBJ,		// make current selected object in diagram
	EV_DESELECT_OBJ,	// deselect object in diagram
	EV_EXPLAIN,			// issue explain command
	// for logging all object ids in example. Not done anymore since objects now carry ids.
	EV_OBJ_MASK,		// masked region 	
	EV_OBJ_TEXT,		// text object. !!! Also used in CFBDView post-adding.
	// 
	// logged by CFBDDoc on behalf of App:
	//
	EV_OPEN_PROBLEM,	// open given problem by id
	EV_OPEN_EXAMPLE,	// open given example by id
	EV_CHECK_ENTRIES,	// logs re-check of initial entries in solution
	EV_FBD_ENTRY,		// logs an initial FBD entry
	EV_VAR_ENTRY,		// logs an initial Variable entry
	EV_EQ_ENTRY,		// logs an initial equation entry
	EV_ANSWER_ENTRY,	// logs an initial answer entry
	EV_NEXT_ID,			// logs id generation counter value
	EV_CLOSE,			// close current open document.
	// next two now obsolete:
	EV_OPEN_ANDES_DOC,	// open a doc (prob, exmpl) from the Andes directory 
	EV_OPEN,			// open a document using absolute path.
	// 
	// logged by CFBDView
	//
	/* "Text", EV_OBJ_TEXT, */
	EV_LBUTTON_DOWN,	// low-level mouse events
	EV_MOUSE_MOVE,
	EV_MOUSE_UP,
	EV_SELECT_TOOL,		// select a drawing tool.
	EV_BEGIN_DRAW,		// begin drawing an object. Ends with MOUSE_UP
	EV_BEGIN_VECTOR,	// begin drawing a vector (special since has subtype arg).
	EV_OBJ_SYSTEM,		// added a system (trace)
	EV_OBJ_VECTOR,		// added a vector (trace)
	EV_OBJ_RADIUS,
	EV_OBJ_ANGLE,
	EV_OBJ_AXES,		// added axes (trace)
	EV_OBJ_OTHER,		// added other graphic object (trace)
	EV_EDIT_PROPS,		// edit properties command
	EV_PROPS_VECTOR,	// logs properties of motion vector (trace)
	EV_PROPS_FORCE,		// logs properties of force (trace)
	EV_PROPS_COMPO,		// logs properties of components (trace)
	EV_PROPS_AXES,		// logs properties of axes (trace)
	EV_PROPS_SYSTEM,	// logs properties of system (trace)
	EV_PROPS_RADIUS,
	EV_PROPS_ANGLE,		// logs properties of angle (trace)
	EV_PROPS_VARIABLE,	// logs properties of variable (trace)
	EV_CLICK_BG,		// clicked nowhere, clearing selection (trace)
	EV_BEGIN_RESIZE,	// begin resizing object. Ends with MOUSE_UP.
	EV_BEGIN_MOVE,		// begin moving an object. Ends with MOUSE_UP.
	EV_SELECT,			// selected an object (trace). !!!Also used in plan view
	EV_REMOVE_SEL,		// removed object from selection (trace)
	EV_DESELECT,		// deselected an object (trace).
	EV_DELETE,			// delete selection command. !!! also used in plan view
	EV_DELETED,			// traces individual deletions
	EV_DUPLICATE,		// duplicate selection command
	EV_OBJECT,			// added an object (trace). Generic, used after duplicate.
	EV_FBD_MENU,		// popped up menu. (trace). !!!also used in plan view
	EV_MARK_VECTOR,		// mark/unmark vector
	EV_EDIT_VEC_TYPE,	// change type of vector
	EV_ANSWER_FOCUS,	// set focus to answer box
	EV_ANSWER_KILLFOCUS,// set focus out of answer box
	EV_ANSWER_CHANGE,	// changed text in answer box
	EV_ANSWER_SUBMIT,	// submitted answer for checking
	EV_ANSWER_STATUS,	// changed answer status
	EV_CHOICE_CLICK,	// clicked a choice button
	EV_CHOICE_SELECT,	// selected answer to a mc question
	EV_CHOICE_CHECK,	// checked a non-exclusive choice box
	EV_FBD_WHATSWRONG,	// what's wrong help request
	EV_FBD_TCARD_MSG,	// notification from TCard help
	EV_SET_ZDIR,		// set zdir value on selection or for drawing
	//
	// CVarView
	//
	EV_SELECT_VARIABLE,
	EV_NEW_VARIABLE,
	EV_MODIFY_VARIABLE,
	EV_DELETE_VARIABLE,
	EV_VARIABLE_WHATSWRONG,
	EV_VARIABLE_MENU,

	//
	// CPrincView
	//
	EV_PRINCIPLE_NEW,
	EV_PRINCIPLE_SELECT,
	EV_PRINCIPLE_MODIFY,
	EV_PRINCIPLE_DELETE,
	EV_PRINCIPLE_WHATSWRONG,
	EV_PRINCIPLE_MENU,
	EV_PRINCIPLE_SETCHECK,
	// 
	// logged by HelpIfc on behalf of App:
	//
	EV_DDE_EXEC,		// called remote function in help system (trace)
	EV_DDE_DISCONNECT,	// got help system disconnect.
	EV_DDE_RESULT,		// got result from help system
	EV_DDE_POST,		// called remote procedure in help system (trace).
	EV_DDE_CANCEL,		// user cancelled DDE call
	EV_DDE_FAILED,		// got DDE failure return
	EV_START_HELP,		// started helpsys running
	// 
	// App-level events logged by CFBDApp:
	//
	EV_TASK_SELECT,		// selected task from task dialog.
	EV_CLOSE_APP,		// initiated close of app.
	EV_CANCEL_CLOSE,	// backed out of close of app.
	EV_LAUNCH_UPLOADER,	// launched upload program
	// logged by CMainframe:
	EV_APP_ACTIVATE,	// activated application frame
	EV_APP_DEACTIVATE,	// deactivated app frame (switched to other app).

	// App-level help commands:
	EV_HELP_HINT,		// I'm stuck, give me a hint = GetProcHelp
	EV_HELP_HOW,		// Help followup commands:
	EV_HELP_EXPLAIN,
	EV_HELP_WHY,
	EV_HELP_LINK,		// non-standard help-command hyperlink in hint

	// Hint pane commands
	EV_HINT_HIDE,
	EV_HINT_POPUP,

	// Interaction View commands
	EV_DISCOURSE_TEXT,
	EV_LEFT_UNSENT,
	EV_UNSENT_CONTENTS,
	// for menu items:
	EV_QUANT_DEF,		// Chose quant type to define from menu
	EV_MENU_CHOICE,		// Chose string from a menu 

	// Events in the PSM selection dialog
	EV_PSM_SELECT,		// selected new item
	EV_PSM_EXPAND,		// expanded section

	EV_KILL_POPUP, 
	
	EV_COMMENT,			// to record a comment.

	// Message Boxes and other UI services: 
	EV_DDE_CMD,			// received command from help system on our DDE server "port".
	EV_REMOTE_CMD,		// remote viewer received command from peer workbench
	
	EV_MSG,				// show plain message box
#if 0 // not exported now
	EV_MSG_INSTRUCT,	// show instruct message
	EV_MSG_WARNING,		// show warning message 
#endif 0
	EV_SHOW_LESSON,		// show a minilesson
	EV_SHOW_RULE_QUERY,	// show a rule query dialog
	EV_SHOW_TCARD,		// show a training card
	EV_SHOW_HINT,		// show a hint with followups
	EV_SHOW_DEMO,		// run a demo script
	EV_SHOW_TEXTBOOK,	// show the online textbook
	EV_OPEN_BROWSER,	// open browser modelessly on a web page or lesson
	EV_CLOSE_BROWSER,	// close it if it's still open
	EV_SET_SCORE,       // update the score

	// Demo pointer "agent":
	EV_POINT_TO,		// move the demo pointer
	EV_DEMO_MSG,		// popup message
	// Minilesson viewer dialog
	EV_BROWSER_OPEN,	// lesson browser opened
	EV_BROWSER_CLOSE,	// lesson browser closed
	EV_BROWSER_NAVIGATE, // user initiated navigation through a link
	EV_BROWSER_GO,		// traces program-caused browser navigation attempts
	EV_BROWSER_FORWARD,	// hit forward command
	EV_BROWSER_BACK,	// hit back command
	
	// 
	// LogDialog Boxes: generic
	//
	EV_DLG_MOVE,
	// dialog help events
	EV_DLG_WHATSWRONG,
	//	generic control events:
	EV_FOCUS,			// was also used for equations, replaced by EQ-F */
	EV_CTL_CHANGE,
	EV_RICHEDIT_CHANGE,
	EV_CBO_DROP,
	EV_CBO_SEL,
	EV_CBO_SOK,
	EV_CBO_CLOSE,
	EV_LIST_SEL,
	EV_BTN_CLICK,

	// Dialog-specific events: doubly obsolete, since dialogs have changed
	// and all button events logged/played now anyway. 
	EV_DLG_SYSTEM_TYPE,		// CSystemDlg -- chose system type
	EV_DLG_VECTOR_TYPE,		// CVectorDlg -- chose vector type

	// Dialog popup msgs just trace showing (really initting) of dialogs.
	EV_DLG_LOGIN,
	EV_DLG_TASK,
	EV_DLG_SYSTEM,
	EV_DLG_RADIUS,
	EV_VECTOR_DLG,
	EV_DLG_AXES,
	EV_VAR_DLG,
	EV_DLG_TEXT,
	EV_DLG_ANGLE,
	EV_DLG_RULER,
	EV_DLG_VECTYPE,
	EV_DLG_WARNING,
	EV_DLG_INSTRUCT,
	EV_DLG_PRINCIPLE,
	EV_PROPERTY_DLG,
	EV_FORMULA_DLG,

	// CPlanView
	EV_SELECT_ITEM,
	EV_EDIT_ITEM,
	EV_INSERT_NEXTITEM,
	EV_INSERT_SUBITEM,
	EV_DBL_CLCK,
	EV_PLAN_MENU,
	// 
	// special events used by log player:
	//
	EV_END_LOG,			// marks normal end of log file
	EV_LOG_LENGTH,		// gives time length of demo log file
	EV_IGNORE,			// special code for no-op event (can be used for trace msgs, eg.)
	EV_ASYNC_MODE,		// records whether async eq checking in effect.
	EV_FBD_VERSION,		// modification date of workbench
	EV_ANDES_VERSION,	// version of Andes released to students
	// for remote command interpreter:
	EV_SOCKET_CLOSE,	// put into input queue on remote cmd socket close
	// CTabView
	EV_SWITCH_TAB,	
	
	// CHiLevelVw
	EV_PLANBTN_CLICK,
	EV_PLANCELL_CLICK,
	EV_EDIT_SOUGHT,
	EV_PLANLIST_SEL,
	EV_LBUTTONDOWN_GRID,
	EV_MOUSEUP_GRID,
	EV_LBUTTONDBLCLK_GRID,
	EV_MOUSEMOVE_LIST,
	EV_SHOW_EXP,
	EV_HIDE_EXP,
	EV_EDIT_PROPERTY,
	EV_EDIT_DIRECTION,
	EV_EDIT_NEXTSTEP,
	EV_HILEVELVW_MENU,
	
};	

// LogEventf: newer logging function takes event id, arg format, args
// fetches name from global event table
extern void LogEventf(EventID id, LPCTSTR pszArgFmt, ...);

// special case routine allows setting timestamp, for when "backdating" is
// necessary because logging at end of long operation.
extern void LogEventfAt(int nTimeStamp, EventID id, LPCTSTR pszArgFmt, ...);

//
// Interface class for objects that can execute ANDES events
//
class IEventHandler
{
public:
	// replay event given id and textual argument string
	virtual BOOL DispatchEvent(EventID id, LPCTSTR pszArgs) { return FALSE; };
	// for demo mode support: move demo ptr to referenced object
	virtual void PointToObject(LPCTSTR pszObjID) { /* no-op */ };
};

// 
// For DDE or script execution:
// 
extern BOOL ExecuteCmd(LPCTSTR pszCmd);

// Default TCP port for remote event notification service.
const int EVENT_SRVR_PORT=54321;

// attach command interpreter to remote event server.
extern BOOL LogPlayerConnectRemote(LPCTSTR pszRemoteHost);
// fired when complete msg available from the remote process.
extern void LogPlayerProcessInput();

//
// MCI command wrapper (implemented in history for audio dev, also used by App)
//
extern BOOL DoMciSendString(LPCTSTR pszCmd, BOOL bSilent = FALSE);

#endif ! HISTORY_INCLUDED