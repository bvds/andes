// ChatView.cpp : implementation file
//

#include "stdafx.h"
#include "expmenu.h"

#include "FBD.h"
#include "Helpifc.h"
#include "fbddoc.h"
#include "varview.h"
#include "ChatView.h"
#include "PopupWnd.h"
#include "Mymenu.h"
#include "drawobjdlg.h"
#include "PsmDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// 
static char* STUDENT_PROMPT = "S: ";
static char* TUTOR_PROMPT = "T: ";
static char* DISCOURSE_SPACING = "\r\n";

// Tags for standard followup command links. We add these in response to
// button codes in HintSpec's. Hyperlink implementation knows to call our 
// DoHelpRequest method passing the help command string as argument.
static const char szExplainLink[] = "{\\h Explain further}{\\v Explain-More}";
static const char szHowLink[] = "{\\h How do I do that?}{\\v Hint-Next-Substep}";
static const char szWhyLink[] = "{\\h Why?}{\\v Why}";
static const char szOKLink[] = "{\\h OK}{\\v Hide}";
// Link used in SKatz followup dialog experiment only. Displays as "OK" but
// sends command to help sys to  go on to next followup question, if any.
static const char szNextLink[] = "{\\h OK}{\\v Next}";

static const char* szLinkIndent = "        "; // indentation of reply link line
static const char* szLinkSp     = "     ";	  // space between reply hyperlinks

// Canned message to show in case of help system failure:
static const char* szFailureMsg = 
"Sorry, an internal error occurred. Help could not be obtained for that request.";

/////////////////////////////////////////////////////////////////////////////
// CChatView

IMPLEMENT_DYNCREATE(CChatView, CRichEditView)

/********************************************
 * Method: CChatView
 * Class:  CChatView
 * Purpose: Constructor
 * Parameters: void
 * Return: Special
 * Side Effects: Initializes member variables.
 *********************************************/
CChatView::CChatView()
{
	m_lInputStartIndex = 0;
	m_lCurMsgStartIndex = 0;
	m_strPrompt = STUDENT_PROMPT;
	m_strSysPrompt = TUTOR_PROMPT;
	m_bInHintSequence = NULL;
	m_strBtns = "";
	// default is disabled until enter tutorial dialog mode
	m_bEnabled = FALSE;
	m_bInputAllowed = FALSE;
	m_bOnSysReplyLine = FALSE;
}


/********************************************
 * Method: ~CChatView
 * Class:  CChatView
 * Purpose: Destructor
 * Parameters: void
 * Return: Special
 * Side Effects: frees member variables.
 *********************************************/
CChatView::~CChatView()
{
	m_strPrompt.Empty();

	// delete any remaining hyperlink objects
	while (!m_links.IsEmpty())
		m_links.RemoveHead()->Delete();
}

BEGIN_MESSAGE_MAP(CChatView, CRichEditView)
	//{{AFX_MSG_MAP(CChatView)
	ON_WM_CHAR()
	ON_WM_MOUSEMOVE()
	ON_WM_SETCURSOR()
	ON_WM_LBUTTONDOWN()
	ON_UPDATE_COMMAND_UI(ID_EDIT_COPY, OnUpdateEditCopy)
	ON_COMMAND(ID_EDIT_COPY, OnEditCopy)
	//}}AFX_MSG_MAP
	ON_NOTIFY_REFLECT(EN_PROTECTED, OnProtected)
	ON_CONTROL_REFLECT_EX(EN_KILLFOCUS, OnKillFocus)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChatView diagnostics

#ifdef _DEBUG
void CChatView::AssertValid() const
{
	CRichEditView::AssertValid();
}

void CChatView::Dump(CDumpContext& dc) const
{
	CRichEditView::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CChatView Operations

/*********************************************************************
 * Method: AddSystemText
 * Class: CChatView
 * Purpose: To handle the text from the system.
 * Parameters: LPCTSTR: The initial text that starts the dialog.  
 *                      defaults to "This is Atlas, how may I help you?"
 *             HintType: message type code.
 *
 * Type was used to distinguish two classes: (1) Unsolicited "popup" tutor messages
 * (type Msg, the default) - these are generally immediate error messages piggybacked on 
 * responses to entries though they are occasionally sent asynchronously; and
 * (2) solicited Hint/Whatswrong help request responses, which are expected to be 
 * part of tutorial dialog sequences.
 * 
 * Hint strings include characters after a tilde separator that code the possible
 * followups or responses allowed.
 *
 * Any message containing the "?" flag or a menu query descriptor always requires a 
 * student text response and so starts or continues tutorial dialog mode. It is also possible 
 * for an unsolicited message to include the '?' flag, to start a dialog at the tutor's 
 * initiative, although Andes may never use this possibility.
 * 
 * For terminal messages (one without any followups) we distinguish the following two:
 *  1. a terminal hint/whatswrong response: we simply ends the dialog mode 
 *     at the tutor's initiative with no need for an "OK" click (tutor said "over and out").
 *  2. a terminal unsolicited msg: we always append an "OK" button to any specified followups 
 *     (perhaps none) so student can dismiss the message after reading it. 
 * !!! Possibly there is not much point in keeping this distinction and we could
 * simplify by requiring OK to dismiss in all cases.
 * !!! With Andes2 helpsys, we may no longer see the first case at all since even solicited
 * responses may now be routed through ShowHint command for uniformity, so will appear as
 * unsolicited messages and get the OK button.
 *
 * Returns: void
 * Side Effects: Prints the text and a prompt to the view.
 *********************************************************************/
void CChatView::AddSystemText(LPCTSTR pszHint, HintType type/*=Msg*/)
{
	// If we are already on the system line awaiting response to student 
	// submission, reset flag; else add blank line separator for new tutor msg.
	if (m_bOnSysReplyLine)
		m_bOnSysReplyLine = FALSE;
	else if (m_lInputStartIndex != 0)	// don't do on first line
		AddText(DISCOURSE_SPACING);

	m_strBtns.Empty();
	BOOL bQuery = FALSE;		// Free text student response required (continue dialog).
	BOOL bMenu = FALSE;			// Menu selection student response required (continue dialog)
	m_lInputStartIndex = GetTextLength();
	// remember start of current message : only links after this should be hot.
	m_lCurMsgStartIndex = m_lInputStartIndex;
	CString strAddLinks;		// will collect added links, if any.

	// Ensure we are in tutor mode whenever we get a message:
	if (pszHint != NULL) {
		theApp.SetTutorMode(TRUE);
	}

	// Remove all earlier hyperlinks on receipt of new message
	DeleteLinks();

	// If help called failed, show canned default message
	if (pszHint == NULL)
		pszHint = szFailureMsg;
			
	/////////////////////////////////////////////////////////////////////////
	// Following all processes hint spec, adding followup buttons as needed
	/////////////////////////////////////////////////////////////////////////
	CString textSpec = pszHint;
	// Parse button tag part out of message text and remove.
	// Now have to parse even if using remote human tutor, since some msgs will
	// come from real ANDES help system. For now,  disallow special separator ~
	// and other formatting tags in body: "$" for Greek and "{" for tags.
	// Should define some way to escape them in case human tutor types them.

	// look for followup codes after trailing tilde separator
	// search from right end in case message text also has a ~ in it.
	int sepIndex = textSpec.ReverseFind('~');	
	if (sepIndex >= 0) {
		if (sepIndex + 1 < textSpec.GetLength()) {// Button list non-empty
			m_strBtns = textSpec.Mid(sepIndex + 1); // get buttons 
			m_strBtns.Remove('~');	// sometimes bad extra tilde from helpsys
		}
		textSpec = textSpec.Mid(0, sepIndex); // drop the button part from the text
	
		// check for "Query" flag ('?') as first button and remove it if present
		bQuery = m_strBtns[0] == '?';
		if (bQuery)
			m_strBtns.Remove('?');
		// check for menu spec beginning with vbar or else single-character menu code
		bMenu = (m_strBtns[0] == '|') ||
				(m_strBtns.FindOneOf("QPE") != -1);
	}

	// Unescape any tildes in message text.
	textSpec.Replace("#_TILDE_#", "~"); // assume unlikely to occur in actual text

	// Form appropriate followup links and add on line after message
	// NB: don't scan button list if we got a menu spec, it may be long and complex.
	if (! bMenu) {
		if (m_strBtns.Find('e') != -1) 
			strAddLinks += szExplainLink;
		if (m_strBtns.Find('h') != -1) 
			strAddLinks += (strAddLinks.IsEmpty() ? "" : szLinkSp) + CString(szHowLink);
		if (m_strBtns.Find('w') != -1) 
			strAddLinks += (strAddLinks.IsEmpty() ? "" : szLinkSp) + CString(szWhyLink);
		if (m_strBtns.Find('n') != -1)
			strAddLinks += (strAddLinks.IsEmpty() ? "" : szLinkSp) + CString(szNextLink);
	} else if (m_strBtns[0] == '|') {
		// convert menu spect to hyperlinks, assume its short
			CStringList strItems;
			SplitStr(m_strBtns, strItems, "|");
			POSITION pos = strItems.GetHeadPosition();
			while (pos) {
				CString strItem = strItems.GetNext(pos);
				CString strMenuLink;
				strMenuLink.Format("{\\h %s}{\\v handle-student-response \"%s\"}",
									strItem, strItem);
				strAddLinks += (strAddLinks.IsEmpty() ? "" : szLinkSp) + strMenuLink;
			}
	}

	// if some optional followups specified, add "OK" option to dismiss mode. Also do 
	// for unsolicited messages unless they query the student (most normally do not) so 
	// student has a way to dismiss the msg. (Maps to "Hide" command which ends mode.)
	if ( (!strAddLinks.IsEmpty() && !bMenu)    // have some followups links and not from menu
		|| (type == Msg && ! (bQuery || bMenu))) // unsolicited msg w/no followups
		// For SKatz version: no OK if Next button specified, it's mandatory.
		if (m_strBtns.Find('n') == -1) // no Next button
			strAddLinks += (strAddLinks.IsEmpty() ? "" : szLinkSp) + CString(szOKLink);
	// append space so trailing empty area not hot because nearest char is.
	if (!strAddLinks.IsEmpty())
		textSpec += CString("\r\n") + szLinkIndent + strAddLinks + " "; 
	/////////////////////////////////////////////////////////////////////////////////
	// End processing hint spec
	/////////////////////////////////////////////////////////////////////////////////

	// OK, add the message (including any links) if non-empty. 
	// Note this routine adds in current font, and leaves default font.
	if (!textSpec.IsEmpty())
		AddText(m_strSysPrompt + textSpec);
/* Taken out since italic font is hard to read. -AW 7/16/2002
	// Change font of just-added system text to system's font = italic. 
	GetRichEditCtrl().SetSel(m_lInputStartIndex, GetTextLength());
	ToggleCharItalic();
*/
	// Add space to advance to next line -- this restores default font (non-italic).
	// Note we don't prompt student unless a response is required (below)
	AddText(DISCOURSE_SPACING);

	// Have to keep this index pointing at start of student input line, even if no 
	// student input, since logging functions use it to detect unsent partial lines 
	m_lInputStartIndex = GetTextLength();


	// WOZ: reset log state. Since whatever event caused this will be logged 
	// and replayed, student line is now reset to "empty" in log
	m_strLoggedLineState.Empty();
	
	// Update hint pane state
	m_bInHintSequence = !m_strBtns.IsEmpty();

	// Give student appropriate reply options:

	// if free text response required, show prompt for reply
	// Not normally combined with followup buttons, though we can handle that.
	if (bQuery)   
	{
		AddText(m_strPrompt);
		m_lInputStartIndex = GetTextLength();
		m_bInputAllowed = TRUE;
		// Set keyboard focus here for student's reply.
		SetFocus();
	}
	// else see if we have a menu spec to process
	else if (!m_strBtns.IsEmpty() && (m_strBtns[0] == '|')) {
			// GetMenuSelection(m_strBtns);
	}
	// else see if we should collect response from a predefined menu
	else if (m_strBtns.Find('Q') != -1) {	// quantity choice
		GetQuantityType();
	}
	else if (m_strBtns.Find('P') != -1) {	// principle choice
		GetPrinciple();
	} else if (m_strBtns.Find('E') != -1) {	// equation choice
		GetEquation();
	}
	// else if no followups specified (including OK to dismiss Msg), end tutor mode
	else if (strAddLinks.IsEmpty())
		theApp.SetTutorMode(FALSE);
	
}


/***********************************************************************
 * Method: AddText
 * Class: CChatView
 * Purpose: To add protected text to the view.
 * Parameters: CString: the string to add to the view.
 * Return: void
 * Side Effects: Adds the new text to the end of the buffer and then 
 *               sets it to protected.  Unselects everything and then 
 *               sets the default character format to allow the addition
 *               of new text.  
 ***********************************************************************/
void CChatView::AddText(LPCTSTR str)
{
	CRichEditCtrl& edit = GetRichEditCtrl();
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	
	edit.SetEventMask(ENM_PROTECTED);
	int buf_len = (int)GetTextLength(); 
	edit.SetSel(buf_len, buf_len); // Select last character, which should be \0
	edit.GetSelectionCharFormat(cf);
	cf.dwMask = 0;
	cf.dwEffects = 0;

	/// Parse formatting
	CString text = str;


// #ifndef HELPIFC_TCP // do not parse text if remote tutor is typing text.
// Now have to parse tags even if using remote human tutor, since some responses will
// come from real ANDES help system. For now, just forbid human tutor from typing
// our special markers '$' or '{'. Will change to a more reliable method later,
	text.Replace("\\n", "\r\n"); // take out bad newlines

	// Parse links and greek letters
	CString preText, letter, tempStr;
	while (text.FindOneOf("{$") != -1)
	{
		int grkPos = text.Find('$');
		int lnkPos = text.Find("{\\");
		if ((grkPos >= 0) && ((lnkPos < 0) || (grkPos < lnkPos)))
		{ // do greek letter
			preText = text.Left(grkPos);
			if (!preText.IsEmpty())
				edit.ReplaceSel(preText);
			letter = text[grkPos + 1];
			if (((letter >= "a") && ( letter <="z"))||((letter >= "A") && ( letter <="Z")))
			{
				//ToggleCharSymbol();
				//edit.ReplaceSel(letter);
				InsertGreekText(letter);
				SetCharPlain();
			}
			else
			{
				tempStr = "$" + letter;
				edit.ReplaceSel(tempStr);
			}
			text = text.Mid(grkPos+2);
		}
		else if (lnkPos >= 0)
		{
			preText = text.Left(lnkPos);
			if (!preText.IsEmpty())
				edit.ReplaceSel(preText);
			tempStr = text.Mid(lnkPos);
			text = ParseOutFormatText(tempStr);
		}
	}
	tempStr = text;
	text = tempStr;
// #endif ! HELPIFC_TCP
	if (!text.IsEmpty())
		edit.ReplaceSel(text);

	// Protect the added text from changes.
	int newBufLen = edit.GetTextLength();
	edit.SetSel(buf_len, newBufLen);
	OnCharEffect(CFM_PROTECTED, CFE_PROTECTED); // Set text to Protected
	
	edit.SetSel(-1,-1);
	edit.SetDefaultCharFormat(cf); // Enable the addition of new text.
	// need to use SetDefaultCharFormat so that the EM_SETCHARFORMAT Message
	// is signaled so that the change will be allowed by OnProtected.
}


/***********************************************************************
 * Method: Clear
 * Class: CChatView
 * Purpose: Clears the interaction's text
 * Parameters: void
 * Return: void
 * Side Effects: Selects all of the text and clears it.
 ***********************************************************************/
void CChatView::Clear ()
{
	CRichEditCtrl& editCtrl = GetRichEditCtrl();
	editCtrl.SetSel(0,-1);
	editCtrl.Clear();
	editCtrl.EmptyUndoBuffer();
	// AddSystemText();
}

// 
// DismissMsgMode -- does work of ending message mode.
//
// Used by handler when student clicks OK on message
// 
void CChatView::DismissMsgMode()
{
	// if we are in middle of hint sequence it ends at student request.
	m_bInHintSequence = FALSE;

	// and get the app out of tutor mode.
	theApp.SetTutorMode(FALSE);

	// Links no longer active
	DeleteLinks();
}

/**********************************************************************
 * Method: GetLastUserString
 * Class: CChatView
 * Purpose: To return the text the user has typed in after the last prompt
 * Parameters: void
 * Return: CString: a string containing the user's text.
 * Side Effects: Selects the user's text and then grabs it, then it 
 *               unselects it.  It takes the string and removes the 
 *               prompt and any trailing whitespace and it changes 
 *               double-quote characters to back-quotes (lisp has 
 *               problems with double-quotes inside the string, lisp 
 *               will think it deliminates the string).
 **********************************************************************/
CString CChatView::GetLastInputStr ()
{
	CString csLine = GetLastInputLine();
	
	csLine.Replace("\"", "`");
	csLine.TrimRight();

	return csLine;
}

// Like GetLastInputStr, but gets "raw" contents w/no filtering
CString CChatView::GetLastInputLine()
{
	CString csLine;
	
	// get the text from the start of the user input to the end of the buffer.
	GetRichEditCtrl().SetSel(m_lInputStartIndex, -1); 								
	csLine = 	GetRichEditCtrl().GetSelText();
	GetRichEditCtrl().SetSel(-1,-1);
	
	return csLine;
}

/////////////////////////////////////////////////////////////////////////////
// CChatView message handlers

/***********************************************************************
 * Method: OnChar
 * Class: CChatView
 * Purpose: To process typed characters and to catch enters so it can 
 *          send the user's input to the help system.
 * Parameters: UINT: the character typed
 *             UINT: the repeat count
 *             UINT: special flags.
 * Return: void
 * Side Effects: On any key except enter, send it to CRichEditView::OnChar.
 *               On the enter key, if the help system is active, send the
 *                 user's response to the help system, get the response
 *                 and send it to the view then supply a new prompt.
 *                 If the help system fails, output a diagnostic response.
 ***********************************************************************/
void CChatView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	// Ignore all typed characters while disabled. 
	// Also if no free text response to this message allowed.
	if (! m_bEnabled || ! m_bInputAllowed) 
		return;

	// on a Enter stroke, process the user's text
	if(nChar == VK_RETURN) 
	{
		CString strDiscourse = GetLastInputStr();
		LogEventf(EV_DISCOURSE_TEXT, "%s", strDiscourse);
		// Note log now up to date with any partial line contents 
		// so don't log them again on killfocus while awaiting reply.
		m_strLoggedLineState = strDiscourse;

		// No more student input till next turn
		m_bInputAllowed = FALSE;

		// Advance cursor to next line immediately for visible feedback of 
		// submission, setting flag to show no need to skip line on response
		AddText(DISCOURSE_SPACING);
		m_bOnSysReplyLine = TRUE;

		// NB: NULL pszResult distinguishes failed call from success w/empty msg.
		// Important since failed calls must end TutorMode (in AddSystemText).
		LPCTSTR pszResult; 
		if (HelpSystemIsConnected()) // test for the connection
			pszResult = HelpSystemExecf("(get-dialog-response \"%s\")", strDiscourse);
		else 
			pszResult = "Help system is unavailable."; // Do something on a HelpSystem problem.

		// Send the response to the screen
		theApp.GetMainFrame()->ShowHint(pszResult, Hint); 
	}
	else
	{
		CRichEditView::OnChar(nChar, nRepCnt, nFlags);
	}
}


/**********************************************************************
 * Method: OnProtected
 * Class: CChatView
 * Purpose: To handle change requests on protected text inside the view.
 * Parameters: NMHDR*: the pointer to the ENPROTECTED struct.
 *             LRESULT*: pointer to the return value.
 * Return: void
 * Side Effects: Allows WM_CLEAR and EM_SETCHARFORMAT opperations on 
 *               the protected text and does not let any other edit.
 **********************************************************************/
void CChatView::OnProtected(NMHDR* pNMHDR, LRESULT* pResult)
{
	ENPROTECTED* pEP =(ENPROTECTED*)pNMHDR;

	switch (pEP->msg) {
	case WM_CLEAR:
	case EM_SETCHARFORMAT:
		pResult = FALSE;  // Do not protect text from these changes
		break;
	default:
		*pResult = TRUE;  // Protect the text from these changes
		break;
	}
}



/***********************************************************************
 * Method: OnInitialUpdate
 * Class: CChatView
 * Purpose: Set up the interaction
 * Parameters: void
 * Return: void
 * Side Effects: Calls AddSystemText with its default argument.
 ***********************************************************************/
void CChatView::OnInitialUpdate() 
{
	CRichEditView::OnInitialUpdate();

	// Set initial and right paragraph indentation in TWIPS 
	// 1 in = 1440 TWIPS 
	SetParaIndents(0, 360); // 1/4 in. right margin

	// Suppress initial prompt until enter TutorMode via help request.
	// AddSystemText();

	// !!! Should try to make initial text in tutor's font.

	// Set initial enabled state, color
	m_bEnabled = theApp.m_bTutorMode;
	EnablePane(m_bEnabled);
}

/************************************************************************
 * Method: DispatchEvent
 * Class: CChatView (IEventHandler)
 * Purpose: To handle the playback of logged events.
 * Parameters: EventID: The enumerated id of the event, this method handles
 *                      EV_DISCOURSE_TEXT
 *             LPCTSTR: The parameter list for the event.  For 
 *                      EV_DISCOURSE_TEXT the parameters are assumed to 
 *                            be the text entered by the user.
 * Return: BOOL: returns if the event was handled by the method.  Only
 *               returns TRUE.
 * Side Effects: On an EV_DISCOURSE_TEXT event, this method will add the
 *               text from the second parameter to the Interaction View
 *               and then call the help system to get the reply.  When
 *               playing a log file, this help system call will read the
 *               reply from the log file.
 ************************************************************************/
BOOL CChatView::DispatchEvent(EventID nEvent, LPCTSTR pszArgs)
{
	int nResult, nArgs;
	char szArg1[80]; 
	char szArg2[80];

	switch (nEvent) {
	case EV_DISCOURSE_TEXT:
		// For WOZ version: clear any intermediate partial lines that 
		// may have been replayed earlier
		SetInputLine("");

		AddText(pszArgs);
		AddSystemText(HelpSystemExecf("(get-dialog-response \"%s\")", pszArgs), Hint);
		break;

	case EV_UNSENT_CONTENTS:
	case EV_LEFT_UNSENT:
		SetInputLine(pszArgs);
		break;

	// to recreate popup menu selection events
	case EV_QUANT_DEF:	// Chose quantity type to define from menu
		// "%d %s %s", nResult, strVecProp, strTypeId
		// quantity type codes now not nec. persistent, so need to
		// to scan strTypeId and recalc nType to replay GetQuantityDef.
		// NB: in case of scalar, second prop arg is empty string
		nArgs = sscanf(pszArgs, "%d %s %s", &nResult, szArg1, szArg2);
		if (nArgs == 2) {
			// get current typecode from scalar quantity id string
			GetQuantityDef(CVarView::LookupId(szArg1));
		} else if (nArgs == 3) {
			// must be vector: get current structured type code
			CVarMenu::MakeVectorPropCode(szArg1, szArg2);
			GetQuantityDef(nResult);
		} 
		break;

	case EV_MENU_CHOICE: // Chose string from a menu 
		SubmitMenuSelection(pszArgs);	// empty string if cancelled!
		break;

	default:
		TRACE("ChatView dispatch: unknown event %d, ignored\n", nEvent);
		break;
	}

	return TRUE;
}

// SetInputLine -- helper for log playback updates the current student line
void CChatView::SetInputLine(LPCTSTR pszText)
{
	GetRichEditCtrl().SetSel(m_lInputStartIndex, -1); 								
	GetRichEditCtrl().ReplaceSel(pszText);
	GetRichEditCtrl().SetSel(-1,-1);
}

/***************************** Formatting Methods **********************************/

/**********************************************************************************
 * Method: SetParaIndents
 * Class: CChatView
 * Purpose: To change the left and right indentation in the RichEditCtrl.
 * Parameters: int: the amount of left indentation in 1/1440 of an inch
 *             int: the amount of right indentation in 1/1440 of an inch.
 * Returning: void
 * Side Effects: Changes the PARAFORMAT in the RichEditCtrl
 **********************************************************************************/
void CChatView::SetParaIndents(int lIndent, int rIndent)
{//indents are in twips (1/1440 of an inch)
	PARAFORMAT pf;
	pf.cbSize = sizeof(PARAFORMAT);
	CRichEditCtrl& edit = GetRichEditCtrl();
	edit.GetParaFormat(pf);
	
	pf.dwMask = pf.dwMask | PFM_STARTINDENT | PFM_RIGHTINDENT;
	pf.dxStartIndent = lIndent;
	pf.dxRightIndent = rIndent;
	edit.SetParaFormat(pf);
}


/**********************************************************************************
 * Method: ClearFormat
 * Class: CChatView
 * Purpose: To change the character format of a selected peice of text to have no
 *          visible effects.
 * Parameters: void
 * Returning: void
 * Side Effects: Changes the CHARFORMAT for the selected peice of text in the 
 *               RichEditCtrl so that Underline, Color, and Face formats are masked.
 **********************************************************************************/
void CChatView::ClearFormat()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetRichEditCtrl().GetSelectionCharFormat(cf);

	CHARFORMAT cfDefault;
	cfDefault.cbSize = sizeof(CHARFORMAT);
	GetRichEditCtrl().GetDefaultCharFormat(cfDefault);
	::lstrcpy(cf.szFaceName, cfDefault.szFaceName); 

	cf.dwMask = CFM_COLOR | CFM_UNDERLINE | CFM_FACE;
	cf.dwEffects = CFE_AUTOCOLOR;
	GetRichEditCtrl().SetSelectionCharFormat(cf);
}


/**********************************************************************************
 * Method: SetCharPlain
 * Class: CChatView
 * Purpose: To change the character format of a selected peice of text to have no
 *          visible effects.
 * Parameters: void
 * Returning: void
 * Side Effects: Changes the CHARFORMAT for the selected peice of text in the 
 *               RichEditCtrl so that Italic, Bold, and Face formats are masked.
 **********************************************************************************/
void CChatView::SetCharPlain()
{
	// undo char format effects
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	cf.dwEffects = 0;
	// restore default face (from default charformat, which we never change.)
	CHARFORMAT cfDefault;
	cfDefault.cbSize = sizeof(CHARFORMAT);
	GetRichEditCtrl().GetDefaultCharFormat(cfDefault);
	::lstrcpy(cf.szFaceName, cfDefault.szFaceName); 
	
	cf.dwMask = CFM_ITALIC | CFM_BOLD | CFM_FACE;
	GetRichEditCtrl().SetSelectionCharFormat(cf);
}


/**********************************************************************************
 * Method: SetCharSymbol
 * Class: CChatView
 * Purpose: To change the currently selected text to symbol face so Greek letters 
 *          are shown.
 * Parameters: void
 * Returning: void
 * Side Effects: Changes the CHARFORMAT for the selected text in the RichEditCtrl
 *               so that it renders the text in symbol face.
 **********************************************************************************/
void CChatView::SetCharSymbol()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);		
	cf.dwMask =  CFM_FACE;					// Change Face to symbol
	::lstrcpy(cf.szFaceName, "Symbol");
	GetRichEditCtrl().SetSelectionCharFormat(cf);				// Set the selection format
}


/**********************************************************************************
 * Method: SetCharLinkColor
 * Class: CChatView
 * Purpose: To change the color of the selected text to a different color to indicate
 *          what kind of link it is.
 * Parameters: char: the type of link it is: d for definition and default is hyperlink
 * Returning: void
 * Side Effects: Modifies the CHARFORMAT for the selected text in the RichEditCtrl
 *               so that it will be rendered in a different color.
 **********************************************************************************/
void CChatView::SetCharLinkColor(char tag)
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetRichEditCtrl().GetSelectionCharFormat(cf);
	
	if ( ((cf.dwMask & CFM_COLOR)&&(cf.dwEffects & CFE_AUTOCOLOR)) ||
		!(cf.dwMask & CFM_COLOR) ){
		cf.dwEffects = 0;
		cf.dwMask = cf.dwMask | CFM_COLOR;
		if (tag == 'd')	// "definition" i.e. popup
			cf.crTextColor = RGB(0, 0, 128); // navy blue
		// else if (tag == 'h') // helpsys call
		//	cf.crTextColor = RGB(0, 128, 128); // "teal"
		else // default: all others as hyperlink jumps
			cf.crTextColor = RGB(128, 0, 128); // purple
	}
	else	// turn off color (autocolor ignores crTextColor and uses system color)
		cf.dwEffects = cf.dwEffects | CFE_AUTOCOLOR;
	cf.dwMask = CFM_COLOR;
	GetRichEditCtrl().SetSelectionCharFormat(cf);
}


/**********************************************************************************
 * Method: SetCharUnderline
 * Class: CChatView
 * Purpose: To underline selected text.
 * Parameters: void
 * Returning: void
 * Side Effects: Modifies the CHARFORMAT for the selected text in the RichEditCtrl
 *               so that it is displayed underlined.
 **********************************************************************************/
void CChatView::SetCharUnderline()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetRichEditCtrl().GetSelectionCharFormat(cf);
	
	// if sel not consistently underlined, underline it all
	if (!(cf.dwMask & CFM_UNDERLINE) || !(cf.dwEffects & CFE_UNDERLINE))
		cf.dwEffects = CFE_UNDERLINE;
	else	// turn off underline
		cf.dwEffects = 0;

	cf.dwMask = CFM_UNDERLINE;
	GetRichEditCtrl().SetSelectionCharFormat(cf);
}


/**********************************************************************************
 * Method: ToggleCharBold
 * Class: CChatView
 * Purpose: To take a selection of text and turn it all bold if it is not bold or
 *          only partially bold, otherwise it removes the bold effect.
 * Parameters: void
 * Returning: void
 * Side Effects: Modifies the CHARFORMAT for the selected text in the RichEditCtrl
 *               and changes its dwMask and dwEffects to make the selection bold or not.
 **********************************************************************************/
void CChatView::ToggleCharBold()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetRichEditCtrl().GetSelectionCharFormat(cf);
	
	// if sel not consistently bold, set it all to bold
	if (!(cf.dwMask & CFM_BOLD) || !(cf.dwEffects & CFE_BOLD))
		cf.dwEffects = CFE_BOLD;
	else	// turn off bold
		cf.dwEffects = 0;

	cf.dwMask = CFM_BOLD;
	GetRichEditCtrl().SetSelectionCharFormat(cf);
}


/**********************************************************************************
 * Method: ToggleCharItalic
 * Class: CChatView
 * Purpose: To take a selection of text and turn it all italic if it is not italic
 *          or only partially italic, otherwise it removes the italic effect.
 * Parameters: void
 * Returning: void
 * Side Effects: Modifies the CHARFORMAT for the selected text in the RichEditCtrl
 *               and changes its dwMask and dwEffects to make the selection italic
 *               or not.
 **********************************************************************************/
void CChatView::ToggleCharItalic()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	CRichEditCtrl& edit = GetRichEditCtrl();
	edit.GetSelectionCharFormat(cf);
	
	// if sel not consistently italic, set it all to italic
	if (!(cf.dwMask & CFM_ITALIC) || !(cf.dwEffects & CFE_ITALIC))
		cf.dwEffects = CFE_ITALIC;
	else // turn off italic
		cf.dwEffects = 0;

	cf.dwMask = CFM_ITALIC;
	edit.SetSelectionCharFormat(cf);
}


/**********************************************************************************
 * Method: ToggleCharSymbol
 * Class: CChatView
 * Purpose: To take a selection of text and turn it all symbol face if it is not 
 *          already or if it is only partially so, otherwise it restores the
 *          default format.
 * Parameters: void
 * Returning: void
 * Side Effects: Modifies the CHARFORMAT for the selected text in the RichEditCtrl
 *               to make the face symbol or to restore the default settings.
 **********************************************************************************/
void CChatView::ToggleCharSymbol()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetRichEditCtrl().GetSelectionCharFormat(cf);

	// if sel face not consistently "Symbol" change it to symbol
	if (!(cf.dwMask & CFM_FACE) || !(lstrcmp(cf.szFaceName, "Symbol") == 0)) {
		::lstrcpy(cf.szFaceName, "Symbol");

	} else {
		// restore default face (from default charformat, which we never change.)
		CHARFORMAT cfDefault;
		cfDefault.cbSize = sizeof(CHARFORMAT);
		GetRichEditCtrl().GetDefaultCharFormat(cfDefault);
		::lstrcpy(cf.szFaceName, cfDefault.szFaceName); 
	}

	cf.dwMask = CFM_FACE;
	GetRichEditCtrl().SetSelectionCharFormat(cf);
}


/*************************************************************************************
 * Method: SetFormat
 * Class: CChatView
 * Purpose: To change the format of the currently selected text depending on its tag.
 * Parameters: char: the tag corresponding to the format the current selection should
 *                   displayed in.  Supports b for bold, i for italic, and links.
 * Returning:
 * Side Effects: Calls corresponding ToggleChar... method or SetChar... method.
 *************************************************************************************/
void CChatView::SetFormat(char tag)
{
	// check for format tags
	if (tag == 'b')//we want bold
	{
		ToggleCharBold();
	}
	else if (tag == 'i')//we want italics
	{
		ToggleCharItalic();
	}
	else{//we have a hyperlink
		SetCharUnderline();
		SetCharLinkColor(tag);
		//link tag: d for definition, l for link (minilesson), h for helpcall
	}
}

void CChatView::InsertGreekText(CString strText)
{
	CRichEditCtrl& edit = GetRichEditCtrl();
	// Simple method consisting of SetCharSymbol to set format at insertion point followed
	// by ReplaceSel does not display Greek symbols in the richedit control on Windows 
	// 2000 and Me, even though the Greek format attributes are correctly retreived on 
	// those character positions in the control. Following inserts the text first, 
	// selects it, then changes format.
	long nStart, nEnd;				// initial selection range (maybe just inesertion pt)
	edit.GetSel(nStart, nEnd);			// save initial selection 
	edit.ReplaceSel(strText);		// Replace sel contents w/text
	int nEndText = nStart + strText.GetLength(); // endpoint of newly inserted text
	edit.SetSel(nStart, nEndText);	// Select newly inserted text
	SetCharSymbol();				// change sel format to Symbol
	edit.SetSel(nEndText, nEndText);// leave sel at insertion pt after inserted text
}


CString CChatView::ParseOutFormatText(CString str)
{
	CRichEditCtrl& edit = GetRichEditCtrl();
	int posBeg = str.Find('{');		
	int posEnd = str.Find('}');
	int len = posEnd - posBeg;
	CString lnkPrt1 = str.Mid(posBeg, len);//find first part of formatted text
										   //link has two parts
	// assign first part of link rect
	CRect rect;
	LONG selPos1, selPos2;
	edit.GetSel(selPos1, selPos2);
	rect.TopLeft() = edit.GetCharPos(selPos2); 	

	//find link tag
	char tag = lnkPrt1[2];			// "0{" "1\" "2tag"
	SetFormat(tag);	// change format to underline 
											// blue or purple color for links/jumps
					//or could change to bold/italic
	//insert link text
	int len1 = lnkPrt1.GetLength();
	CString lnkTxt = lnkPrt1.Mid(4, len1-4);
	
	if (!lnkTxt.IsEmpty())
		edit.ReplaceSel(lnkTxt);

	SetFormat(tag);	//set format back (does a toggle)

	CString strRest = str.Mid(posEnd + 1);
	if ((tag == 'b') || (tag == 'i'))
		return strRest;
	
	//if link, find second part
	posBeg = strRest.Find('{');
	posEnd = strRest.Find('}');
	len = posEnd - posBeg;
	CString lnkPrt2 = strRest.Mid(posBeg, len);
	int len2 = lnkPrt2.GetLength();
	CString lnkId = lnkPrt2.Mid(4, len2 - 4); // "{0" "/1" "v2" "space3"
	
	//if link, create link object and add to our list
	CHyperLnk* pLnk = new CHyperLnk();
	pLnk->m_posBeg = selPos1; 
	edit.GetSel(selPos1, selPos2); 
	pLnk->m_posEnd = selPos1;
	rect.BottomRight() = GetRichEditCtrl().GetCharPos(selPos2);// assign second part 
																// of link rect
	pLnk->m_strLink = lnkId;
	pLnk->m_position = rect;
	pLnk->m_strName = lnkTxt;
	pLnk->SetLinkType(tag);
	
	m_links.AddTail(pLnk);

	str = strRest.Mid(posEnd + 1);
	return str;
}

void CChatView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	// Update state if Tutor mode changes
	if (lHint == HINT_TUTOR_MODE)
		EnablePane(DWORD(pHint));
	
	// Ignore all other updates.
	return;  	
}

//
// Enable/Disable whole pane
//
void CChatView::EnablePane(BOOL bEnable)
{
	// remember enabled state for command updating
	m_bEnabled = bEnable;
/* Don't disable input with Windows, since want to allow scrolling when grey
	// enable/disable input events to this pane
	// EnableWindow(bEnable); */
	// adjust background color to show state (richedits don't grey on disabling).
	if (m_bEnabled)
		GetRichEditCtrl().SetBackgroundColor(TRUE, NULL); // sets to system color
	else
		GetRichEditCtrl().SetBackgroundColor(FALSE, ::GetSysColor(COLOR_3DFACE));
}

// 
// OnKillFocus -- filter (reflected) kill focus event from control
//
BOOL  CChatView::OnKillFocus()
{
	// for WOZ version: ensure any unsent msg contents logged on loss of focus
	LogUnsentText(EV_LEFT_UNSENT);

	// always let message continue to parent
	return FALSE;	// means message not consumed
}

// 
// LogUnsentText -- Ensure any current unsent text has been logged
// nEvent is event under which to log it.
//
void CChatView::LogUnsentText(EventID nEvent /*=EV_UNSENT_CONTENTS*/)
{
/* Only needed for WOZ experiement when tutor shared screen so could read unsent text
	
	  // Do nothing if we have advanced to next line after student
	// submission, since logged it on student submission. Needed because
	// what GetLastInputLine return differs from last logged line by including
	// the extra CRLF we added to advance cursor to the next line.
	if (m_bOnSysReplyLine) return;

	// Else log in case line contents differ from last logged state
	CString csLine = GetLastInputLine();
	if (csLine != m_strLoggedLineState) {
		LogEventf(nEvent, "%s", csLine);
		m_strLoggedLineState = csLine;
	}
*/
}

/***************************** Hyperlink Support **********************************/

// return hyperlink data for link at client-area point; 
// NULL if not a link, or link not "live" now.
CHyperLnk* CChatView::HyperAt(CPoint point)
{
	// No links are live when disabled
	if (! m_bEnabled)
		return NULL;

	//Get closest char to point
	POINT pt;
	pt.x = point.x;
	pt.y = point.y;
	LPPOINT lppoint = &pt;
	LPARAM nPos = (LPARAM)lppoint;
	DWORD pos = SendMessage(EM_CHARFROMPOS , 0, nPos);
	LONG poschar = LOWORD(pos);

	// Only links in latest message should be "live", not those in history.
	// !!! Should probably change their appearance to show this.
	if (poschar < m_lCurMsgStartIndex)
		return NULL;
	
	// else look for associated Hyperlink data
	return GetLinkFromPos(poschar);
}

// return  link given character position, NULL if none.
CHyperLnk* CChatView::GetLinkFromPos(int poschar)
{
	POSITION pos = m_links.GetTailPosition();
	while (pos != NULL)
	{
		CHyperLnk* pLnk = m_links.GetPrev(pos);
		if ((pLnk->m_posBeg <= poschar) && (pLnk->m_posEnd >= poschar))
			return pLnk;
	}
	return NULL;
}

void CChatView::OnMouseMove(UINT nFlags, CPoint point) 
{
	m_bOnHyperText = (HyperAt(point) !=  NULL);
	CRichEditView::OnMouseMove(nFlags, point);
}

BOOL CChatView::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
	// set hand cursor over hyperlink, else standard arrow.
	::SetCursor(m_bOnHyperText ? AfxGetApp()->LoadCursor(IDC_LINK)
							   : AfxGetApp()->LoadStandardCursor(IDC_ARROW));
	return TRUE;
}

void CChatView::OnLButtonDown(UINT nFlags, CPoint point) 
{
	// Note HyperAt knows about enabled flag.
	CHyperLnk* pLnk = HyperAt(point);
	if (pLnk != NULL){
		// turn off hand cursor until next mouse move over link
		::SetCursor(AfxGetApp()->LoadStandardCursor(IDC_ARROW));
		m_bOnHyperText = FALSE;
		// tell the link to fire
		pLnk->HyperActivate(point, this);
		return;
	}

	// when input not allowed, eat this msg so caret doesn't appear
	if (m_bInputAllowed)
		CRichEditView::OnLButtonDown(nFlags, point);
}

void CChatView::PopupDef(CString term, CPoint point)
{
	LogEventf(EV_HINT_POPUP, "%s %d %d", term, point.x, point.y);
	CString strDef;
	term.MakeLower();
	if (! theApp.LookupDef(term, strDef) || strDef.IsEmpty())
		strDef = "Definition not found";	// show something in case of error
	TRACE("popping up %s = %s\n", term, strDef);
	CPopupWnd* pWnd = new CPopupWnd(); 
	pWnd->m_strDef = strDef;
	ClientToScreen(&point);
	pWnd->Create(this, point);

	// For log playback only: save pointer to currK_ent popup wnd for 
	// easy access when replaying the KILL_POPUP event
	// m_pPopup = pWnd;
}

// disable all existing links, changing format to normal text
void CChatView::DeleteLinks()
{
	// Following toggles format on links
	POSITION pos = m_links.GetHeadPosition();
	while (pos != NULL) {
		CHyperLnk* pLnk = m_links.GetNext(pos);
		
		GetRichEditCtrl().SetSel(pLnk->m_posBeg, pLnk->m_posEnd);
		SetFormat('\0'); // should clear link format
	}
	GetRichEditCtrl().SetSel(-1, -1);

	// following empties link storage list
	while (!m_links.IsEmpty())
		m_links.RemoveHead()->Delete();
}

//////////////////////////////////////////////////////////////////////////////////////////
//
// Menus for student responses
//
// Note: We can't easily recreate going through popup menus when playing back logs.
// So we split these operations into two steps, one of collecting the result from the
// user via the popup-menu and logging it, and second of processing the result further
// and submitting to help system. The collection phase is not run during log playback;
// but the recreation of the logged events will take its place and lead to the second
// phase. Running through dialogs is possible in log playback so we don't worry about 
// that. The whole thing would be simpler if we used our logging dialogs exclusively
// instead of popup menus.
//
//////////////////////////////////////////////////////////////////////////////////////////
// return location to place popup menu, screen coords
CPoint CChatView::GetMenuPos()
{
	CPoint ptMenu;
	// Try to align menu top with top last system text, backing up 
	// over move to next line. Windows will probably move up, however.
	CPoint ptEndLine = GetRichEditCtrl().GetCharPos(GetTextLength() - 1);
	ptMenu.y = ptEndLine.y;
	// Place menu left edge in right margin of client area, so that if it is 
	// moved up it won't obscure any text on previous lines.
	CRect rcClient;
	GetClientRect(rcClient);
	CClientDC dc(this);
	ptMenu.x = rcClient.Width() - dc.GetDeviceCaps(LOGPIXELSX)/4; // 1/4in margin

	ClientToScreen(&ptMenu);
	return ptMenu;
}

// Collect a quantity definition and submit to help system as student response:
// In two pieces for log playback: first gets quantity type with popup-menu,
// second piece chooses the actual quantity of that type.
void CChatView::GetQuantityType()
{
	// don't run menu during playback. Replay of logged events should have same effect.
	if (LogPlayerInPlayback()) return;

	// First use popup menu to choose major quantity type.
	CVarMenu mnuVar;
	mnuVar.CreatePopupMenu();
	mnuVar.AttachProblemMenu(theApp.GetDocument()->m_wConcept, TRUE);

	// Run popup menu with flag to return selected ID, not send WM_COMMAND.	
	CPoint point = GetMenuPos();
	int nResult = mnuVar.TrackPopupMenu(TPM_RETURNCMD | TPM_NONOTIFY| TPM_LEFTALIGN | 
		                                TPM_LEFTBUTTON | TPM_RIGHTBUTTON, 
										point.x, point.y, this);

	// Log quantity type choice, including code (maybe cancel) + readable ids
	// !!! logged strVecProp is empty for scalar quantities
	CString strVecProp, strVecPrefix, strTypeId;
	if (nResult) {
		int nVarType = mnuVar.SplitQuantId(nResult, strVecProp, strVecPrefix);
		strTypeId = CVarView::LookupTypeId(nVarType);
	}
	LogEventf(EV_QUANT_DEF, "%d %s %s", nResult, strVecProp, strTypeId);

	// and go on to define the quant and submit
	GetQuantityDef(nResult);
}

void CChatView::GetQuantityDef(int nResult)
{
	CVarMenu mnuVar;   // for splitting
	CVariable var;
	CString strVecProp = "NIL"; // default to send to helpsys for non-vector
	CString strVecPrefix;			// prefix to add when adding text.
	if (nResult)	// made a selection
	{
		// result id may code vector + property (mag, dir, etc)
		nResult = mnuVar.SplitQuantId(nResult, strVecProp, strVecPrefix);
		TRACE("Popup menu got variable type %d prop:%s\n", nResult, strVecProp);
		// Create variable of selected type. Note may be a vector variable,
		ASSERT(nResult >= ID_VARIABLE_ADDFIRST);
		ASSERT(nResult <= ID_VARIABLE_ADDLAST);
		var.m_nType = nResult;
		// Edit the variable properties with appropriate dialog.
		// This uses special "choose sought" mode and doesn't check result
		CDialog* pDlg = var.GetPropertyDlg();
		((CDrawObjDlg*)pDlg)->m_bSought = TRUE;
		((CDrawObjDlg*)pDlg)->m_bNoCheck = TRUE;
		nResult = (pDlg->DoModal() == IDOK);
		delete pDlg;
	} 

	// if cancelled at any point, tell help system so it can decide what to do
	LPCTSTR pszResult;
	if (! nResult) {
		AddText(m_strPrompt + "[Cancelled]\r\n");
		pszResult = HelpSystemExecf("(handle-student-response Cancel)");
	}
	else // picked one
	{
		// Add result quantity description as student input line
		// For vector variables definition may already begin "magnitude of ". If so
		// we take it out then add correct prefix. (may be mag again but this is simple.)
		const char szMagPrefix[] = "magnitude of ";	// w/space so BaseDef has none
		const int nMagPrefix = sizeof(szMagPrefix) - 1;
		CString strBaseDef = var.GetDef();
		CString strDefStart = strBaseDef.Left(nMagPrefix);
		if (strDefStart.CompareNoCase(szMagPrefix) == 0)
			strBaseDef = strBaseDef.Mid(nMagPrefix);

		AddText(m_strPrompt + "The " + strVecPrefix +  " " + strBaseDef + "\r\n");
	
		// Get quantity defining parameters from variable and
		// submit call to help system as student response argument. To simplify things we
		// include reader-dispatch to conversion function (wb-quant 'api-cmd 'vecprop)
		pszResult = HelpSystemExecf( "(handle-student-response #.(wb-quant \'%s \'%s))", 
									  var.GetCheckCmd(), strVecProp);
	}
	// Send the response to the screen
	theApp.GetMainFrame()->ShowHint(pszResult, Hint); 
}

// keep dialog CWnd in static so last selection persists across modal invocations
// in CWnd member var. !!! Note also means that m_bPrinciples and m_bSelect flags
// persist as well, so have to reset before each use.
static CPsmDlg psmDlg;	

// Collect a principle definition and submit to help system as student response
void CChatView::GetPrinciple()
{
	LPCTSTR pszResult;
	psmDlg.m_bPrinciples = TRUE;
	psmDlg.m_bSelect = TRUE;
	if (psmDlg.DoModal() != IDOK) {
		AddText(m_strPrompt + "[Cancelled]\r\n");
		pszResult = HelpSystemExecf("(handle-student-response Cancel)");
	} else {
		TRACE("Selected psm %s\n", psmDlg.m_strHelpID);
		AddText(m_strPrompt + psmDlg.m_strPSM + "\r\n");
		pszResult = HelpSystemExecf("(handle-student-response %s)", psmDlg.m_strHelpID);
	}
	// Send the response to the screen
	theApp.GetMainFrame()->ShowHint(pszResult, Hint); 
}

// Collect an equation definition. If bSubmit, submit to help system as student response
// Set bSubmit to FALSE to just show the menu, for perusing help pages.
void CChatView::GetEquation(BOOL bSubmit /*=TRUE*/)
{
	psmDlg.m_bPrinciples = FALSE;
	psmDlg.m_bSelect = bSubmit;
	int nResult = psmDlg.DoModal();
	// if not submitting choice, don't do anything more
	if (! bSubmit) return;

	LPCTSTR pszResult;
	if (nResult != IDOK) {
		AddText(m_strPrompt + "[Cancelled]\r\n");
		pszResult = HelpSystemExecf("(handle-student-response Cancel)");
	} else {
		TRACE("Selected psm %s\n", psmDlg.m_strHelpID);
		AddText(m_strPrompt + psmDlg.m_strPSM + "\r\n");
		pszResult = HelpSystemExecf("(handle-student-response %s)", psmDlg.m_strHelpID);
	}
	// Send the response to the screen
	theApp.GetMainFrame()->ShowHint(pszResult, Hint); 
}

void CChatView::GetMenuSelection(CString& strSpec)
{
	// don't run menu during playback. Replay of logged events should have same effect.
	if (LogPlayerInPlayback()) return;

	// Split the menu spec at vbar delimiters
	CStringList strItems;
	SplitStr(strSpec, strItems, "|");

	// Build popup menu containing choices.
	// Command ID is just 1-based index into the stringlist (1-based so we can
	// distinguish zero return value for failure).
	CMenu menu;
	menu.CreatePopupMenu();
	POSITION pos = strItems.GetHeadPosition();
	int n = 0;
	while (pos != NULL) {
		n += 1;
		CString strItem = strItems.GetNext(pos);
		TRACE("Adding menu item %d = %s\n", n, strItem);
		if (! strItem.IsEmpty())
			menu.AppendMenu(MF_STRING, n, strItem);
	}

	// Run popup menu with flag to return selected ID, note send WM_COMMAND.
	CString strChosen;		// text of student choice, empty if none
	CPoint point = GetMenuPos();
	int nResult = menu.TrackPopupMenu(TPM_RETURNCMD | TPM_NONOTIFY| TPM_LEFTALIGN | 
		                              TPM_LEFTBUTTON | TPM_RIGHTBUTTON, 
									  point.x, point.y, this);
	if (nResult)	// made a selection
	{
		POSITION posChosen = strItems.FindIndex(nResult - 1);
		if (posChosen)
			strChosen = strItems.GetAt(posChosen);
		TRACE("Chose menu item %d = %s\n", nResult, strChosen);
	}

	// Log it
	LogEventf(EV_MENU_CHOICE, "%s", strChosen); 
	// and continue
	SubmitMenuSelection(strChosen);
}

void CChatView::SubmitMenuSelection(CString strChosen)
{
	LPCTSTR pszResult;
	// if cancelled any choice, tell help system so it can decide what to do
	if (strChosen.IsEmpty()) {
		AddText(m_strPrompt + "[Cancelled]\r\n");
		pszResult = HelpSystemExecf("(handle-student-response Cancel)");
	}
	else // picked one
	{
		// Add result text as student input
		AddText(m_strPrompt + strChosen + "\r\n");
	
		// Get quantity defining parameters from variable and
		// submit call to help system as student response argument. To simplify things we
		// include reader-dispatch to conversion function on the other side
		pszResult = HelpSystemExecf( "(handle-student-response \"%s\")", strChosen);
	}
	// Send the response to the screen
	theApp.GetMainFrame()->ShowHint(pszResult, Hint); 
}

//
// For copying to clipboard
//
void CChatView::OnUpdateEditCopy(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(TRUE); // 
}

void CChatView::OnEditCopy() 
{
	// For some reason, controls Copy() method doesn't work. Possibly some problem
	// with supporting context needed by CRichEditView. So we just copy text
	// ourselves 
	GetRichEditCtrl().SetSel(0, -1);
	CString strText = GetRichEditCtrl().GetSelText();
	GetRichEditCtrl().SetSel(-1, -1);
	if(OpenClipboard())
	{
		HGLOBAL clipbuffer;
		char * buffer;
		EmptyClipboard();
		clipbuffer = GlobalAlloc(GMEM_DDESHARE, strText.GetLength()+1);
		buffer = (char*)GlobalLock(clipbuffer);
		strcpy(buffer, LPCSTR(strText));
		GlobalUnlock(clipbuffer);
		SetClipboardData(CF_TEXT,clipbuffer);
		CloseClipboard();
	}
}

// For framework's OLE support. It's idea is that CRichEditView will be linked to
// a CRichEditDoc which is a COleDocument which may contain OLE items. To update standard menu items, the framework may query a view 
// to find out if one of those OLE item is selected in that view. Since we don't provide
// any interface for selecting an OLE item, we always return FALSE.
// We are not actually using a CRichEditDoc, which means use of framework CRichEditView 
// may have problems or assertion failures, but we are
// linked to a COleDocument nonetheless so this can get called. 
BOOL CChatView::IsSelected(const CObject* pDocItem) const
{
	// return CRichEditView::IsSelected(pDocItem);
	return FALSE;	
}



