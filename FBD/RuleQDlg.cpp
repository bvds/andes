// RuleQDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "helpifc.h"
#include "FBDDoc.h"		// just for CCheckedObj::ApplyStatus, urgh 
#include "RuleQDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CRuleQDlg dialog


CRuleQDlg::CRuleQDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CRuleQDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CRuleQDlg)
	//}}AFX_DATA_INIT

	// just for testing, default Q1 ans = 2, Q2 Ans = 1
	m_q1.m_nAnswer = 2;
	m_q2.m_nAnswer = 1;
	
	m_state = OnQ1;
	m_bFirstShow = FALSE;
	m_bCanQuit = FALSE;		// we don't let them quit till they have tried both
}


void CRuleQDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CRuleQDlg)
	DDX_Control(pDX, IDC_BTN_Q1_ENTER, m_btnQ1Enter);
	DDX_Control(pDX, IDC_BTN_Q2_ENTER, m_btnQ2Enter);
	DDX_Control(pDX, IDC_TEXT_RESULT, m_textResult);
	DDX_Control(pDX, IDC_TEXT_Q2, m_textQ2);
	DDX_Control(pDX, IDC_TEXT_Q1, m_textQ1);
	DDX_Control(pDX, IDC_Q2_CHOICES, m_cboQ2Choices);
	DDX_Control(pDX, IDC_Q1_CHOICES, m_cboQ1Choices);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CRuleQDlg, CDialog)
	//{{AFX_MSG_MAP(CRuleQDlg)
	ON_BN_CLICKED(IDC_BTN_Q1_ENTER, OnBtnQ1Enter)
	ON_BN_CLICKED(IDC_BTN_Q2_ENTER, OnBtnQ2Enter)
	ON_BN_CLICKED(IDC_DONTKNOW, OnDontknow)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRuleQDlg message handlers

BOOL CRuleQDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	Logf("Rule-Q-dlg");
	
	// !!! Log event
	// Load questions from file in m_strPathName
	if (!LoadFile())			// load failed (message should have been shown)
	{
		EndDialog(IDABORT);
		return TRUE;
	}
	
	// Initialize dropdown selctions to first element (arbitary, but better to
	// show something then blank combo box).
	m_cboQ1Choices.SetCurSel(0);
	m_cboQ2Choices.SetCurSel(0);

	// set focus on question 1 combo. Will drop it on first show
	// of window (doesn't work to drop it now since MFC moves window
	// before showing it, but doesn't update dropped window).
	m_cboQ1Choices.SetFocus();
	m_state = OnQ1;

	m_bFirstShow = TRUE;

	return FALSE;  // return TRUE unless you set the focus to a control
}

// drop question one combo on initial display, so they see its a choice
void CRuleQDlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CDialog::OnShowWindow(bShow, nStatus);
	
	if (m_bFirstShow) {
		m_cboQ1Choices.ShowDropDown();
		m_bFirstShow = FALSE;
	}
}

// Load question data from file. Transfers choices to controls if successful
BOOL CRuleQDlg::LoadFile()
{
	BOOL bSucceeded;
	FILE * fp;
	CString strMsg;
	bSucceeded = (fp = fopen(m_strPathName, "r")) != NULL;
	if (!bSucceeded)	// error opening
	{	
		// For now, just show error message in dialog question field
		strMsg.Format ("[Internal Error. Couldn't open dialog file %s   Hit Esc to continue]",
						m_strPathName); 
		theApp.DoWarningMessage(strMsg, this);
		return FALSE;

		/* // For now, just show error message in dialog question field
		m_textQ1.SetWindowText(strMsg);
		m_bCanQuit = TRUE; // makes Esc (cancel) mean quit, not don't know. 
		return; */
	}

	// Stream in the two questions' data
	bSucceeded = m_q1.LoadFromFile(fp) &&
				 m_q2.LoadFromFile(fp) ;
	if (fp) 
		fclose(fp);

	if (!bSucceeded)	// error loading data
	{	// For now, just show error message in dialog question field
		strMsg.Format ("[Internal Error. Syntax error in dialog file %s   Hit Esc to continue]",
						m_strPathName); 
		theApp.DoWarningMessage(strMsg, this);
		return FALSE;
		/* ( m_textQ1.SetWindowText(strMsg);
		m_bCanQuit = TRUE; // makes Esc (cancel) mean quit, not don't know.
		return; */
	}

	// Init controls from file data

	m_textQ1.SetWindowText(m_q1.m_strQuestion);
	// Delete any existing strings, since this may be used to reset 
	m_cboQ1Choices.ResetContent();
	POSITION pos = m_q1.m_strChoices.GetHeadPosition();
	while (pos != NULL) {
		CString strChoice = m_q1.m_strChoices.GetNext(pos);
		m_cboQ1Choices.AddString(strChoice);
	}
	
	m_textQ2.SetWindowText(m_q2.m_strQuestion);
	m_cboQ2Choices.ResetContent();
	pos = m_q2.m_strChoices.GetHeadPosition();
	while (pos != NULL) {
		CString strChoice = m_q2.m_strChoices.GetNext(pos);
		m_cboQ2Choices.AddString(strChoice);
	}

	// Initialize dropdown selctions to first element (arbitary, but better to
	// show something then blank combo box).
	m_cboQ1Choices.SetCurSel(0);
	m_cboQ2Choices.SetCurSel(0);

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////////
//
// Dialog file parsing:
//

// We use nested class for question data object loadable from text file
// Might also be useful elsewhere in system

// Answer line format:
const char* CRuleQDlg::CMCQuestion::szAnswer = "A="; // prefix of answer line
const int nAnswerLen = 2;							 // length of prefix
const char* szAnswerFmt = "A=%d";					 // sscanf fmt for reading answer

// Load next non-blank line from file into buffer as string. ret TRUE if succeeded
BOOL CRuleQDlg::CMCQuestion::GetLine(FILE* fp, char* pBuf, int nLen)
{
	// loop till get a nonblank line
	do {
		if (fgets(pBuf, nLen, fp) == NULL) {
			TRACE("MCQ: Unexepected EOF\n");
			return FALSE;
		}
	} while	(pBuf[0] == '\n');
	
	/* // clobber trailing newline in buf.
	 pBuf[strlen(pBuf) - 1] = '\0'; */
	// TRACE("MCQ read line: |%s", pBuf);
	return TRUE;
}

// Load a single question from file									 
BOOL CRuleQDlg::CMCQuestion::LoadFromFile(FILE* fp)
{
	const int MAXLINE = 512;
	char szLine[MAXLINE] = "";

	// First line is question text
	if (! GetLine(fp, szLine, MAXLINE)) return FALSE;
	m_strQuestion = szLine;
	TRACE("MCQ got question: |%s", m_strQuestion);

	// loop reading choices till hit answer line 
	if (! GetLine(fp, szLine, MAXLINE)) return FALSE;
	while (strncmp(szLine, szAnswer, nAnswerLen) != 0) // haven't hit answer line
	{
		// split choice text out of "id<SP>choice-text\n"
		char szChoice[MAXLINE];
		if (sscanf(szLine, "%*s %[^\n]", szChoice) != 1) return FALSE;
		TRACE("MCQ got choice: %s\n", szChoice);

		// add it to string list for choices (copies into CString, I assume).
		m_strChoices.AddTail(szChoice);	

		// advance to next line
		if (! GetLine(fp, szLine, MAXLINE)) return FALSE;
	} 

	// read answer number 
	if (sscanf(szLine, szAnswerFmt, &m_nAnswer) != 1) return FALSE;
	TRACE("MCQ got answer= %d\n", m_nAnswer);

	// ensure answer number is in range of choices we have
	if (m_nAnswer < 1 || m_nAnswer > m_strChoices.GetCount())
		return FALSE;

	return TRUE;
}

///////////////////////////////////////////////////////////
//
// Dialog event handling:
//
////////////////////////////////////////////////////////////

void CRuleQDlg::OnBtnQ1Enter() // Hit enter button to submit Q1 answer
{
	int nSel = m_cboQ1Choices.GetCurSel();
	if (nSel == -1) {						// no selection! 
		MessageBeep(MB_ICONASTERISK);
		return;
	}
	OnQ1Answer(nSel + 1);	// convert to 1-based choice number
}

// worker routine to handle answer for Q1 (antecedent)
void CRuleQDlg::OnQ1Answer(int nChoice)	// nChoice is 1-based choice num for help sys
{
	Logf("Q1-Answer %d  %d", nChoice, nChoice == m_q1.m_nAnswer);
	if (nChoice == m_q1.m_nAnswer) // Chose correct answer  
	{
		// Show next question and move focus to it

		// Disable Q1 controls so can't change
		m_btnQ1Enter.ShowWindow(SW_HIDE);
		m_cboQ1Choices.EnableWindow(FALSE);

		// Show Q2 control group
		m_textQ2.ShowWindow(SW_SHOW);
		m_btnQ2Enter.ShowWindow(SW_SHOW);
		m_cboQ2Choices.ShowWindow(SW_SHOW);
		// Move focus and drop the choice list
		m_cboQ2Choices.SetFocus();
		m_cboQ2Choices.ShowDropDown();

		m_state = OnQ2;				// now on Question 2
		return;
	}

	// Else chose incorrect answer: 

	// notify help system, which should return a command string or T if there is none
	LPCTSTR pszResult = HelpSystemExecf("(antecedent-answer %d)", nChoice);
	CString strDummy, strCommand;
	CStringList strErrs;
	CCheckedObj::SplitResult(pszResult, strDummy, strCommand);

	if (strCommand.IsEmpty()) // No command sent or DDE failed.
	{ 
		// Show a little feedback and quit.
		m_textResult.SetWindowText("Incorrect");
		m_textResult.ShowWindow(SW_SHOW); 
		m_textResult.UpdateWindow();
		CWaitCursor showWait;
		Sleep(1000);
		EndDialog(IDOK);
		return;
	}
	// returned command is either "show-lesson" to open a lesson or "show-dlg" to show
	// another dialog to replace this one. In latter case we re-init controls

	// check for command to reset to a new dialog
	const char* szShowCmd = "show-dlg";
	if (strncmp(strCommand, szShowCmd, strlen(szShowCmd)) == 0) 
	{
		// Pull out file name and build path name to open
		CString strFileName = strCommand.Mid(strlen(szShowCmd + 1));
		TRACE("Re-initializing dialog from |%s|\n", strFileName);
		Logf("Rule-Q-reset");
		m_strPathName = g_strAndesDir + g_szLessonDir + "\\" + strFileName;

		// Re-init controls from file.
		if (!LoadFile())			// load failed (message should have been shown)
		{
			EndDialog(IDABORT);
			return;
		}
	
		// Highlight question 1 again
		m_cboQ1Choices.SetFocus();
		m_cboQ1Choices.ShowDropDown();
	}
	else
		ExecuteCmd(strCommand);	// just execute
}


void CRuleQDlg::OnBtnQ2Enter()				// hit enter button to submit Q2 choice
{
	int nSel = m_cboQ2Choices.GetCurSel();
	if (nSel == -1) {						// no selection! show message
		MessageBeep(MB_ICONASTERISK);
		return;
	}
	OnQ2Answer(nSel + 1); // convert to 1-based
}

// worker routine to handle answer for Q2 (consequent)
void CRuleQDlg::OnQ2Answer(int nChoice)		// nChoice is 1-based num for help sys
{
	Logf("Q2-Answer %d  %d", nChoice, nChoice == m_q2.m_nAnswer);

	if (nChoice == m_q2.m_nAnswer) // Correct answer 
	{
		// For correct answer: show result
		m_textResult.SetWindowText("Correct.  Change your entry accordingly.");
		m_textResult.ShowWindow(SW_SHOW); // default text says "that's correct"
		m_textResult.UpdateWindow();
		// Freeze Q2
		m_btnQ2Enter.ShowWindow(SW_HIDE);
		m_cboQ2Choices.EnableWindow(FALSE);
		// mark dialog state as done
		m_state = Finished;
	}
	m_bCanQuit = TRUE;				// can escape once have tried both questions

	// Right or wrong: In either case: notify help system of selection 
	// returns mini-lesson cmd or T if none
	// for new conchelp: pass incorrect choices as negative values
	int nChoiceArg = (nChoice == m_q2.m_nAnswer) ? nChoice : -nChoice;
	LPCTSTR pszResult = HelpSystemExecf("(consequent-answer %d)", nChoiceArg);
	CString strDummy, strCommand;
	CStringList strErrs;
	CCheckedObj::SplitResult(pszResult, strDummy, strCommand);

	if (strCommand.IsEmpty()) // No mini-lesson command sent (or DDE failed.)
	{ 
		//If was wrong answer, give incorrect feedback 
		if (nChoice != m_q2.m_nAnswer) { 
			m_textResult.SetWindowText("Incorrect.");
			m_textResult.ShowWindow(SW_SHOW); 
			m_textResult.UpdateWindow();
		}
		// set state to quit below
		m_state = Finished;			
	}
	else
		ExecuteCmd(strCommand); // may enter mini-lesson mode 
	
	// if finished with questions, give them a second to read result, then quit
	if (m_state == Finished) {
		CWaitCursor showWait;
		Sleep(1000);
		EndDialog(IDOK);
	}
	// !!! NB else we loop back to same state in dialog again. 
}

void CRuleQDlg::OnDontknow() // hit our catch all "don't know" button
{
	// If on a question, treat as always incorrect choice 0
	if (m_state == OnQ1) {
		OnQ1Answer(0);
	}
	else if (m_state == OnQ2) {
		OnQ2Answer(0);
	}
	// else not on a question: just ignore
}

void CRuleQDlg::OnCancel() // User hit escape key (or cancel button, if we show it)
{
	Logf("Cancel-RuleQ");
	if ((m_state != Finished) && !m_bCanQuit) 
	{
		OnDontknow();	// first time, treat as incorrect answer to current question
		m_bCanQuit = TRUE;	// Next time can really quits
		return;
	}
	// else quit
	CDialog::OnCancel();
}






