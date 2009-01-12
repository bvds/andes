// RuleQDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CRuleQDlg dialog

class CRuleQDlg : public CDialog
{
// Construction
public:
	CRuleQDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CRuleQDlg)
	enum { IDD = IDD_RULE_QUERY };
	CButton	m_btnQ1Enter;
	CButton	m_btnQ2Enter;
	CStatic	m_textResult;
	CStatic	m_textQ2;
	CStatic	m_textQ1;
	CComboBox	m_cboQ2Choices;
	CComboBox	m_cboQ1Choices;
	//}}AFX_DATA

	CString m_strPathName;		// full path of question file to load.

	class CMCQuestion			// Single multiple choice question data 
	{
	public:
		CString m_strQuestion;		// the question text
		CStringList m_strChoices;	// list of choices as strings
		int m_nAnswer;				// 1-based index of correct answer

		BOOL LoadFromFile(FILE* fp);// parse 1 question's data from open stdio file.

		static const char* szAnswer; // answer string marks end of choice lines
	private:
		BOOL GetLine(FILE* fp, char* pBuf, int nBufLen);
	};

	// Our dialogs have exactly two questions, 
	CMCQuestion m_q1;				// Q1 tests rule antecedent
	CMCQuestion m_q2;				// Q2 tests rule consequent
	BOOL LoadFile();				// loads data into q1 and q2 and inits controls

	enum { OnQ1, OnQ2, Finished } m_state;	// where they are in answering
	BOOL m_bCanQuit;				// if set, Esc (cancel) means quit
	
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRuleQDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void OnQ2Answer(int nSel);
	void OnQ1Answer(int nSel);
	BOOL m_bFirstShow;				// set for initial show of dialog
	
	// Generated message map functions
	//{{AFX_MSG(CRuleQDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnBtnQ1Enter();
	afx_msg void OnBtnQ2Enter();
	virtual void OnCancel();
	afx_msg void OnDontknow();
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
