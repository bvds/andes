//////////////////////////////////////////////////////////////////////
//
// DocPages.h -- declarations for the Document property page dialogs
// 
// $Id: DOCPAGES.H,v 1.1 2005/01/24 16:28:09 bvds Exp $
/////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////
// CGeneralPage dialog
typedef struct { 
    int id; 
	char* str; 
} PROBLEM_CONCEPT; 

extern const PROBLEM_CONCEPT concepts[];
extern const int nConcepts;

class CGeneralPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CGeneralPage)

// Construction
public:
	CGeneralPage();
	~CGeneralPage();

// Dialog Data
	//{{AFX_DATA(CGeneralPage)
	enum { IDD = IDD_GENERAL_PAGE };
	CListBox	m_lstConcept;
	CString	m_strProblemId;
	int		m_nProblemType;
	BOOL	m_bIncludePlan;
	int		m_nKBType;
	int		m_nAssessor;
	//}}AFX_DATA

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CGeneralPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CGeneralPage)
	afx_msg void OnProblemType();
	afx_msg void OnQualitativeButton();
	afx_msg void OnExampleButton();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	void DDX_FillSelConceptList(CDataExchange* pDX, int nIDC, DWORD &SelFlag);

};

/////////////////////////////////////////////////////////////////////////////
// CObjectsPage dialog

class CObjectsPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CObjectsPage)

// Construction
public:
	CObjectsPage();
	~CObjectsPage();

// Dialog Data
	//{{AFX_DATA(CObjectsPage)
	enum { IDD = IDD_OBJECTS_PAGE };
	//}}AFX_DATA

//	CStringList	m_strObjects;

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CObjectsPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CObjectsPage)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

/////////////////////////////////////////////////////////////////////////////
// CTimesPage dialog

class CTimesPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CTimesPage)

// Construction
public:
	CTimesPage();
	~CTimesPage();

// Dialog Data
	//{{AFX_DATA(CTimesPage)
	enum { IDD = IDD_TIMES_PAGE };
	//}}AFX_DATA

//	CStringList	m_strTimes;

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CTimesPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CTimesPage)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

/////////////////////////////////////////////////////////////////////////////
// CBranchesPage dialog

class CBranchesPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CBranchesPage)

// Construction
public:
	CBranchesPage();
	~CBranchesPage();

// Dialog Data
	//{{AFX_DATA(CBranchesPage)
	enum { IDD = IDD_BRANCHES_PAGE };
	CString	m_strDirections;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CBranchesPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CBranchesPage)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

/////////////////////////////////////////////////////////////////////////////
// CListsPage dialog

class CListsPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CListsPage)

// Construction
public:
	CListsPage();
	~CListsPage();

// Dialog Data
	//{{AFX_DATA(CListsPage)
	enum { IDD = IDD_LISTS_PAGE };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CListsPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CListsPage)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};



/////////////////////////////////////////////////////////////////////////////
// CPositionsPage dialog

class CPositionsPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CPositionsPage)

// Construction
public:
	CPositionsPage();
	~CPositionsPage();

// Dialog Data
	//{{AFX_DATA(CPositionsPage)
	enum { IDD = IDD_POSITIONS_PAGE };
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CPositionsPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CPositionsPage)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};
/////////////////////////////////////////////////////////////////////////////
// CStatementPage dialog

class CStatementPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CStatementPage)

// Construction
public:
	CStatementPage();
	~CStatementPage();

// Dialog Data
	//{{AFX_DATA(CStatementPage)
	enum { IDD = IDD_STATEMENT_PAGE };
	CEdit	m_editGraphicFile;
	CEdit	m_editStmt;
	CString	m_strStatement;
	CString	m_strGraphicFile;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CStatementPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CStatementPage)
	virtual BOOL OnInitDialog();
	afx_msg void OnBrowse();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	CFont m_fontText;
};
