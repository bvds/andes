//////////////////////////////////////////////////////////////////////
//
// DocPages.cpp -- classes for the Document property page dialogs
//
// Used in author mode in the document property sheet to set
// parameters of the problem.
/////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "DocPages.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

//
// concepts: Table of <flag, name> pairs for setting concept flags.
//
// These are mainly consulted to control the choices of variables
// offered, see VarMenu code. However this also provides an extensible
// (up to 32 elements as currently represented) set of named features 
// that can be used to control various other problem-specific things:
//	Vector-Grid enables coarse grid
//  Rotation enables angular quantities
//  Rotation|Circular enables z-axis
//  Gravitation enables "Gravitational" rather than Weight on force menu
//
// Probably z-axis should be its own feature.
//
//
extern const PROBLEM_CONCEPT concepts[] = 
{
	ID_PROB_KINEMATICS,	"Kinematics",
	ID_PROB_FORCE,		"Force", 
	ID_PROB_ENERGY,		"Energy",					
	ID_PROB_CIRCMOTION,	"Circular Motion",
	ID_PROB_GRAVITATION, "Gravitation",
	ID_PROB_ROTKINEMATICS, "Rotation",
	ID_PROB_WORK,		"Work",
	ID_PROB_VECTOR,		"Vector Grid",
	ID_PROB_MOMENTUM,	"Momentum",
    ID_PROB_RELVEL,     "Relative velocity",
	ID_PROB_CIRCUITS,	"Circuits",
	ID_PROB_EM,         "Electricity & Magnetism",
	ID_PROB_FLUIDS,     "Fluids",
	ID_PROB_PROBABILITY, "Probability",
	ID_PROB_CHANGING_VOLTAGE, "Changing Voltage",
};

const int nConcepts ARRAY_SIZE(concepts);
/////////////////////////////////////////////////////////////////////////////
// CGeneralPage property page

IMPLEMENT_DYNCREATE(CGeneralPage, CPropertyPage)

CGeneralPage::CGeneralPage() : CPropertyPage(CGeneralPage::IDD)
{
	//{{AFX_DATA_INIT(CGeneralPage)
	m_strProblemId = _T("");
	m_nProblemType = -1;
	m_bIncludePlan = FALSE;
	m_nKBType = -1;
	m_nAssessor = -1;
	//}}AFX_DATA_INIT
}

CGeneralPage::~CGeneralPage()
{
}

void CGeneralPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CGeneralPage)
	DDX_Control(pDX, IDC_PROBCONCEPT_LIST, m_lstConcept);
	DDX_Text(pDX, IDC_PROBLEM_NAME, m_strProblemId);
	DDX_Radio(pDX, IDC_PROBLEM_TYPE, m_nProblemType);
	DDX_Check(pDX, IDC_INCLUDEPLAN, m_bIncludePlan);
	DDX_Radio(pDX, IDC_KBTYPE, m_nKBType);
	DDX_Radio(pDX, IDC_ASSESSOR, m_nAssessor);
	//}}AFX_DATA_MAP
	DDX_FillSelConceptList(pDX, IDC_PROBCONCEPT_LIST, theApp.GetDocument()->m_wConcept);
}


BEGIN_MESSAGE_MAP(CGeneralPage, CPropertyPage)
	//{{AFX_MSG_MAP(CGeneralPage)
	ON_BN_CLICKED(IDC_PROBLEM_TYPE, OnProblemType)
	ON_BN_CLICKED(IDC_QUALITATIVE_BUTTON, OnQualitativeButton)
	ON_BN_CLICKED(IDC_EXAMPLE_BUTTON, OnExampleButton)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGeneralPage message handlers
/////////////////////////////////////////////////////////////////////////////
void CGeneralPage::DDX_FillSelConceptList(CDataExchange* pDX, int nIDC, DWORD &wSelFlag)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);

	if (!pDX->m_bSaveAndValidate)
	{
		::SendMessage(hWndCtrl, LB_RESETCONTENT, (WPARAM)0, (LPARAM)0);
		for (int i = 0; i < nConcepts; i++)
		{		
			CString strValue = concepts[i].str;
			int nSel = ::SendMessage(hWndCtrl, LB_ADDSTRING, 0, (LPARAM)(LPSTR)(LPCTSTR)strValue);
			if (wSelFlag & concepts[i].id)
				::SendMessage(hWndCtrl, LB_SETSEL, (WPARAM)TRUE, (LPARAM)(UINT)nSel);
		}
	}
	else
	{
		wSelFlag = 0;
		int nSelCount= (int)::SendMessage(hWndCtrl, LB_GETSELCOUNT, 0, 0);
		if (nSelCount > 0)
		{
			int SelectSet[32];						// set of selected indices
			::SendMessage(hWndCtrl, LB_GETSELITEMS, nSelCount, (LPARAM)&SelectSet);
			for (int i=0; i<nSelCount; i++)
			{
				CString value;
				value.GetBufferSetLength(255);
				::SendMessage(hWndCtrl, LB_GETTEXT, SelectSet[i], (LPARAM)(LPCTSTR)value);
				for (int i = 0; i < nConcepts; i++)
				{	
					if (value == concepts[i].str)
					{
						wSelFlag = wSelFlag | concepts[i].id;
					}
				}
			}
		}
	}
}

void CGeneralPage::OnProblemType() 
{
	m_lstConcept.EnableWindow(TRUE);
	
}

void CGeneralPage::OnQualitativeButton() 
{
	m_lstConcept.EnableWindow(FALSE);
	
}

void CGeneralPage::OnExampleButton() 
{
	m_lstConcept.EnableWindow(FALSE);
	
}

// CObjectsPage property page

IMPLEMENT_DYNCREATE(CObjectsPage, CPropertyPage)

CObjectsPage::CObjectsPage() : CPropertyPage(CObjectsPage::IDD)
{
	//{{AFX_DATA_INIT(CObjectsPage)
	//}}AFX_DATA_INIT
}

CObjectsPage::~CObjectsPage()
{
}

void CObjectsPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CObjectsPage)
	//}}AFX_DATA_MAP
	DDX_FillEdit(pDX, IDC_OBJECT_TEXT,  &theApp.GetDocument()->m_strObjects);
}


BEGIN_MESSAGE_MAP(CObjectsPage, CPropertyPage)
	//{{AFX_MSG_MAP(CObjectsPage)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CObjectsPage message handlers

/////////////////////////////////////////////////////////////////////////////
// CTimesPage property page

IMPLEMENT_DYNCREATE(CTimesPage, CPropertyPage)

CTimesPage::CTimesPage() : CPropertyPage(CTimesPage::IDD)
{
	//{{AFX_DATA_INIT(CTimesPage)
	//}}AFX_DATA_INIT
}

CTimesPage::~CTimesPage()
{
}

void CTimesPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTimesPage)
	//}}AFX_DATA_MAP
	DDX_FillEdit(pDX, IDC_TIMES_TEXT, &theApp.GetDocument()->m_strTimes);
}


BEGIN_MESSAGE_MAP(CTimesPage, CPropertyPage)
	//{{AFX_MSG_MAP(CTimesPage)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTimesPage message handlers


/////////////////////////////////////////////////////////////////////////////
// CBranchesPage property page

IMPLEMENT_DYNCREATE(CBranchesPage, CPropertyPage)

CBranchesPage::CBranchesPage() : CPropertyPage(CBranchesPage::IDD)
{
	//{{AFX_DATA_INIT(CBranchesPage)
	m_strDirections = _T("");
	//}}AFX_DATA_INIT
}

CBranchesPage::~CBranchesPage()
{
}

void CBranchesPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CBranchesPage)
	DDX_Text(pDX, IDC_BRANCHES_TEXT, m_strDirections);
	//}}AFX_DATA_MAP
	DDX_FillEdit(pDX, IDC_BRANCHES_TEXT,  &theApp.GetDocument()->m_strBranches);
}


BEGIN_MESSAGE_MAP(CBranchesPage, CPropertyPage)
	//{{AFX_MSG_MAP(CBranchesPage)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CBranchesPage message handlers


/////////////////////////////////////////////////////////////////////////////
// CListsPage property page

IMPLEMENT_DYNCREATE(CListsPage, CPropertyPage)

CListsPage::CListsPage() : CPropertyPage(CListsPage::IDD)
{
	//{{AFX_DATA_INIT(CListsPage)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

CListsPage::~CListsPage()
{
}

void CListsPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CListsPage)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CListsPage, CPropertyPage)
	//{{AFX_MSG_MAP(CListsPage)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CListsPage message handlers

/////////////////////////////////////////////////////////////////////////////
// CPositionsPage property page

IMPLEMENT_DYNCREATE(CPositionsPage, CPropertyPage)

CPositionsPage::CPositionsPage() : CPropertyPage(CPositionsPage::IDD)
{
	//{{AFX_DATA_INIT(CPositionsPage)
	//}}AFX_DATA_INIT
}

CPositionsPage::~CPositionsPage()
{
}

void CPositionsPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPositionsPage)
	//}}AFX_DATA_MAP
	DDX_FillEdit(pDX, IDC_POSITIONS_TEXT,  &theApp.GetDocument()->m_strPositions);
}


BEGIN_MESSAGE_MAP(CPositionsPage, CPropertyPage)
	//{{AFX_MSG_MAP(CPositionsPage)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPositionsPage message handlers



/////////////////////////////////////////////////////////////////////////////
// CStatementPage property page

IMPLEMENT_DYNCREATE(CStatementPage, CPropertyPage)

CStatementPage::CStatementPage() : CPropertyPage(CStatementPage::IDD)
{
	//{{AFX_DATA_INIT(CStatementPage)
	m_strStatement = _T("");
	m_strGraphicFile = _T("");
	//}}AFX_DATA_INIT
}

CStatementPage::~CStatementPage()
{
}

void CStatementPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStatementPage)
	DDX_Control(pDX, IDC_TEXT_FILENAME, m_editGraphicFile);
	DDX_Control(pDX, IDC_TEXT_STATEMENT, m_editStmt);
	DDX_Text(pDX, IDC_TEXT_STATEMENT, m_strStatement);
	DDX_Text(pDX, IDC_TEXT_FILENAME, m_strGraphicFile);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStatementPage, CPropertyPage)
	//{{AFX_MSG_MAP(CStatementPage)
	ON_BN_CLICKED(IDC_BROWSE, OnBrowse)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStatementPage message handlers

BOOL CStatementPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	
	// Set edit box font to document default.
	LOGFONT lfText;
	CFBDDoc::GetDefaultFont(&lfText);
	m_fontText.CreateFontIndirect(&lfText);

	m_editStmt.SetFont(&m_fontText);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CStatementPage::OnBrowse() 
{
	// run a file dialog to get the image file
	CFileDialog dlg(TRUE, NULL, NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "All supported image types|*.bmp;*.gif;*.jpg;*.emf;*.wmf;*.ico|Bitmaps (*.bmp)|*.bmp|GIF Files (*.gif)|*.gif|JPEG Files (*.jpg)|*.jpg|Enhanced Metafiles (*.emf)|*.emf|Windows Metafiles (*.wmf)|*.wmf|Icons (*.ico)|*.ico||");
    if (dlg.DoModal() == IDCANCEL)
		return;

	// move chosen filename (no path) for transfer at end.
    m_editGraphicFile.SetWindowText(dlg.GetFileName());	
}
