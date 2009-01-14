// LoginDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "LoginDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLoginDlg dialog


CLoginDlg::CLoginDlg(CFBDDoc* pDoc , CWnd* pParent /*=NULL*/)
	: CLogDialog(CLoginDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CLoginDlg)
	m_strName = _T("");
	//}}AFX_DATA_INIT
}


void CLoginDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLoginDlg)
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_LOGNAME, m_listName);
	DDX_CBString(pDX, IDC_LOGNAME, m_strName);
	//}}AFX_DATA_MAP
	DDV_LoginName(pDX, IDC_LOGNAME, m_strName);

}


BEGIN_MESSAGE_MAP(CLoginDlg, CLogDialog)
	//{{AFX_MSG_MAP(CLoginDlg)
	ON_CBN_DBLCLK(IDC_LOGNAME, OnDblclkLogname)
	ON_WM_KILLFOCUS()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLoginDlg message handlers

BOOL CLoginDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	LogEventf(EV_DLG_LOGIN, "");

	WIN32_FIND_DATA FileData;
	char szPathName[MAX_PATH];
	
	BOOL bFinished = FALSE;
	if (! g_strAndesDir.IsEmpty())
		strcpy(szPathName, g_strAndesDir);
	else
		GetCurrentDirectory(MAX_PATH, szPathName);


	// char szDirectory[] = // "\\Students\\*.old";
	// Now check student solution directory, see ProblemSet functions
	// !!! Note this only gets students who've saved solutions.
	char szDirectory[] = 	"\\solutions\\Solutions\\*";
	
	strcat(szPathName, szDirectory);
	HANDLE hSearch = FindFirstFile(szPathName, &FileData);
	if (hSearch == INVALID_HANDLE_VALUE)
		bFinished = TRUE;
	while (!bFinished && 
		   (FileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
	{
		CString name = FileData.cFileName;
		// following is from when we listed student.old files:
		// int strlen = name.GetLength();
		// name = name.Left(strlen-4);
		if (name != "." && name != "..") // ignore special directory entries
			m_listName.AddString(name);
		
		if (!FindNextFile(hSearch, &FileData))
			bFinished = TRUE;
	}
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CLoginDlg::OnDblclkLogname() 
{
	OnOK();
	
}

void AFXAPI DDV_LoginName(CDataExchange* pDX, int nIDC, CString editStr)
{
	pDX->PrepareEditCtrl(nIDC);
	char illegalChar[]="\\/:*?""<>|";
	if (editStr.IsEmpty()){
		if (pDX->m_bSaveAndValidate)
	    {
		    theApp.DoWarningMessage("Please enter a login name");
			pDX->Fail();
		    return;
	    }
	}else if  (editStr.FindOneOf(illegalChar)!=-1){
		if (pDX->m_bSaveAndValidate)
	    {
		    theApp.DoWarningMessage("Login name cannot contain { \\ / : * ? "" < > | }");
			pDX->Fail();
		    return;
	    }
	}
}

// Handle cancel command, which could come from keyboard if not button
void CLoginDlg::OnCancel() 
{
	// prompt them for confirmation
	if (AfxMessageBox("Cancel Andes session?", MB_YESNO) == IDYES)
		CLogDialog::OnCancel();
}

