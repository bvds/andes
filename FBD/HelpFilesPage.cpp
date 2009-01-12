// HelpFilesPage.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "HelpFilesPage.h"
#include "FBDDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// filenames:
static const char szGrphName[] = "grph.txt";
static const char szHashName[] = "hash.fsl";
static const char szSvarsName[] = "svars.txt";
// registry keys
static const char szAuthorSection[] = "Author";
static const char szSourcePdataDirKey[] = "Source Pdata Directory";

/////////////////////////////////////////////////////////////////////////////
// CHelpFilesPage property page

IMPLEMENT_DYNCREATE(CHelpFilesPage, CPropertyPage)

CHelpFilesPage::CHelpFilesPage() : CPropertyPage(CHelpFilesPage::IDD)
{
	//{{AFX_DATA_INIT(CHelpFilesPage)
	//}}AFX_DATA_INIT
}

CHelpFilesPage::~CHelpFilesPage()
{
}

void CHelpFilesPage::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHelpFilesPage)
	DDX_Control(pDX, IDC_PDATA_DIR, m_editPdataDir);
	DDX_Control(pDX, IDC_SVARS_SRC_INFO, m_stcSvarsSrcInfo);
	DDX_Control(pDX, IDC_HASH_SRC_INFO, m_stcHashSrcInfo);
	DDX_Control(pDX, IDC_GRPH_SRC_INFO, m_stcGrphSrcInfo);
	DDX_Control(pDX, IDC_SVARS_INFO, m_stcSvarsInfo);
	DDX_Control(pDX, IDC_HASH_INFO, m_stcHashInfo);
	DDX_Control(pDX, IDC_GRPH_INFO, m_stcGrphInfo);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CHelpFilesPage, CPropertyPage)
	//{{AFX_MSG_MAP(CHelpFilesPage)
	ON_BN_CLICKED(ID_UPDATE_GRPH, OnUpdateGrph)
	ON_BN_CLICKED(ID_UPDATE_HASH, OnUpdateHash)
	ON_BN_CLICKED(ID_UPDATE_SVARS, OnUpdateSvars)
	ON_BN_CLICKED(ID_UPDATE_ALL, OnUpdateAll)
	ON_EN_KILLFOCUS(IDC_PDATA_DIR, OnKillfocusPdataDir)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHelpFilesPage message handlers
BOOL CHelpFilesPage::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();

	// Init Source Pdata dir from registry, defaulting to current dir 
	// we init ctl here rather than via DDX to localize info to this module
	char szCurrentDir[MAX_PATH] = "";
	::GetCurrentDirectory(MAX_PATH, szCurrentDir);
	m_strPdataDir = theApp.GetProfileString(szAuthorSection, 
		                              szSourcePdataDirKey, szCurrentDir);
	m_editPdataDir.SetWindowText(m_strPdataDir);

	// show included file info
	m_stcGrphInfo.SetWindowText(GetInfoStr(szGrphName));
	m_stcHashInfo.SetWindowText(GetInfoStr(szHashName));
	m_stcSvarsInfo.SetWindowText(GetInfoStr(szSvarsName));

	// show source file info
	UpdateSrcInfo();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// Get info string for included file
static const char szTimeFmt[] = "%m/%d/%y %H:%m"; // concise format to fit

CString CHelpFilesPage::GetInfoStr(LPCSTR pszFileName)
{
	CFBDDoc* pDoc = (CFBDDoc*) theApp.GetDocument();
	CHelpData* pItem = pDoc->GetHelpData(pszFileName);
	if (pItem == NULL) 
		return "<none>";

	// info string has modified time in concise format plus file size
	CString strInfo;
	CString strTime = pItem->m_timeModified.Format(szTimeFmt);
	strInfo.Format("%s (%d)", strTime, pItem->m_cbLength);

	return strInfo;
}

// get full source path = PdataDir/Problem-ID/FileName
CString CHelpFilesPage::GetSrcPath(LPCSTR pszFileName)
{
	CFBDDoc* pDoc = (CFBDDoc*) theApp.GetDocument();
	CString strProblemId = pDoc->m_strProblemId;
	// If new problem, author might not have entered problem id yet.
	if (strProblemId.IsEmpty())
		strProblemId = pDoc->GetTitle();

	return m_strPdataDir + "/" + strProblemId + "/" + pszFileName; 
}

// Get info string for src file on disk
CString CHelpFilesPage::GetSrcInfoStr(LPCSTR pszFileName)
{
	CString strPathName = GetSrcPath(pszFileName);
	CFileStatus statSrc;
	if (CFile::GetStatus(strPathName, statSrc)) {
		CString strInfo;
		CString strTime = statSrc.m_mtime.Format(szTimeFmt);
		strInfo.Format("%s (%d)", strTime, statSrc.m_size);

		return strInfo;
	}
	// else not found:
	return "?";
}

// Update src file info display.
void CHelpFilesPage::UpdateSrcInfo()
{
	m_stcGrphSrcInfo.SetWindowText(GetSrcInfoStr(szGrphName));
	m_stcHashSrcInfo.SetWindowText(GetSrcInfoStr(szHashName));
	m_stcSvarsSrcInfo.SetWindowText(GetSrcInfoStr(szSvarsName));
}


// Commands to update included files from src:
void CHelpFilesPage::OnUpdateGrph() 
{
	UpdateFile(szGrphName);
	m_stcGrphInfo.SetWindowText(GetInfoStr(szGrphName));
}

void CHelpFilesPage::OnUpdateHash() 
{
	UpdateFile(szHashName);
	m_stcHashInfo.SetWindowText(GetInfoStr(szHashName));
}

void CHelpFilesPage::OnUpdateSvars() 
{
	UpdateFile(szSvarsName);
	m_stcSvarsInfo.SetWindowText(GetInfoStr(szSvarsName));
}

void CHelpFilesPage::OnUpdateAll() 
{
	OnUpdateGrph();
	OnUpdateHash();
	OnUpdateSvars();
}

// Worker routine updates one included file from current source dir and problem ID.
void CHelpFilesPage::UpdateFile(LPCSTR pszFileName)
{
	// Ensure we have a source directory
	if (m_strPdataDir.IsEmpty())
		AfxMessageBox("Must set Pdata directory");

	// find or create doc item for this file
	CFBDDoc* pDoc = (CFBDDoc*) theApp.GetDocument();
	CHelpData* pItem = pDoc->GetHelpData(pszFileName);
	if (pItem == NULL) { // doesn't exist: add it
		pItem = new CHelpData();
		pItem->m_strFileName = pszFileName;
		pDoc->AddItem(pItem);
	}

	// and update the item
	pItem->SetData(GetSrcPath(pszFileName));
}

#if 0 // Now obsolete: cmd to add or update item prompting for full path to srcfile
void CHelpFilesPage::OnAddHelpFile() 
{
	CFileDialog dlg(TRUE);
	if (dlg.DoModal() != IDOK)
		return;

	CString strFileName = dlg.GetFileName();
	CString strPathName = dlg.GetPathName();

	CFBDDoc* pDoc = (CFBDDoc*) theApp.GetDocument();
	CHelpData* pItem = pDoc->GetHelpData(strFileName);
	if (pItem == NULL) { // doesn't exist: add it
		pItem = new CHelpData();
		pItem->m_strFileName = strFileName;
		pDoc->AddItem(pItem);
	}

	pItem->SetData(strPathName);
}
#endif 0

// update after possible change to PdataDir
void CHelpFilesPage::OnKillfocusPdataDir() 
{
	// See if it changed, updating member
	CString strPrev = m_strPdataDir;
	m_editPdataDir.GetWindowText(m_strPdataDir);
	if (m_strPdataDir == strPrev)
		return;
	// else changed:
	UpdateSrcInfo();
	// Update saved value in registry. Note means registry contains last contents on 
	// every change, even if dir is bad.
	theApp.WriteProfileString(szAuthorSection, szSourcePdataDirKey, m_strPdataDir);
}
