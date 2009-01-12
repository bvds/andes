#if !defined(AFX_HELPFILESPAGE_H__22E2F6E5_807E_11D2_807C_88A95E8A3415__INCLUDED_)
#define AFX_HELPFILESPAGE_H__22E2F6E5_807E_11D2_807C_88A95E8A3415__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// HelpFilesPage.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CHelpFilesPage dialog

class CHelpFilesPage : public CPropertyPage
{
	DECLARE_DYNCREATE(CHelpFilesPage)

// Construction
public:
	CHelpFilesPage();
	~CHelpFilesPage();

// Dialog Data
	//{{AFX_DATA(CHelpFilesPage)
	enum { IDD = IDD_HELPFILES_PAGE };
	CEdit	m_editPdataDir;
	CStatic	m_stcSvarsSrcInfo;
	CStatic	m_stcHashSrcInfo;
	CStatic	m_stcGrphSrcInfo;
	CStatic	m_stcSvarsInfo;
	CStatic	m_stcHashInfo;
	CStatic	m_stcGrphInfo;
	//}}AFX_DATA
	CString m_strPdataDir;

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CHelpFilesPage)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CHelpFilesPage)
	afx_msg void OnUpdateGrph();
	afx_msg void OnUpdateHash();
	afx_msg void OnUpdateSvars();
	afx_msg void OnUpdateAll();
	virtual BOOL OnInitDialog();
	afx_msg void OnKillfocusPdataDir();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	CString GetInfoStr(LPCSTR pszFileName);
	CString GetSrcPath(LPCSTR pszFileName);
	CString GetSrcInfoStr(LPCSTR pszFileName);
	void UpdateSrcInfo();

	void UpdateFile(LPCSTR pszFileName);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_HELPFILESPAGE_H__22E2F6E5_807E_11D2_807C_88A95E8A3415__INCLUDED_)
