#if !defined(AFX_DIRECTORYDLG_H__5201E77B_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_DIRECTORYDLG_H__5201E77B_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// DirectoryDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CDirectoryDlg dialog

class CDirectoryDlg : public CDialog
{
// Construction
public:
	BOOL InitSelection(BOOL bCheck);
	BOOL AddDriveNode(CString& strDrive);
	int InitDirTree();
	HTREEITEM ExpandNode(CString strDir, HTREEITEM hItem);
	void DeleteAllChildren (HTREEITEM hParent);
	void DeleteFirstChild (HTREEITEM hParent);
	int AddDirectories (HTREEITEM hItem, CString& strPath);
	CString GetPathFromNode (HTREEITEM hItem);
	BOOL SetButtonState (HTREEITEM hItem, CString& strPath);
	CInstDirPg* m_pParent;





	CDirectoryDlg(CWnd* pParent = NULL);   // standard constructor
	CImageList m_imgDrives;
// Dialog Data
	//{{AFX_DATA(CDirectoryDlg)
	enum { IDD = IDD_DIRECTORY_DLG };
	CEdit	m_ctrlPath;
	CTreeCtrl	m_ctrlDirTree;
	CString	m_strPath;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDirectoryDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CDirectoryDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnItemexpandingDirectoryTree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSelchangedDirectoryTree(NMHDR* pNMHDR, LRESULT* pResult);
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DIRECTORYDLG_H__5201E77B_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
