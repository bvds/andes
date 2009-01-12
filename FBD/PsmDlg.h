#if !defined(AFX_PSMDLG_H__E2C54383_7187_11D5_8735_00036D1733F4__INCLUDED_)
#define AFX_PSMDLG_H__E2C54383_7187_11D5_8735_00036D1733F4__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PsmDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPsmDlg dialog

class CPsmDlg : public CLogDialog
{
// Construction
public:
	
	void UpdateHelpButton();
	BOOL m_bPrinciples;		// true => show major principles only
	BOOL m_bSelect;			// false => browsing only, not being used to select
	CString RemoveDollars(LPCTSTR szText);
	CString GetItemLesson(HTREEITEM hItem);
	int m_nSelected;
	CString m_strPSM;
	CString m_strHelpID;
	void PopulateTree();
	
	CPsmDlg(CWnd* pParent = NULL);   // standard constructor

	// Custom window creation func for modeless dialogs of this class.
	BOOL Create(CWnd* pParent = NULL) {
		return CDialog::Create(CPsmDlg::IDD, pParent);
	}

	static void LoadPsmInfo(LPCSTR pszPathName);
	static void AddInfo(CString strType, CString strText, CString strHelpID, CString strName, int nLine);
	static void InitPsmInfo();
	/* void DumpTable();*/

// Dialog Data
	//{{AFX_DATA(CPsmDlg)
	enum { IDD = IDD_PSM };
	CStatic	m_stcPrompt;
	CLogBtn	m_btnHelp;
	CLogBtn	m_btnOk;
	CLogBtn	m_btCancel;
	CTreeCtrl	m_tree;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPsmDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual BOOL OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult);
	//}}AFX_VIRTUAL

	virtual BOOL DispatchEvent(EventID id, LPCTSTR parms);	// for log playback

// Implementation
protected:
	void AddSubtree(int& i, HTREEITEM hParent = TVI_ROOT);
	HTREEITEM FindItemData(DWORD dwData, HTREEITEM hItem = TVI_ROOT);
	
	// Generated message map functions
	//{{AFX_MSG(CPsmDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnPsmHelp();
	afx_msg void OnSelchangedPsmTree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnDblclkPsmTree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnItemexpandedPsmTree(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	afx_msg UINT	OnNcHitTest(CPoint point);
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PSMDLG_H__E2C54383_7187_11D5_8735_00036D1733F4__INCLUDED_)
