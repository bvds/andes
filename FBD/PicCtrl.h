#if !defined(AFX_PICCTRL_H__659D30E1_C7BA_11D1_A6D7_0000C0086DCF__INCLUDED_)
#define AFX_PICCTRL_H__659D30E1_C7BA_11D1_A6D7_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PicCtrl.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPicCtrl dialog

class CFBDDoc;	// forward ref

class CPicCtrl : public CFileDialog
{
// Construction
public:
	
	
	CPicCtrl(BOOL bOpenFileDialog, 
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		CWnd* pParentWnd = NULL);	   // standard constructor

// Dialog Data
	//{{AFX_DATA(CPicCtrl)
	enum { IDD = IDD_FILEOPEN_PIC };
	CStatic	m_ctrlPreview;
	//}}AFX_DATA


	

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPicCtrl)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL


// Implementation
protected:
	CString m_strOldName;
	void CreatePreview(CFBDDoc* pTempDoc);

	// Generated message map functions
	//{{AFX_MSG(CPicCtrl)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	virtual void OnFileNameChange();
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PICCTRL_H__659D30E1_C7BA_11D1_A6D7_0000C0086DCF__INCLUDED_)
