#if !defined(AFX_SETTINGPG_H__5201E778_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_SETTINGPG_H__5201E778_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// SettingPg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CSettingPg dialog

class CSettingPg : public CPropertyPage
{
	DECLARE_DYNCREATE(CSettingPg)

// Construction
public:
	CString m_strInstDir;
	CSettingPg();
	~CSettingPg();

// Dialog Data
	//{{AFX_DATA(CSettingPg)
	enum { IDD = IDD_STARTCOPY_DLG };
	CStatic	m_ctrlSettings;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CSettingPg)
	public:
	virtual BOOL OnSetActive();
	virtual LRESULT OnWizardNext();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CSettingPg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SETTINGPG_H__5201E778_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
