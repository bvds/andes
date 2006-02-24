#if !defined(AFX_VIDEODLG_H__E931CB54_FCEA_4A6D_89CC_C77DE7BD1C4F__INCLUDED_)
#define AFX_VIDEODLG_H__E931CB54_FCEA_4A6D_89CC_C77DE7BD1C4F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// VideoDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CVideoDlg dialog

class CVideoDlg : public CDialog
{
// Construction
public:
	CVideoDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CVideoDlg)
	enum { IDD = IDD_VIDEO };
	int		m_nChoice;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVideoDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CVideoDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VIDEODLG_H__E931CB54_FCEA_4A6D_89CC_C77DE7BD1C4F__INCLUDED_)
