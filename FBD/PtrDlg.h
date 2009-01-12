#if !defined(AFX_PTRDLG_H__1C900442_9E8E_11D1_BC04_0000C037C67D__INCLUDED_)
#define AFX_PTRDLG_H__1C900442_9E8E_11D1_BC04_0000C037C67D__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PtrDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMsgBalloon dialog. -- shows message in a floating roundrect-shaped window
// 
// Currently colored like Windows help windows (cream).
//  
class CMsgBalloon : public CDialog
{
// Construction
public:
	CMsgBalloon(CWnd* pParent = NULL);   // standard constructor
	// custom Windows window creator for modeless dialogs of this type
	BOOL Create(CWnd* pParent = NULL) 
		{ return CDialog::Create(IDD, pParent); };

// Dialog Data
	//{{AFX_DATA(CMsgBalloon)
	enum { IDD = IDD_PTR_MSG };
	CEdit	m_editMsg;
	//}}AFX_DATA

// operations:
	void SetMsg(LPCTSTR pszMsg);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMsgBalloon)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CFont m_fontMsg;
	CBrush m_bkBrush;

	// Generated message map functions
	//{{AFX_MSG(CMsgBalloon)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	virtual BOOL OnInitDialog();
	afx_msg void OnPaint();
	afx_msg void OnDestroy();
	afx_msg void OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
// CPtrWnd dialog

class CPtrWnd : public CWnd /*, public IPlayerUI */
{
// Construction
public:
	CPtrWnd();			// standard constructor
	BOOL Create();		// custom Window creation function
	
// Attributes
	enum PtrDir {UpLeft, UpRight};   // directions in which we may point
	void SetDir(PtrDir dir);

// Operations
	void MoveTo(int xNew, int yNew);
	void ShowMsg(LPCTSTR pszMsg);
		
// overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPtrWnd)
	protected:
	//}}AFX_VIRTUAL

// Implementation
protected:
	CBrush m_bkBrush;						// brush for painting background.						
	
	CMsgBalloon m_wndMsg;					// msg balloon 
	void PlaceMsg();

	// for defining arrow region as PolyPolygon:
	static int PolySizes[2]; /*={5, 4}*/	// 5-pt arrowhead then shaft rectangle
	enum { ARROWPTS = 9};
	CPoint m_pts[ARROWPTS];					// points for current region
	
	PtrDir m_dir;							// current ptr direction.
	void FlipLeftRight();					// change direction of arrow
	
	// Generated message map functions
	//{{AFX_MSG(CPtrWnd)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnMove(int x, int y);
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PTRDLG_H__1C900442_9E8E_11D1_BC04_0000C037C67D__INCLUDED_)
