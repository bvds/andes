// ViewOptsDlg.h : header file
// 
// $Id: VWOPTDLG.H,v 1.1 2005/01/24 16:28:09 bvds Exp $

/////////////////////////////////////////////////////////////////////////////
// CViewOptsDlg dialog

class CViewOptsDlg : public CDialog
{
// Construction
public:
	CViewOptsDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CViewOptsDlg)
	enum { IDD = IDD_VIEW_OPTS };
	CSpinButtonCtrl	m_spinGreyLevel;
	int		m_nMaskMode;
	int		m_nGreyLevel;
	BOOL	m_bHighlightBox;
	BOOL	m_bScaleToFit;
	//}}AFX_DATA
	COLORREF m_colorBox;


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CViewOptsDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CViewOptsDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnBoxColor();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
