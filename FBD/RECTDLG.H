// rectdlg.h : header file
// 
// $Id: RECTDLG.H,v 1.2 2005/04/11 18:53:54 anders Exp $
//  

/////////////////////////////////////////////////////////////////////////////
// CPreviewStc window -- CStatic subclass displays preview image of obj or doc

class CDrawObj;
class CFBDDoc;

class CPreviewStc : public CStatic
{
// Construction
public:
	CPreviewStc();

// Attributes
public:
	void SetObj(const CDrawObj* pObj);
	void SetDoc(const CFBDDoc* pDoc);

// Operations
public:
	
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPreviewStc)
	//}}AFX_VIRTUAL

// Implementation
public:
	void UpdateSize();
	const CDrawObj* m_pObj;
	const CFBDDoc* m_pDoc;
	// virtual ~CPreviewStc();

	// Generated message map functions
protected:
	//{{AFX_MSG(CPreviewStc)
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// CRectDlg dialog
//
class CDrawRect;
class CRectDlg : public CPropertyPage
{
// Construction
public:
	CRectDlg(CDrawRect* pObj = NULL, CWnd* pParent = NULL); 

// Dialog Data
	//{{AFX_DATA(CRectDlg)
	enum { IDD = IDD_PROP_RECT };
	CPreviewStc	m_stcPreview;
	CSpinButtonCtrl	m_SpinCtrl;
	BOOL    m_bNoFill;
	UINT    m_penSize;
	int		m_nPenStyle;
	//}}AFX_DATA

	CDrawRect* m_pObj;	// ptr back to drawing object
	void UpdateObj(CDrawRect* pObj);	// transfers values back into obj

	COLORREF m_colorFill;
	COLORREF m_colorPen;
	
// Implementation
protected:
	BOOL m_bInitialized;		// set after controls initialized in OnInitDialog
	void OnPropChange();
	void UpdatePreview();
	CDrawRect* m_pObjPreview;	// temp copy used for previewing changes
	
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual BOOL OnApply();
	virtual void OnOK();

	// Generated message map functions
	//{{AFX_MSG(CRectDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnFillColor();
	afx_msg void OnLineColor();
	afx_msg void OnNofill();
	afx_msg void OnChangeWeight();
	afx_msg void OnDestroy();
	//}}AFX_MSG
	afx_msg void OnPenStyle(UINT nId);
	DECLARE_MESSAGE_MAP()
};

// Pen style radio button group values
#define PEN_SOLID   0		// default if no button checked
#define PEN_DOTTED  1
#define PEN_DASHED  2

/////////////////////////////////////////////////////////////////////////////
