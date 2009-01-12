// AxesDlg.h : header file
// 
// $Id: AxesDlg.h,v 1.2 2005/04/11 18:53:54 anders Exp $

/////////////////////////////////////////////////////////////////////////////
// CAxesDlg dialog
#include "DrawObjDlg.h"

class CFBDDoc;
class CAxesDlg : public CDrawObjDlg
{
// Construction
public:
	
	CAxesDlg(CDrawObj* m_pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	DECLARE_CTL_TBL()

// Dialog Data
	//{{AFX_DATA(CAxesDlg)
	enum { IDD = IDD_AXES };
	CLogBtn	m_btnOK;
	CLogBtn	m_btnCancel;
	CLogEdit	m_editDirection;
	CComboBox	m_cboSystemList;
	CSpinButtonCtrl	m_spinDirection;
	CString	m_strXLabel;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAxesDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual void InitObjectDlg();
	void UpdateTempAxes();

	// Generated message map functions
	//{{AFX_MSG(CAxesDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

// values for m_nAxisButton = which axis, x or y, is being oriented
#define AXIS_X 0
#define AXIS_Y 0
