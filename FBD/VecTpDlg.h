// VecTpDlg.h : header file
//
//$Id: VecTpDlg.h,v 1.1 2005/01/24 16:28:09 bvds Exp $
/////////////////////////////////////////////////////////////////////////////
// CVectorTypeDlg dialog
#include "DrawObjDlg.h"

class CVectorTypeDlg : public CDrawObjDlg
{
// Construction
public:
	
	CVectorTypeDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CVectorTypeDlg)
	enum { IDD = IDD_VECTOR_TYPE };
	CLogBtn	m_Force;
	CLogBtn m_Velocity;
	CLogBtn m_Acceleration;
	CLogBtn m_Component;
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	int		m_nVectorType;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVectorTypeDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
	afx_msg void OnClickType(UINT nId);

// Implementation
protected:



	// Generated message map functions
	//{{AFX_MSG(CVectorTypeDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//value indices for the vector type radio button group
#define VECTOR_FORCE		0
#define VECTOR_VELOCITY		1
#define VECTOR_ACCELERATION	2
#define VECTOR_COMPONENT	3
#define VECTOR_DISPLACEMENT	4