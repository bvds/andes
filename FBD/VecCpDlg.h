// VecCpDlg.h : header file
//
//$Id: VecCpDlg.h,v 1.2 2005/04/11 18:53:54 anders Exp $
/////////////////////////////////////////////////////////////////////////////
// CVectorCompDlg dialog

#include "DrawObjDlg.h"

class CFBDDoc;

class CVectorCompDlg : public CDrawObjDlg
{
// Construction
public:
	CVectorCompDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	DECLARE_CTL_TBL()


// Dialog Data
	//{{AFX_DATA(CVectorCompDlg)
	enum { IDD = IDD_VECTOR_COMP };
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CLogEdit	m_editName;
	CLogCombo	m_cboCompOf;
	CLogCombo	m_cboCompDirList;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVectorCompDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual void InitObjectDlg();
	void UpdateTempComp();
	
	// Generated message map functions
	//{{AFX_MSG(CVectorCompDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
