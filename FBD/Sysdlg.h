// SysDlg.h : header file
// 
// $Id: Sysdlg.h,v 1.2 2005/04/11 18:53:54 anders Exp $

/////////////////////////////////////////////////////////////////////////////
// CSystemDlg dialog

#include "DrawObjDlg.h"
#include "EQEdit.h"			// for rich edit label of angle

class CSystem;
class CSystemDlg : public CDrawObjDlg
{
// Construction
public:
	CSystemDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	BOOL m_bInPlan;//we are in the plan, No label needed

	virtual CLabelRichEdit* GetLabelCtrl();

	DECLARE_CTL_TBL()

// Dialog Data
	//{{AFX_DATA(CSystemDlg)
	enum { IDD = IDD_SYSTEM };
	CStatic		m_stcLabel;
	CStatic		m_stcTimeList;
	CLogList	m_listBodies;
	CLogBtn		m_Ok;
	CLogBtn		m_Cancel;
	CLogCombo	m_cboTimeList;
	CLabelRichEdit	m_editName;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSystemDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Operations
protected:
	virtual int	GetTrainerId(int ctrlId);
	virtual void InitObjectDlg();
	
	void	UpdateTempSystem();
	void	SelectBodies(CString& strBodies);


// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CSystemDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};


