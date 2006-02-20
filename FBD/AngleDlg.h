// AngleDlg.h : header file
//
#include "DrawObjDlg.h"
#include "EQEdit.h"			// for rich edit label of angle
#include "SymbolMenu.h"		// embedded symbol menu window object
/////////////////////////////////////////////////////////////////////////////
// CAngleDlg dialog

class CAngle;

class CAngleDlg : public CDrawObjDlg
{
// Construction
public:

	virtual CLabelRichEdit* GetLabelCtrl();
	CAngleDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor
	DECLARE_CTL_TBL()

// Dialog Data
	//{{AFX_DATA(CAngleDlg)
	enum { IDD = IDD_ANGLE };
	CStatic	m_stcDirLabel;
	CLogRichCombo	m_cboSide1;
	CLogRichCombo	m_cboSide2;
	CLogBtn		m_btnOK;
	CLogBtn		m_btnCancel;
	CLabelRichEdit m_ctrlName;
	CStatic		m_ctlDegrees;
	//}}AFX_DATA

	CLogBtn	m_btnGreek;
	CSymbolMenu m_wndMenu;

// Operations
protected:
	void SetRichEditText(CString str);

	virtual void InitObjectDlg();
	virtual void InitVariableDlg();

	void UpdateTempVariable();

public:
	static int GetAngleBetween(const CString& strSide1, const CString& strSide2, 
							   BOOL* pbAxis = NULL);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAngleDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void PositionGreekMenu();

	void UpdateAngleMagDisplay();
	int  GetAngleMag(BOOL* pbAxis = NULL);

	// Generated message map functions
	//{{AFX_MSG(CAngleDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnGreekbtn();
	afx_msg void OnMove(int x, int y);
	afx_msg void OnSelchangeSide();
	afx_msg void OnCloseupSide();
	//}}AFX_MSG
	afx_msg void OnInsertGreekLetter(UINT nID);
	DECLARE_MESSAGE_MAP()
};
