// VectorMoveDlg.h : header file
//
//
/////////////////////////////////////////////////////////////////////////////
// CVectorMoveDlg dialog

#include "DrawObjDlg.h"
#include "RichCombo.h"
#include "EQEdit.h"			// for rich edit label of angle

class CFBDDoc;

class CVectorMoveDlg : public CDrawObjDlg
{
// Construction
public:
	CVectorMoveDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor
	DECLARE_DYNAMIC(CVectorMoveDlg);

	virtual CLabelRichEdit* GetLabelCtrl();
	virtual int GetTrainerId(int ctrlId);

protected:
	virtual void InitObjectDlg();
	virtual void InitVariableDlg();

	void UpdateTempVector();
	void UpdateTempVariable();
	void UpdateComponents();
	BOOL TimeAgrees();

//item data for combos
	#define ID_GRAVACCEL	2
	#define ID_AVERAGE		3
	#define ID_INSTANTANEOUS	4

public:
// Dialog Data
	//{{AFX_DATA(CVectorMoveDlg)
	enum { IDD = IDD_VECTOR_MOVE };
	CStatic	m_stcEquals;
	CButton	m_stcGiven;
	CStatic	m_stcOr;
	CLogBtn	m_btnUnknown;
	CLogEdit m_editValue;
	CLogCombo	m_cboAngular;
	CStatic	m_txtDescription;
	CStatic	m_stcBody;
	CLogCombo	m_cboBodyList;
	CStatic	m_stcType;
	CLogCombo	m_cboMvmntType;
	CRichStatic	m_stcComp;
	CStatic	m_stcTimeList;
	CLogCombo	m_cboTimeList;
	CRichStatic	m_stcVecAng2;
	CLogEdit	m_editOrientation;
	CSpinButtonCtrl	m_spinDirection;
	CStatic	m_stcVecAng1;
	CStatic	m_stcVecAng;
	CLogCombo	m_cboZDir;
	CStatic	m_stcLet;
	CLabelRichEdit	m_editName;
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	//}}AFX_DATA
	CString m_strDescription;
	BOOL	m_bAddGrav;

	// test if being used for motion diagram vec:
	BOOL IsMDVector(); 

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVectorMoveDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
	DECLARE_CTL_TBL()

// Implementation
protected:
	void UpdateAngularFlag();
	void UpdateMDVecType(CString& strTimePt);

	// Generated message map functions
	//{{AFX_MSG(CVectorMoveDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnChangeVectorNameText();
	afx_msg void OnSelchangeMovementType();
	afx_msg void OnSelchangeBody();
	afx_msg void OnSelchangeAngular();
	afx_msg void OnSelchangeZdir();
	afx_msg void OnCheckUnknown();
	afx_msg void OnChangeGivenValue();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

