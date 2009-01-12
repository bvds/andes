// VectorDlg.h : header file
// 
// 

/////////////////////////////////////////////////////////////////////////////
// CVectorDlg dialog
#include "DrawObjDlg.h"
#include "RichCombo.h"
#include "EQEdit.h"			// for rich edit label of angle
#include "ValueDlg.h"

class CFBDDoc;

class CVectorDlg : public CDrawObjDlg
{
// Construction
public:
	virtual BOOL DispatchEvent(EventID nEvent, LPCTSTR pszArgs);
	CVectorDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor
		
	virtual CLabelRichEdit* GetLabelCtrl();
// Dialog Data
	//{{AFX_DATA(CVectorDlg)
	enum { IDD = IDD_VECTOR_FORCE };
	CLogBtn	m_stcGroup;
	CLogBtn	m_btnNet;
	CLogBtn m_btnForce;
	CStatic	m_stcLet;
	CRichStatic	m_stcComp;
	CStatic	m_stcVecAng;
	CStatic	m_stcVecAng1;
	CRichStatic	m_stcVecAng2;
	CLogCombo	m_cboTimeList;
	CStatic	m_stcTimeList;
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CLogCombo	m_cboForceType;
	CLogCombo	m_cboBodyList;
	CLogCombo	m_cboAgentList;
	CLogEdit	m_editOrientation;
	CLabelRichEdit	m_editName;
	CSpinButtonCtrl	m_spinDirection;
	//}}AFX_DATA

	CValueDlg* m_pDlgValues;

	int m_nType;

	virtual void InitObjectDlg();
	virtual void InitVariableDlg();
	void UpdateTempVector();
	void UpdateTempVariable();
	void UpdateComponents();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVectorDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

	DECLARE_CTL_TBL()
	virtual int GetTrainerId(int ctrlId);

// Implementation
protected:
	void AdjustForceTypes();

	// Generated message map functions
	//{{AFX_MSG(CVectorDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnChangeVectorNameText();
	afx_msg void OnNetbtn();
	afx_msg void OnForcebtn();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
