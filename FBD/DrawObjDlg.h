/////////////////////////////////////////////////////////////////////////////
//CDrawObjDlg.h
//
////////////////////////////////////////////////////////////////////////////
#ifndef DRAWOBJ_DIALOG_INCLUDED
#define DRAWOBJ_DIALOG_INCLUDED 1

#include "LogEdit.h"
#include "Lgdialog.h"

#include "EQEdit.h"


class CDrawObjDlg : public CCheckedDlg
{

// Construction
protected:
	CDrawObj* m_pObj;//pointer to CDrawObj
	CDrawObj* m_pTempObj;

	void UpdateObj();

	void InitDlg();
	virtual void InitObjectDlg() {};//purely virtual
	virtual void InitVariableDlg() {};//purely virtual
	virtual CLabelRichEdit* GetLabelCtrl() 
	{	
		CWnd* pCtl = GetDlgItem(IDC_CUSTOM_LABEL);// fetch by default control id.
		if ((pCtl != NULL) && pCtl->IsKindOf(RUNTIME_CLASS(CLabelRichEdit)))
			return (CLabelRichEdit*) pCtl;
		return NULL;
	};	
	
	void InitLabel();

	// to support saving snapshot files with comments describing dialog state
public:
	virtual void SaveSnapshot(const CString& strSnapPath);
	virtual CString GetDlgInfo();
	
public:
	CDrawObjDlg(int id, CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // constructor takes resource id
	DECLARE_DYNAMIC(CDrawObjDlg);
	
// Dialog Data
	//{{AFX_DATA(CDrawObjDlg)
	//}}AFX_DATA

public:
	// Flags for special uses of the definition dialogs when simply defining quantities
	// in certain contexts:
	BOOL m_bSought;				// Defining sought quantity for example study plan
	BOOL m_bProp;				// Defining system property for hilevel plan view
	CStringList m_strPlanBodies;// list of bodies in hi-level plan stage's system. 
	                            // Set coming in by plan stage table editor 

	// Following switch seems always to be set to T in current Andes- AW
	BOOL m_bMagnitude;		// Vector dialogs: defining magnitude. FALSE => direction 

	BOOL m_bNoCheck;			// T => suppress checking of definitions on OK. 
	
	void SetComboReadOnly(CComboBox* pBox);	// helper to make good-looking r/o combo box

protected:
	CFBDDoc* m_pDocument;
// For when checking object from within dialog. Initially don't want to allow 
// students to close an incorrectly filled out dialog.  Close allowed if student persists.
// Need to know if updated object between close attempts, so save the previous object	
	CCheckedObj* m_pPrevObj;

							
// Operations
protected:
	//
	// control helper functions
	//
	void EnableComboBox(CComboBox * pBox, BOOL bEnable);
	void EnableListBox(CListBox* pBox, BOOL bEnable);
	BOOL IsEmpty(CComboBox* m_cboBox);
	void UpdatePlanStrings(CLogCombo* m_pCboBox);
	void RemoveTimePeriods(CComboBox* m_pCboBox);

	CString GetCurString(CComboBox* m_pCboBox);
	CString GetBodiesFromLabel(CString label);

	void MoveDlgToBtmRight();
//	void CenterTwoButtons(CLogDialog* pDlg, CButton* pBtn1, CButton* pBtn2);

	
	void DroplistToDropdown(CComboBox* pBox);
	
	//
	// helper function for dialog data validation
	//
	BOOL IsValidLabel(CString editStr);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDrawObjDlg)
	protected:
	virtual void PostNcDestroy();
	virtual void DoDataExchange(CDataExchange* pDX);
	//}}AFX_VIRTUAL

protected:
	//Removes the space taken up by the named static box
	void Remove(int nIDBox);
	//Called by Remove, updates the controls following the static box
	void UpdateControls(CWnd* pCtrlBegin, int xDist);

// Implementation
protected:
	BOOL CheckDialog();

	// Generated message map functions
	//{{AFX_MSG(CDrawObjDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnDialogWhatswrong();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#define ID_USER_DEFINED	1 //user defined times in combo have item data == 1

void AFXAPI DDX_AddCompoundBodies(CDataExchange* pDX, int nIDC, CDrawObjList* pObjList);
void AFXAPI DDX_AddUserTimes(CDataExchange* pDX, int nIDC, CVarList* pObjList);
void AFXAPI DDX_AddEquivComponents(CDataExchange* pDX, int nIDC, CVarList * pObjList);

#endif DRAWOBJ_DIALOG_INCLUDED
