/////////////////////////////////////////////////////////////////////////////
//CTemplateDlg.h
//
////////////////////////////////////////////////////////////////////////////

#include "LogEdit.h"
#include "Lgdialog.h"

/////////////////////////////////////////////////////////////////////////////
// CTemplateDlg dialog
#ifndef TEMPLATE_DIALOG_INCLUDED
#define TEMPLATE_DIALOG_INCLUDED 1


class CTemplateDlg : public CLogDialog
{
// Construction
public:
	CTemplateDlg(int id, CWnd* pParent = NULL);   // constructor takes resource id
	DECLARE_DYNAMIC(CTemplateDlg);
	
// Dialog Data
	//{{AFX_DATA(CTemplateDlg)
	//}}AFX_DATA

	int m_nPos;
	BOOL m_bExplained;
	int m_rule;
	CString m_strRule;
	BOOL m_bIsNewChoice;

protected:
	Status m_status[4];
	CLogCombo* m_boxes[4];
	CStatic* m_statics[5];

	CLogBtn	m_Cancel;
	CLogBtn	m_Back;
	CLogBtn	m_Submit;


// Operations
protected:
	void InitTemplate();
	Status CheckCombo(CLogCombo* pCbo);
	Status GetStatus(CWnd* pWnd);
	BOOL IsRightRule();
	BOOL IsAllCorrect(int nCboBoxes);
	void UpdateCombos(int nCboBoxes);
	void UpdateEXInfo(int nCboBoxes);
	void UpdateBtns(BOOL bReset);
	void MoveDlgToBtmRight();


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTemplateDlg)
	public:
	//}}AFX_VIRTUAL
protected:
	virtual void OnCancel();
	virtual void OnBack();
	virtual void OnSubmit();
	virtual void ResetTemplate();
	virtual void InsertCtrlStrs();



// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CTemplateDlg)
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	//}}AFX_MSG
	afx_msg void OnSelchangeCombo(UINT nID);
	DECLARE_MESSAGE_MAP()

};

#endif TEMPLATE_DIALOG_INCLUDED
