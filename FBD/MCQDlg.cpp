// MCQDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "MCQDlg.h"
#include "ChoiceDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMCQDlg dialog


CMCQDlg::CMCQDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CMCQDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CMCQDlg)
	m_strQuestion = _T("");
	m_nCorrect = 0;
	m_strId = _T("");
	//}}AFX_DATA_INIT
}


void CMCQDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CMCQDlg)
	DDX_Control(pDX, IDC_CHOICE_LIST, m_listChoices);
	DDX_Control(pDX, IDC_SPIN1, m_spinCorrect);
	DDX_Text(pDX, IDC_CHOICE_QUESTION, m_strQuestion);
	DDX_Text(pDX, IDC_EDIT_CORRECT, m_nCorrect);
	DDX_Text(pDX, IDC_MCQ_ID, m_strId);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CMCQDlg, CDialog)
	//{{AFX_MSG_MAP(CMCQDlg)
	ON_BN_CLICKED(IDC_CHOICE_ADD, OnChoiceAdd)
	ON_BN_CLICKED(IDC_CHOICE_EDIT, OnChoiceEdit)
	ON_BN_CLICKED(IDC_CHOICE_REMOVE, OnChoiceRemove)
	ON_BN_CLICKED(IDC_CHOICE_DOWN, OnChoiceDown)
	ON_BN_CLICKED(IDC_CHOICE_UP, OnChoiceUp)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMCQDlg message handlers

BOOL CMCQDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// fill list box from m_strsChoices StringList
	POSITION pos = m_strsChoices.GetHeadPosition();
	while (pos != NULL) {
		CString& strChoice = m_strsChoices.GetNext(pos);
		m_listChoices.AddString(strChoice);
	}

	// set spinner range to number of items
	m_spinCorrect.SetRange(1, m_listChoices.GetCount());
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CMCQDlg::OnChoiceAdd() // add a new choice string
{
	// Run a subdialog to add a new choice to the list box
	CChoiceDlg dlgChoice;
	if (dlgChoice.DoModal() != IDOK)
		return;

	// Update list box with new choice
	int nNew = m_listChoices.AddString(dlgChoice.m_strText);

	// add new string to our member var
	m_strsChoices.AddTail(dlgChoice.m_strText);
	
	// update correct answer spinner range
	m_spinCorrect.SetRange(1, nNew);

	// update correct answer field if new choice was flagged as correct in dialog
	// (if not, user can still set from the spinner field).
	if (dlgChoice.m_bCorrect) 
	{
		m_nCorrect = nNew;
		UpdateData(FALSE); // transfers member value to edit control.
	}
}

void CMCQDlg::OnChoiceEdit()	// Edit currently selected choice string
{
	int nSel = m_listChoices.GetCurSel();
	BOOL bWasCorrect = ((nSel + 1) == (int) m_nCorrect);

	// Run a subdialog to edit choice string
	CChoiceDlg dlgChoice;
	m_listChoices.GetText(nSel, dlgChoice.m_strText);
	dlgChoice.m_bCorrect = bWasCorrect;

	if (dlgChoice.DoModal() != IDOK)
		return;

	// update text of this item
	m_listChoices.DeleteString(nSel);
	m_listChoices.InsertString(nSel, dlgChoice.m_strText);

	// update correct answer
	if (dlgChoice.m_bCorrect) {
		// get number of items in list box
		// set to edit control
		// update member var?
	}
	// !if was correct but was flagged incorrect, we now have no correct answer.
}

void CMCQDlg::OnChoiceRemove() // Delete currently selected choice
{
	int nSel = m_listChoices.GetCurSel();
	m_listChoices.DeleteString(nSel);
}


void CMCQDlg::OnChoiceDown() // move choice down in list
{
	int nSel = m_listChoices.GetCurSel();
	if (nSel != LB_ERR && nSel < m_listChoices.GetCount() - 1) 
	{
		CString strSelect;
		m_listChoices.GetText(nSel, strSelect);
		m_listChoices.DeleteString(nSel);
		m_listChoices.InsertString(nSel + 1, strSelect);
	}
	
}

void CMCQDlg::OnChoiceUp()	// move choice up in list
{
	int nSel = m_listChoices.GetCurSel();
	if (nSel != LB_ERR && nSel > 1) 
	{
		CString strSelect;
		m_listChoices.GetText(nSel, strSelect);
		m_listChoices.DeleteString(nSel);
		m_listChoices.InsertString(nSel - 1, strSelect);
	}
}
