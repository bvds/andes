// ValueDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ValueDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CValueDlg dialog


CValueDlg::CValueDlg(CVector* pObj, CWnd* pParent /*=NULL*/)
	: CLogDialog(CValueDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CValueDlg)
	m_bCompoForm = 0;
	//}}AFX_DATA_INIT
	m_pVec = pObj;
}


void CValueDlg::DoDataExchange(CDataExchange* pDX)
{
	CLogDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CValueDlg)
	DDX_Control(pDX, IDC_ANGLE, m_stcAngIcon);
	DDX_Control(pDX, IDC_DIRECTION_SPIN, m_spinDirection);
	DDX_Control(pDX, IDC_DIRVAR, m_stcDirLabel);
	DDX_Control(pDX, IDC_MAGVAR, m_stcMagLabel);
	DDX_Control(pDX, IDC_ZCVAR, m_stcZCLabel);
	DDX_Control(pDX, IDC_YCVAR, m_stcYCLabel);
	DDX_Control(pDX, IDC_XCVAR, m_stcXCLabel);
	DDX_Control(pDX, IDC_ZDIR, m_cboZDir);
	DDX_Control(pDX, IDC_ORIENTATION_TEXT, m_editDirValue);
	DDX_Control(pDX, IDC_COMPO_BTN, m_btnCompo);
	DDX_Control(pDX, IDC_MAGDIR_BTN, m_btnMagDir);
	DDX_Control(pDX, IDC_ZC_VALUE, m_editZCValue);
	DDX_Control(pDX, IDC_YC_VALUE, m_editYCValue);
	DDX_Control(pDX, IDC_XC_VALUE, m_editXCValue);
	DDX_Control(pDX, IDC_MAG_VALUE, m_editMagValue);
	DDX_Control(pDX, IDC_CHECK_ZC_UNKNOWN, m_btnZCUnknown);
	DDX_Control(pDX, IDC_CHECK_YC_UNKNOWN, m_btnYCUnknown);
	DDX_Control(pDX, IDC_CHECK_XC_UNKNOWN, m_btnXCUnknown);
	DDX_Control(pDX, IDC_CHECK_MAG_UNKNOWN, m_btnMagUnknown);
	DDX_Control(pDX, IDC_CHECK_DIR_UNKNOWN, m_btnDirUnknown);
	DDX_Radio(pDX, IDC_MAGDIR_BTN, m_bCompoForm);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CValueDlg, CLogDialog)
	//{{AFX_MSG_MAP(CValueDlg)
	ON_BN_CLICKED(IDC_COMPO_BTN, OnCompoBtn)
	ON_BN_CLICKED(IDC_MAGDIR_BTN, OnMagdirBtn)
	ON_BN_CLICKED(IDC_CHECK_MAG_UNKNOWN, OnCheckMagUnknown)
	ON_BN_CLICKED(IDC_CHECK_DIR_UNKNOWN, OnCheckDirUnknown)
	ON_BN_CLICKED(IDC_CHECK_XC_UNKNOWN, OnCheckXCUnknown)
	ON_BN_CLICKED(IDC_CHECK_YC_UNKNOWN, OnCheckYCUnknown)
	ON_BN_CLICKED(IDC_CHECK_ZC_UNKNOWN, OnCheckZCUnknown)
	ON_EN_CHANGE(IDC_MAG_VALUE, OnChangeMagValue)
	ON_EN_CHANGE(IDC_ORIENTATION_TEXT, OnChangeDirValue)
	ON_EN_CHANGE(IDC_XC_VALUE, OnChangeXCValue)
	ON_EN_CHANGE(IDC_YC_VALUE, OnChangeYCValue)
	ON_EN_CHANGE(IDC_ZC_VALUE, OnChangeZCValue)
	ON_CBN_SELCHANGE(IDC_ZDIR, OnSelchangeZdir)
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CValueDlg message handlers

BOOL CValueDlg::OnInitDialog() 
{
	// Set our compo form member so that value will be transferred into radio
	// group in DoDataExchange called on initializing dialog.
	m_bCompoForm = m_pVec->m_bCompoForm;

	CLogDialog::OnInitDialog();

	// enable change notifications from the richedits
	m_editMagValue.SetEventMask(ENM_CHANGE);
	m_editXCValue.SetEventMask(ENM_CHANGE);
	m_editYCValue.SetEventMask(ENM_CHANGE);
	m_editZCValue.SetEventMask(ENM_CHANGE);
	// set the context menuse for the richedits
	m_editMagValue.m_nMenuId = IDR_POPUP_DIALOG;
	m_editXCValue.m_nMenuId = IDR_POPUP_DIALOG;
	m_editYCValue.m_nMenuId = IDR_POPUP_DIALOG;
	m_editZCValue.m_nMenuId = IDR_POPUP_DIALOG;

	// First time through must update attribute labels based on prefix used in label 
	// control, not object's name which is initially empty.
	OnUpdateName(m_pVec->m_strName.IsEmpty() ? m_pVec->GetLabelPrefix() : m_pVec->m_strName);
	
	// adjust for value type:
	m_bCompoForm ? OnCompoBtn() : OnMagdirBtn() ;

	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


// These set the major type of our value
void CValueDlg::OnMagdirBtn() 
{
	// hide all component form controls
	GetDlgItem(IDC_STATIC_XC)->ShowWindow(SW_HIDE);
	m_stcXCLabel.ShowWindow(SW_HIDE);
	m_editXCValue.ShowWindow(SW_HIDE);
	GetDlgItem(IDC_STATIC_XC_OR)->ShowWindow(SW_HIDE);
	m_btnXCUnknown.ShowWindow(SW_HIDE);
	
	GetDlgItem(IDC_STATIC_YC)->ShowWindow(SW_HIDE);
	m_stcYCLabel.ShowWindow(SW_HIDE);
	m_editYCValue.ShowWindow(SW_HIDE);
	GetDlgItem(IDC_STATIC_YC_OR)->ShowWindow(SW_HIDE);
	m_btnYCUnknown.ShowWindow(SW_HIDE);

	GetDlgItem(IDC_STATIC_ZC)->ShowWindow(SW_HIDE);
	m_stcZCLabel.ShowWindow(SW_HIDE);
	m_editZCValue.ShowWindow(SW_HIDE);
	GetDlgItem(IDC_STATIC_ZC_OR)->ShowWindow(SW_HIDE);
	m_btnZCUnknown.ShowWindow(SW_HIDE);

	GetDlgItem(IDC_STATIC_AXES_MSG)->ShowWindow(SW_HIDE);

	// show all mag/dir controls
	GetDlgItem(IDC_STATIC_MAG)->ShowWindow(SW_SHOW);
	m_stcMagLabel.ShowWindow(SW_SHOW);
	m_editMagValue.ShowWindow(SW_SHOW);
	GetDlgItem(IDC_STATIC_MAG_OR)->ShowWindow(SW_SHOW);
	m_btnMagUnknown.ShowWindow(SW_SHOW);

	GetDlgItem(IDC_STATIC_DIR)->ShowWindow(SW_SHOW);
	m_stcDirLabel.ShowWindow(SW_SHOW);
	m_editDirValue.ShowWindow(SW_SHOW);
	GetDlgItem(IDC_DIRECTION_SPIN)->ShowWindow(SW_SHOW);
	GetDlgItem(IDC_STATIC_DEG)->ShowWindow(SW_SHOW);
	GetDlgItem(IDC_STATIC_DIR_OR)->ShowWindow(SW_SHOW);
	// show ccw icon OR z-axis choice but not both 
	BOOL bZAxis = theApp.GetCurrentProblem()->UseZAxis();
	m_cboZDir.ShowWindow(bZAxis ? SW_SHOW : SW_HIDE);
	m_stcAngIcon.ShowWindow(bZAxis ? SW_HIDE : SW_SHOW);
	m_btnDirUnknown.ShowWindow(SW_SHOW);
	
}

void CValueDlg::OnCompoBtn() 
{
	// get here => can define compos
	// hide mag/dir form controls
	GetDlgItem(IDC_STATIC_MAG)->ShowWindow(SW_HIDE);
	m_stcMagLabel.ShowWindow(SW_HIDE);
	m_editMagValue.ShowWindow(SW_HIDE);
	GetDlgItem(IDC_STATIC_MAG_OR)->ShowWindow(SW_HIDE);
	m_btnMagUnknown.ShowWindow(SW_HIDE);

	GetDlgItem(IDC_STATIC_DIR)->ShowWindow(SW_HIDE);
	m_stcDirLabel.ShowWindow(SW_HIDE);
	m_editDirValue.ShowWindow(SW_HIDE);
	GetDlgItem(IDC_DIRECTION_SPIN)->ShowWindow(SW_HIDE);
	GetDlgItem(IDC_STATIC_DEG)->ShowWindow(SW_HIDE);
	m_stcAngIcon.ShowWindow(SW_HIDE);
	GetDlgItem(IDC_STATIC_DIR_OR)->ShowWindow(SW_HIDE);
	m_cboZDir.ShowWindow(SW_HIDE);
	m_btnDirUnknown.ShowWindow(SW_HIDE);

	// maybe set orientation to unknown

	////////////////////////////
	// show compo form controls
	////////////////////////////

	// if no axes drawn, just show message on form
	// compo form controls should default to invisible, 
	// so no need to hide them.
	if (!theApp.GetCurrentProblem()->IsAxesDrawn()) {
		GetDlgItem(IDC_STATIC_AXES_MSG)->ShowWindow(SW_SHOW);
		return;
	} 

	GetDlgItem(IDC_STATIC_XC)->ShowWindow(SW_SHOW);
	m_stcXCLabel.ShowWindow(SW_SHOW);
	m_editXCValue.ShowWindow(SW_SHOW);
	GetDlgItem(IDC_STATIC_XC_OR)->ShowWindow(SW_SHOW);
	m_btnXCUnknown.ShowWindow(SW_SHOW);
	
	GetDlgItem(IDC_STATIC_YC)->ShowWindow(SW_SHOW);
	m_stcYCLabel.ShowWindow(SW_SHOW);
	m_editYCValue.ShowWindow(SW_SHOW);
	GetDlgItem(IDC_STATIC_YC_OR)->ShowWindow(SW_SHOW);
	m_btnYCUnknown.ShowWindow(SW_SHOW);

	// if Z-axis used on problem:
	if (theApp.GetCurrentProblem()->UseZAxis()) {
		GetDlgItem(IDC_STATIC_ZC)->ShowWindow(SW_SHOW);
		m_stcZCLabel.ShowWindow(SW_SHOW);
		m_editZCValue.ShowWindow(SW_SHOW);
		GetDlgItem(IDC_STATIC_ZC_OR)->ShowWindow(SW_SHOW);
		m_btnZCUnknown.ShowWindow(SW_SHOW);
	}
}

// Following are for normal vector attributes

// Magnitude
void CValueDlg::OnCheckMagUnknown() 
{
	if (m_btnMagUnknown.GetCheck()) {
		m_editMagValue.SetWindowText("");
	} else {
		m_editMagValue.SetFocus();
	}
}
void CValueDlg::OnChangeMagValue() 
{
	CString strText;
	m_editMagValue.GetWindowText(strText);
	strText.Remove(' ');
	m_btnMagUnknown.SetCheck(strText.IsEmpty());
	// if this has changed to zero mag, disable direction.
	// But don't change value, since user might be in the midst of
	// typing 0.05, and don't want to clobber. (Could change on kill focus)
	int fToldMag;
	if (sscanf(strText, "%f", &fToldMag) == 1 && fToldMag == 0.0) {
			// no direction for zero magnitude vectors
			m_editDirValue.EnableWindow(FALSE);
			m_spinDirection.EnableWindow(FALSE);
			m_cboZDir.EnableWindow(FALSE);
	} 
	else // not a zero length vector (maybe unknown)
	{ 
			m_editDirValue.EnableWindow(TRUE);
			m_spinDirection.EnableWindow(TRUE);
			m_cboZDir.EnableWindow(TRUE);
	}

	// if this has changed to non-zero mag, enable direction as
	// appropriate
}

// Direction
void CValueDlg::OnCheckDirUnknown() 
{
	if (m_btnDirUnknown.GetCheck()) {
		m_editDirValue.SetWindowText("");
	} else {
		m_editDirValue.SetFocus();
	}
}
void CValueDlg::OnChangeDirValue() 
{
	// this event can be received in initialization before 
	if (! m_editDirValue.m_hWnd) return; 

	CString strText; int nDegrees;
	m_editDirValue.GetWindowText(strText);
	strText.Remove(' ');
	// unlike others, this is unknown if we can't parse an integer from it.
	// !!! doesn't handle garbage after integer (maybe floating point).
	m_btnDirUnknown.SetCheck( sscanf(strText, "%d", &nDegrees) != 1 );
	
}

// X component
void CValueDlg::OnCheckXCUnknown() 
{
	if (m_btnXCUnknown.GetCheck()) {
		m_editXCValue.SetWindowText("");
	} else {
		m_editXCValue.SetFocus();
	}
	
}
void CValueDlg::OnChangeXCValue() 
{
	CString strText;
	m_editXCValue.GetWindowText(strText);
	strText.Remove(' ');
	m_btnXCUnknown.SetCheck(strText.IsEmpty());
}

// Y component
void CValueDlg::OnCheckYCUnknown() 
{
	if (m_btnYCUnknown.GetCheck()) {
		m_editYCValue.SetWindowText("");
	} else {
		m_editYCValue.SetFocus();
	}
}

void CValueDlg::OnChangeYCValue() 
{
	CString strText;
	m_editYCValue.GetWindowText(strText);
	strText.Remove(' ');
	m_btnYCUnknown.SetCheck(strText.IsEmpty());
}

// Z component:
void CValueDlg::OnCheckZCUnknown() 
{
	if (m_btnZCUnknown.GetCheck()) {
		m_editZCValue.SetWindowText("");
	} else {
		m_editZCValue.SetFocus();
	}
}

void CValueDlg::OnChangeZCValue() 
{
	CString strText;
	m_editZCValue.GetWindowText(strText);
	strText.Remove(' ');
	m_btnZCUnknown.SetCheck(strText.IsEmpty());
}

void CValueDlg::OnUpdateName(CString strName)
{
	// handle change in vector name
	m_stcMagLabel.SetRichText(strName + "=");
	m_stcDirLabel.SetRichText("$q" + strName + "=");
	m_stcXCLabel.SetRichText(strName + "_x=");
	m_stcYCLabel.SetRichText(strName + "_y=");
	m_stcZCLabel.SetRichText(strName + "_z=");
}


LRESULT CValueDlg::DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam) 
{
	// Horrible hack. Copied from CCheckedDlg. Needed to get mouse clicks on popup listboxes
	// for this dialog passed through filter in CCheckedDlg::DoModalWithHints, which the parent 
	// dialog will use when containing this sub-dialog as a control. 
	if ( WM_CTLCOLORLISTBOX == message ) 
    {
        // Get the list box window handle (combo box drop down list)
        // The parent of the list box is the desktop window
	
		// the drop list of a combobox is a control of the dialog that needs to 
		// receive messages, but the wnd is not a child of the dialog
		// We need to get parent dialog to save its hwnd so it passes it along
		CWnd* pParent = GetParent();
		if (pParent->IsKindOf(RUNTIME_CLASS(CCheckedDlg)))
			((CCheckedDlg*)pParent)->AllowMessages((HWND) lParam);
    }
	
	return CLogDialog::DefWindowProc(message, wParam, lParam);
}

// Value transfer from/to parent dialog's vector object

void CValueDlg::TransferValues(BOOL bSaving)
{
	UpdateData(bSaving); // to sync radio button check with our m_bCompoForm value

	if (! bSaving) // going from object to dialog
	{
		// initialize direction
		if (m_pVec->IsZeroMag() && !m_pVec->IsZAxisVector()) {
			// no direction for zero magnitude vectors
			m_editDirValue.EnableWindow(FALSE);
			m_spinDirection.EnableWindow(FALSE);
			m_cboZDir.EnableWindow(FALSE);
			m_editDirValue.SetWindowText(""); // NB: OK to transfer back below
		} else {
			if (! m_pVec->IsZAxisVector()) {
				int nDegrees;
				if (m_pVec->GetOrientation(nDegrees)) { 
					// ASSERT((0 <= nDegrees) && (nDegrees < 360));
					m_spinDirection.SetRange(nDegrees - 20, nDegrees + 20);
				}
				m_editDirValue.SetWindowText(m_pVec->m_strOrientation);
			} 
			m_cboZDir.SetCurSel(m_pVec->m_nZDir);
			OnSelchangeZdir();
		}

		// update other values
		if (m_pVec->IsZeroMag())  // drawn as zero-mag. 
		{
			// zero-drawing overrides told values
			m_editMagValue.SetWindowText("0");
			m_editXCValue.SetWindowText("0");
			m_editYCValue.SetWindowText("0");
			m_editZCValue.SetWindowText("0");
		
/*
			// for now, disable controls to disallow changes if drawn as zero
			m_editMagValue.EnableWindow(FALSE);
			m_editDirValue.EnableWindow(FALSE);
			m_editXCValue.EnableWindow(FALSE);
			m_editYCValue.EnableWindow(FALSE);
			m_editZCValue.EnableWindow(FALSE);
			m_btnMagUnknown.EnableWindow(FALSE);
			m_btnDirUnknown.EnableWindow(FALSE);
			m_btnXCUnknown.EnableWindow(FALSE);
			m_btnYCUnknown.EnableWindow(FALSE);
			m_btnZCUnknown.EnableWindow(FALSE);
*/
		}
		else // drawn non-zero mag
		{
			m_editMagValue.SetRichEditText(m_pVec->m_strMag); OnChangeMagValue();
			m_editXCValue.SetRichEditText(m_pVec->m_strXC);	  OnChangeXCValue();
			m_editYCValue.SetRichEditText(m_pVec->m_strYC);   OnChangeYCValue();
			m_editZCValue.SetRichEditText(m_pVec->m_strZC);   OnChangeZCValue();
		}
	}
	else  // save values from dialog to object.
	{
		m_pVec->m_bHaveValues = TRUE;
		m_pVec->m_bCompoForm = m_bCompoForm;
		m_editMagValue.GetRichEditText(m_pVec->m_strMag);
		m_editXCValue.GetRichEditText(m_pVec->m_strXC);
		m_editYCValue.GetRichEditText(m_pVec->m_strYC);
		m_editZCValue.GetRichEditText(m_pVec->m_strZC);
		// transfer direction and type (xy plane vs z-axis)

		// make sure drawn mag matches told mag
		m_pVec->SyncDrawnMag();

		// If component form, then set direction to unknown
		// !! should also do if currently specified as zero-mag
		if (m_bCompoForm) {
			m_pVec->m_strOrientation = "";
		} else {
			m_editDirValue.GetWindowText(m_pVec->m_strOrientation);
			if (m_cboZDir.IsWindowEnabled()) {
				int nZDir = m_cboZDir.GetCurSel();
				if (nZDir >= 0 && nZDir <= ZDIR_MAX)
					m_pVec->m_nZDir = nZDir;
			}
		}
		// This just updates member variables in temp obj. Closing dialog will use
		// updateobj to write through to underlying object. This should update
		// graphic to match new direction, zero/non-zero status.
	}
}

void CValueDlg::OnSelchangeZdir() 
{
	int nZDir = m_cboZDir.GetCurSel();
	if (nZDir < 0 || nZDir > ZDIR_MAX) return;

	// enable degree edit accordingly
	if (nZDir == ZDIR_NONE) {
		m_editDirValue.EnableWindow(TRUE);
		// static icon showing ccw orientation.Only show if no z-axis choice
		// m_stcAngIcon.ShowWindow(SW_SHOW);	
		// !!!if had been showing zdir direction, probably should resync displayed 
		// degree value with drawn x-y plane direction. For now, too bad if you change
	} else {
		// degree display now read only
		m_editDirValue.EnableWindow(FALSE);
		// hide ccw angle icon
		m_stcAngIcon.ShowWindow(SW_HIDE);
		// display Z-Axis degrees in orientation field
		m_editDirValue.SetWindowText(nZDir == ZDIR_OUTOF ? "0" :
											(nZDir == ZDIR_INTO ? "180" : "?"));
	}
	
	// mainly to change phi/theta label if needed:
	//UpdateComponents();
}


HBRUSH CValueDlg::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	// HBRUSH hbr = CLogDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	
	// TODO: Change any attributes of the DC here
	
	// TODO: Return a different brush if the default is not desired

	// Handle in containing parent.
	const MSG* pMsg = GetCurrentMessage();
	return (HBRUSH) GetParent()->SendMessage(pMsg->message, pMsg->wParam, pMsg->lParam);
}
