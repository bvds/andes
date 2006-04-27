// AngleDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "SymbolMenu.h"
#include "AngleDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAngleDlg dialog


CAngleDlg::CAngleDlg(CDrawObj* pObj, CWnd* pParent /*=NULL*/)
	: CDrawObjDlg(CAngleDlg::IDD, pObj, pParent)
{
	//{{AFX_DATA_INIT(CAngleDlg)
	//}}AFX_DATA_INIT
}

void CAngleDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAngleDlg)
	DDX_Control(pDX, IDC_STC_DIRECTION, m_stcDirLabel);
	DDX_Control(pDX, IDC_SIDE1, m_cboSide1);
	DDX_Control(pDX, IDC_SIDE2, m_cboSide2);
	DDX_Control(pDX, IDOK, m_btnOK);
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	DDX_Control(pDX, IDC_CUSTOM_LABEL, m_ctrlName);
	DDX_Control(pDX, IDC_DEGREES, m_ctlDegrees);
	//}}AFX_DATA_MAP
	DDX_Control(pDX, IDC_GREEKBTN, m_btnGreek);
	//Initializes controls from temporary object
	CDrawObjDlg::DoDataExchange(pDX);
}

BEGIN_CTL_TBL(CAngleDlg)
	"name",	IDC_CUSTOM_LABEL,
	"degrees", IDC_DEGREES,
	"side1",	IDC_SIDE1,
	"side2",	IDC_SIDE2,
	"OK",			IDOK,
	"Cancel",		IDCANCEL,
	// to accept long names for backwards-compatibility:
	"angle-name",	IDC_CUSTOM_LABEL,
	"angle-degrees", IDC_DEGREES,
	"angle-side1",	IDC_SIDE1,
	"angle-side2",	IDC_SIDE2,
END_CTL_TBL(CAngleDlg)



BEGIN_MESSAGE_MAP(CAngleDlg, CDrawObjDlg)
	//{{AFX_MSG_MAP(CAngleDlg)
	ON_BN_CLICKED(IDC_GREEKBTN, OnGreekbtn)
	ON_WM_MOVE()
	ON_CBN_SELCHANGE(IDC_SIDE1, OnSelchangeSide)
	ON_CBN_SELCHANGE(IDC_SIDE2, OnSelchangeSide)
	ON_CBN_CLOSEUP(IDC_SIDE1, OnCloseupSide)
	ON_CBN_CLOSEUP(IDC_SIDE2, OnCloseupSide)
	//}}AFX_MSG_MAP
	ON_COMMAND_RANGE(IDM_GREEKLETTER_FIRST, IDM_GREEKLETTER_LAST, OnInsertGreekLetter)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAngleDlg message handlers

BOOL CAngleDlg::OnInitDialog() 
{
	LogEventf(EV_DLG_ANGLE, "%s |%s|",m_pObj->m_strId, OBJ_NAME(m_pObj));

	CDrawObjDlg::OnInitDialog();//this is where we move the dialog to 
	
	// adjust for use as sought quantity
	if (m_bSought)
	{
		Remove(IDC_BOX_LABEL);
		// now hides all windows within this?
	}
	
	// Prepare the combobox controls
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CAngle)))
	{
		// The sides for a drawn angle cannot be changed from within the dialog, so
		// we set them to "read-only", i.e. disabled. Since normal disabled combo box
		// shows grey text which is hard to read, we use special technique to show
		// black text on grey background (see SetComboReadOnly.)
		SetComboReadOnly(&m_cboSide1);
		SetComboReadOnly(&m_cboSide2);
	}
	
	// run through document to populate angle side lists
	// Use reverse order so more recent obs are first
	if (! m_pDocument)
		return TRUE;

	POSITION pos = m_pDocument->m_objects.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = m_pDocument->m_objects.GetPrev(pos);
		if (  pObj->IsKindOf(RUNTIME_CLASS(CVector)) &&
						! pObj->m_strName.IsEmpty()  )
		{
			m_cboSide1.AddString(pObj->m_strName);
			m_cboSide2.AddString(pObj->m_strName);
		}
		else if (  pObj->IsKindOf(RUNTIME_CLASS(CGuideLine)) &&
						! pObj->m_strName.IsEmpty()  &&
						! ((CGuideLine*)pObj)->IsZAxisVector() )
		{
			m_cboSide1.AddString(pObj->m_strName);
			m_cboSide2.AddString(pObj->m_strName);
		}
		// don't add axes if used for a sought:
		if (m_bSought) continue;
		else if (pObj->IsKindOf(RUNTIME_CLASS(CAxes)) &&
				 ((CAxes*)pObj)->m_nIndex == 0) // only use first axes if many
		{
			for (int i=0; i < nAxesStrs; i++) {
				m_cboSide1.AddString(axes[i].strDef);
				m_cboSide2.AddString(axes[i].strDef);
			}
		}
		
	}
	
	//load sigma/sigma bitmap onto button which pops up the greek menu.
	HBITMAP hBitmap = (HBITMAP) ::LoadImage(AfxGetInstanceHandle(),
		MAKEINTRESOURCE(IDB_GREEKBTN), IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT|LR_LOADMAP3DCOLORS);
	m_btnGreek.SetBitmap(hBitmap);

	// Create Greek symbol "menu" window, which is actually a modeless dialog owned 
	// by this window. The dialog contains the greek symbol toolbar.
	if (!m_wndMenu.Create(IDD_HORZ_GREEKBAR, this))
			TRACE0("Failed to create symbol menu\n");

	// the Greek toolbar functions as a popup window that needs to receive 
	// messages even when the app is blocking messages to other windows.
	// Since the toolbar is not a child of the dialog, we register its hwnd as the
	// current popup, so the dialog message loop explicitly passes messages along.
	// This setting is overridden when droplists popup; we restore it when they close.
	m_hwndCtrl = m_wndMenu.m_TBar.GetSafeHwnd();

	// Place the menu.
	PositionGreekMenu();

	// initialize Data
	UpdateData(FALSE);

	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CAngle)))
		m_ctrlName.SetFocus();

	return FALSE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CAngleDlg::OnOK() 
{
	// retrieve equation string from control
	CString strName;
	m_ctrlName.GetRichEditText(strName);
	m_pTempObj->m_strName = strName;

	if (m_pObj->IsKindOf(RUNTIME_CLASS(CVariable)))
		UpdateTempVariable();

	if (IsEmpty(&m_cboSide1))
	{
		theApp.DoWarningMessage("Please select the first angle side", this);
		return;
	}

	if (IsEmpty(&m_cboSide2))
	{
		theApp.DoWarningMessage("Please select the second angle side", this);
		return;
	}

	if (! m_bSought) {
		CString str = m_pTempObj->m_strName;
		str.Remove('$');
			
		if (!IsValidLabel(str))	return;

		if (! m_pTempObj->IsValid()) return;
	}

	if (!CheckDialog())	return;
	
	// Finished OK: transfer new props into obj
	// UpdateObj() called from base class
	CDrawObjDlg::OnOK();
}

void CAngleDlg::OnGreekbtn() 
{
	if (!m_wndMenu.IsWindowVisible()){
		m_hwndCtrl = m_wndMenu.m_TBar.m_hWnd;
		m_wndMenu.ShowWindow(SW_SHOW);
		m_ctrlName.SetFocus();
	}								
	else{
		m_wndMenu.ShowWindow(SW_HIDE);
		m_btnGreek.SetCheck(0);
		m_ctrlName.SetFocus();
	}								
	
}

void CAngleDlg::OnMove(int x, int y) 
{
	CDrawObjDlg::OnMove(x, y);
	if (::IsWindow(m_wndMenu.m_hWnd)) // ignore on moves before InitDlg creates menu wnd
		PositionGreekMenu();
}

void CAngleDlg::OnInsertGreekLetter(UINT nID )
{
	CString alpha = "abgdezhqiklmnxoprstufcyw";//greek translation
	int pos = nID - IDM_GREEKLETTER_FIRST;
	CString strLetter = alpha[pos];
	// insert upper-case letter if SHIFT pressed or CAPSLOCK on
	if (   (::GetKeyState(VK_SHIFT)   & 0x8000)	 // high bit of (promoted) 16-bit result => pressed
		|| (::GetKeyState(VK_CAPITAL) & 0x0001)) // low bit of result => toggle key is "on". 
		strLetter.MakeUpper();


	m_ctrlName.InsertGreekText(strLetter);
	m_ctrlName.SetFocus();
}

void CAngleDlg::SetRichEditText(CString str)
{
 /*  int pos = str.Find('$');
	CString grchar;
	while (pos != -1){
		if (pos > 0){
			CString prevStr = str.Mid(0, pos);
			m_ctrlName.ReplaceSel(prevStr);
		}

		grchar = str.Mid(pos + 1, 1);
		CString gralpha = "abgdezhqiklmnxoprstufcyw";//greek translation
		int grpos = gralpha.Find(grchar);
		if (grpos != -1){
			UINT msg = (UINT)grpos + IDM_GREEKLETTER_FIRST;
			SendMessage(WM_COMMAND, msg);
		}
		str = str.Mid(pos + 2);
		pos = str.Find('$');
	
	}
  
	if (!str.IsEmpty())
		m_ctrlName.ReplaceSel(str);*/
}

void CAngleDlg::PositionGreekMenu()
{
	CRect btnPos, mnuPos;
	m_btnGreek.GetWindowRect(&btnPos);
	m_wndMenu.GetWindowRect(&mnuPos);
	m_wndMenu.MoveWindow(btnPos.right, btnPos.top,	
			mnuPos.Width(), mnuPos.Height(), TRUE);	

}

void CAngleDlg::InitObjectDlg()
{
	CAngle* pAng = (CAngle*)m_pTempObj;
	SetWindowText("Angle definition");

	CString str1, str2;
	if (!pAng->m_pAngSide1->IsKindOf(RUNTIME_CLASS(CAxes)))
		str1 = pAng->m_pAngSide1->m_strName;
	else
		str1 = axes[pAng->m_nAxis - 2].strDef;
	
	m_cboSide1.SelectStringExact(str1);

	if (!pAng->m_pAngSide2->IsKindOf(RUNTIME_CLASS(CAxes)))
		str2 = pAng->m_pAngSide2->m_strName;
	else
		str2 = axes[pAng->m_nAxis - 2].strDef;
	
	m_cboSide2.SelectStringExact(str2);

	UpdateAngleMagDisplay();
}

void CAngleDlg::InitVariableDlg()
{
	CVariable* pVar = (CVariable*)m_pTempObj;
	SetWindowText("Variable definition");

	if (!pVar->m_strObject.IsEmpty())
	{
		m_cboSide1.SelectString(-1, pVar->m_strObject);
	}
	if (!pVar->m_strAgent.IsEmpty())
	{
		m_cboSide2.SelectString(-1, pVar->m_strAgent);

	}

	UpdateAngleMagDisplay();
}

CLabelRichEdit* CAngleDlg::GetLabelCtrl()
{
	return &m_ctrlName;
}


void CAngleDlg::UpdateTempVariable()
{
	((CVariable*)m_pTempObj)->m_strObject = GetCurString(&m_cboSide1);
	((CVariable*)m_pTempObj)->m_strAgent = GetCurString(&m_cboSide2);
	((CVariable*)m_pTempObj)->m_strQuantName = "Angle";
	
	((CVariable*)m_pTempObj)->m_strDef = 
			((CVariable*)m_pTempObj)->m_strQuantName + " between " + 
				((CVariable*)m_pTempObj)->m_strObject + " and " +
					((CVariable*)m_pTempObj)->m_strAgent;
}


void CAngleDlg::OnSelchangeSide() 
{
	if (m_pTempObj->IsKindOf(RUNTIME_CLASS(CVariable)))
		UpdateAngleMagDisplay();
}

// Update static text displaying angle magnitude in degrees
void CAngleDlg::UpdateAngleMagDisplay()
{
	// calc degrees and find out if axis is involved
	BOOL bAxis = FALSE;
	int nDeg = GetAngleMag(&bAxis);
	

	// update static display
	CString strDeg = "?";
	if (nDeg != -1)
		strDeg.Format("%d", nDeg);
	m_ctlDegrees.SetWindowText(strDeg);

	// also update direction explanation
	if (bAxis)
		m_stcDirLabel.SetWindowText("(counterclockwise)");
	else
		m_stcDirLabel.SetWindowText("(the smaller of the two possible angles.)");
}

//
// Return magnitude of currently defined angle from controls, -1 if unknown
// return val, pbAxis out parameter as in GetAngleBetween
// 
int CAngleDlg::GetAngleMag(BOOL* pbAxis /*= NULL*/)
{
	CString strSide1, strSide2;
	m_cboSide1.GetWindowText(strSide1);
	m_cboSide2.GetWindowText(strSide2);

	return GetAngleBetween(strSide1, strSide2, pbAxis);
}

//
// Calculate angle between, given two side ids.
// returns angle in degrees, or -1 if angle is unknown.
// there are two types of angle, an angle between vectors or an angle from an axis.
// OUT: pbAxis, if non-null, is filled in with T if at least one side is an axis
// this method is static so can be used outside of dialog -- could be document method
int CAngleDlg::GetAngleBetween(const CString& strSide1, const CString& strSide2,
							   BOOL* pbAxis)
{
	int nAxis1, nAxis2;
	int nAngle1, nAngle2;
	int zDir1, zDir2;
	int nAng = -1;
	
	// see if one or other side is an axis spec
	BOOL bIsAxis1 = FALSE;		// unless we find name on axis list
	BOOL bIsAxis2 = FALSE;
	for (int i=0; i < nAxesStrs; i++)
	{
		if (strSide1 == axes[i].strDef) {
			bIsAxis1 = TRUE;
			nAxis1 = i;
		}
		if (strSide2 == axes[i].strDef) {
			bIsAxis2 = TRUE;
			nAxis2 = i;
		}
	}	
	// fill axis flag out parameter if requested
	if (pbAxis)
		*pbAxis = bIsAxis1 || bIsAxis2;

	// no angle if don't have both strings
	if (strSide1.IsEmpty() || strSide2.IsEmpty())
		return -1;

	// search objects to find obj for side1 and get its angle
	CFBDDoc* pDoc = (CFBDDoc*)theApp.GetDocument();
	POSITION pos = pDoc->m_objects.GetTailPosition();
	while (pos != NULL) 
	{
		CDrawObj* pObj = pDoc->m_objects.GetPrev(pos);
		if (bIsAxis1 && pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
		{
			nAngle1 = (pObj->GetDirection() + ((nAxis1) * 90)) % 360;
			break;
		}
		else if (pObj->m_strName == strSide1)  // hit side1 
		{
			// vector always has drawn direction, but may represent
			// vector with unknown orientation.
			if (pObj->IsKindOf(RUNTIME_CLASS(CVector))) {
				if (((CVector*)pObj)->UnknownDir()) 
					return -1;		// early exit 
				zDir1 = ((CVector*)pObj)->m_nZDir;
				((CVector*)pObj)->GetOrientation(nAngle1);	
			} 
			else if (pObj->IsKindOf(RUNTIME_CLASS(CGuideLine))){
				if (((CGuideLine*)pObj)->UnknownDir())
					return -1;		// early exit 
				 zDir1 = ((CGuideLine*)pObj)->m_nZDir;
				 nAngle1 = pObj->GetDirection();
			}
			break;
		}
	}
	// search objects to find obj for side2 and get its angle
	pos = pDoc->m_objects.GetTailPosition();
	while (pos != NULL) 
	{
		CDrawObj* pObj = pDoc->m_objects.GetPrev(pos);
		if (bIsAxis2 && pObj->IsKindOf(RUNTIME_CLASS(CAxes)))
		{
			nAngle2 = (pObj->GetDirection() + ((nAxis2) * 90)) % 360;
			break;
		}
		else if (pObj->m_strName == strSide2)
		{
			// vector always has drawn direction, but may represent
			// vector with unknown orientation.
			if (pObj->IsKindOf(RUNTIME_CLASS(CVector))) {
				if (((CVector*)pObj)->UnknownDir()) 
					return -1;		// early exit 
				zDir2 = ((CVector*)pObj)->m_nZDir;
				((CVector*)pObj)->GetOrientation(nAngle2);	
			} 
			else if (pObj->IsKindOf(RUNTIME_CLASS(CGuideLine))){
				if (((CGuideLine*)pObj)->UnknownDir())
					return -1;		// early exit 		
				 zDir2 = ((CGuideLine*)pObj)->m_nZDir;
				 nAngle2 = pObj->GetDirection();
			}
			break;
		}
	}

	// first deal with z-axis vector possibilities, coded in zDir1 and zDir2 as one
	// of {0 [ZDIR_NONE], 1 [ZDIR_INTO], 2 [ZDIR_OUTOF], 3 [ZDIR_UNKNOWN]}

	if ((zDir1==ZDIR_NONE) != (zDir2==ZDIR_NONE)) // not both same type of angle
		return 90;

	if (zDir1!=ZDIR_NONE && zDir2!=ZDIR_NONE) { // two z-axis vectors
		if (zDir1 == ZDIR_UNKNOWN || zDir2 == ZDIR_UNKNOWN) // z-unknown
			return -1;
		return zDir1 == zDir2 ? 0 : 180;
	}

	// else two x-y plane angles
	// calculate angle value depending on type of angle it is
	if (bIsAxis1 || bIsAxis2) // angle involves an axis:
	{	
		// want counterclockwise angle from first to second 
		if (nAngle1 > nAngle2)
   			nAng = 360 - abs(nAngle1 - nAngle2);
 		else
	  		nAng = abs(nAngle1 - nAngle2);
	} 
	else // angle between two vectors:
	{
		 // pick smaller of two angles
		int nDiff12 = abs(nAngle1 - nAngle2);
		int nDiff21 = 360 - nDiff12;
		nAng = (nDiff12 <= nDiff21) ? nDiff12 : nDiff21;
	}

	return nAng;
}

// handle closeup of drop-down list for side choice:
void CAngleDlg::OnCloseupSide() 
{
	// set current popup hWnd back to Greek menu, if it is up
	if (m_wndMenu.IsWindowVisible()){
		m_hwndCtrl = m_wndMenu.m_TBar.m_hWnd;
	}								
}



