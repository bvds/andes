// PicCtrl.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "PicCtrl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPicCtrl dialog


CPicCtrl::CPicCtrl(BOOL bOpenFileDialog, 
		LPCTSTR lpszDefExt, LPCTSTR lpszFileName, DWORD dwFlags,
		LPCTSTR lpszFilter,	CWnd* pParentWnd) : CFileDialog(
		bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags,
		lpszFilter, pParentWnd)
{
	//{{AFX_DATA_INIT(CPicCtrl)
	//}}AFX_DATA_INIT
	m_ofn.Flags |= OFN_NOCHANGEDIR | OFN_ENABLETEMPLATE | OFN_EXPLORER | OFN_ENABLEHOOK; 
	m_ofn.lpTemplateName = MAKEINTRESOURCE(IDD_FILEOPEN_PIC);
	if (lpszFilter == NULL)
		m_ofn.lpstrFilter = "Andes Physics Problems (.fbd)";
}


void CPicCtrl::DoDataExchange(CDataExchange* pDX)
{
	CFileDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPicCtrl)
	DDX_Control(pDX, IDC_FILE_PREVIEW, m_ctrlPreview);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPicCtrl, CFileDialog)
	//{{AFX_MSG_MAP(CPicCtrl)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPicCtrl message handlers

BOOL CPicCtrl::OnInitDialog() 
{
	return CFileDialog::OnInitDialog();;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// Draw the preview image on the static control.
// We render an image of the document into an offscreen bitmap, then the final bitblt in this routine
// paints it over the client area of the preview window -- a static text control. Note this method involves
// painting over this control "from outside" --  so it doesn't handle repainting, e.g. if the static is 
// covered then uncovered, it will just repaint its default appearance. Different method should really be 
// used that sets the bitmap of a picture control, or uses a subclassed control that manages repainting.
// 
void CPicCtrl::CreatePreview(CFBDDoc* pTempDoc)
{
	CRect rect;
	m_ctrlPreview.GetClientRect(rect);

	CWnd* pWnd = (CWnd*)&m_ctrlPreview;
	CClientDC dc(pWnd);
	// Create a memory DC that will hold the picture
	CDC* pMemDC = new CDC;
	ASSERT(pMemDC);

	if (!pMemDC->CreateCompatibleDC(&dc)) { // does an attach 
		TRACE("Failed to create compatible DC.");
		return;
	}

	// Create a bitmap the same size as the Static
	// and select it into the memory dc so we can do a BitBlt
	CBitmap* pBmp = new CBitmap;
	ASSERT(pBmp);
	CSize docSize = pTempDoc->GetSize();
	
	if (!pBmp->CreateCompatibleBitmap(&dc, docSize.cx, docSize.cy)) {
		TRACE("Failed to create discardable bitmap.");
		return;
	}
	CBitmap* pOldBmp = pMemDC->SelectObject(pBmp);
	dc.FillSolidRect(rect, RGB(255, 255, 255));	
	CRect bmpRect(0, 0, docSize.cx, docSize.cy);
	pMemDC->FillSolidRect(bmpRect, RGB(255, 255, 255));
	POSITION pos = pTempDoc->m_objects.GetHeadPosition();
	while (pos != NULL){
		CDrawObj* pObj = pTempDoc->m_objects.GetNext(pos);
		pObj->Draw(pMemDC);
	}

	if (pOldBmp == NULL) {
		TRACE("Failed to select bitmap.");
	}
	dc.SetMapMode(MM_ISOTROPIC);//we want the x and y to be in proportion
	dc.SetWindowExt(docSize.cx, docSize.cy);
	dc.SetViewportExt(rect.Width(), rect.Height());
	dc.SetViewportOrg(0, 0);
	dc.SetWindowOrg(0, 0);
	// Get the memory DC picture onto the screen
	if (!dc.BitBlt(0, 0,  docSize.cx, docSize.cy, 
			            pMemDC, 0, 0,SRCCOPY)) {
		TRACE("Failed to blt to the memory dc.");
			return;
	}

	pMemDC->SelectObject(pOldBmp);
	delete pMemDC;
	delete pBmp;
}

void CPicCtrl:: OnFileNameChange()
{
	CString strFile = GetPathName();
	CString strName = GetFileName();
	CString strExt = GetFileExt();
	strExt.MakeLower();
	BOOL bDrewOK = FALSE;
	if ((strFile.Find('*') == -1) && (strExt == "fbd") && (strName != m_strOldName)){
		
		// allocate temp doc for preview. Doc not on doctemplate list & has no views.
		// dyncreate from RUNTIME CLASS, since default constructor is protected
		CFBDDoc* pTempDoc = (CFBDDoc*)(RUNTIME_CLASS(CFBDDoc))->CreateObject();
		if (pTempDoc) {
			// flag open reason as preview mode to doc open/close methods
			pTempDoc->m_bFilePreview = TRUE;
			
			// load doc and create preview image
			// !!! Any message box for file load exception is bad here, seems to hang the dialog
			if (pTempDoc->OnOpenDocument(strFile)) {
				CreatePreview(pTempDoc);
				bDrewOK = TRUE;
			}
			// clean up temp doc in any case
			pTempDoc->DeleteContents();		// good style, but not currently implemented
			delete pTempDoc;
		}
	}
	// clear any earlier preview image if didn't draw OK
	if (! bDrewOK)
		m_ctrlPreview.Invalidate();  // repaint suffices to clear it

	m_strOldName = strName;
	CFileDialog::OnFileNameChange();
}
