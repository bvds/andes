//MyMenu.cpp
//Implementation file for my owner-drawn menus
#include "stdafx.h"
#include "fbd.h"
#include "MainFrm.h"
#include "MyMenu.h"
#include "GreekOpts.h"			// Greek text drawing funcs
#include "VarView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define CX_BORDER	4
#define CY_BORDER	1
////////////////////////////////////////////////////////////////////////////////////////
//CMyMenu
////////////////////////////////////////////////////////////////////////////////////////
IMPLEMENT_DYNCREATE(CMyMenu, CMenu);

CMyMenu::CMyMenu(){}

CMyMenu::~CMyMenu(){}

void CMyMenu::MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct)
{
	
	CDC *pDC = AfxGetApp()->m_pMainWnd->GetDC();	

 	LPCTSTR lpz = (LPCTSTR)lpMeasureItemStruct->itemData;

  	CGdiObject* pOldFont = pDC->SelectStockObject(SYSTEM_FONT);//in menu strings
  
	//so that we get the proper text extent
  	CString str = lpz;//menu string passed in itemData
  	CSize size = pDC->GetTextExtent(str);
  	lpMeasureItemStruct->itemWidth = size.cx;
  	lpMeasureItemStruct->itemHeight= size.cy;

	pDC->SelectObject(pOldFont);//select old font

 	AfxGetApp()->m_pMainWnd->ReleaseDC(pDC);//release dc
}

void CMyMenu::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{}

void CMyMenu::ChangeMenuItem(UINT nID, LPCTSTR lpz)
{

}

////////////////////////////////////////////////////////////////////////////////////////
//CGreekMenu
////////////////////////////////////////////////////////////////////////////////////////
IMPLEMENT_DYNCREATE(CGreekMenu, CMyMenu);

CGreekMenu::CGreekMenu(){}

CGreekMenu::~CGreekMenu()
{
	Detach();
	ASSERT(m_hMenu == NULL);
}


void CGreekMenu::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	CDC* pDC = CDC::FromHandle(lpDrawItemStruct->hDC);
 	
  	CRect rcText(lpDrawItemStruct->rcItem);
 	
	if (lpDrawItemStruct->itemState & ODS_FOCUS)//if menuitem has focus
 		pDC->DrawFocusRect(&rcText);//draw focus rect
 	//backround color is highlight color if selected,
 	//normal menu color if not
 	COLORREF cr = (lpDrawItemStruct->itemState & ODS_SELECTED) ?
 		::GetSysColor(COLOR_HIGHLIGHT) : ::GetSysColor(COLOR_MENU) ;
 	
	CBrush* pBrush = new CBrush(cr);
 	int nBkMode = pDC->SetBkMode(TRANSPARENT);//make transparent 
 	pDC->FillRect( &rcText, pBrush);
 	delete pBrush;
 	
 	cr = (lpDrawItemStruct->itemState & ODS_SELECTED) ?
		::GetSysColor(COLOR_HIGHLIGHTTEXT) : ::GetSysColor(COLOR_MENUTEXT);

	COLORREF oldCr = pDC->SetTextColor(cr);
		
 	LPCTSTR lpz = (LPCTSTR)lpDrawItemStruct->itemData;
 	CString str = lpz;//menu string passed in itemData
	
	OSVERSIONINFO osvi;
	osvi.dwOSVersionInfoSize = sizeof(osvi);
	GetVersionEx(&osvi);

	int cxBorder = 0;
	if (osvi.dwPlatformId == VER_PLATFORM_WIN32_NT) 
		cxBorder = GetSystemMetrics(SM_CXBORDER);
	else
		cxBorder = 2 * CX_BORDER;

	rcText.left = GetSystemMetrics(SM_CXMENUCHECK) + cxBorder;
	rcText.top = rcText.top + CY_BORDER;
	CGreekText::DrawText(pDC, rcText, str);//Now also need to draw VarList context menu,
										//(which may include greek text)
										// specifically, "Solve for..."
 	pDC->SetBkMode(nBkMode);//set to previous bk mode
 	pDC->SetTextColor(oldCr);//set to previous txt color
}

void CGreekMenu::ChangeMenuItem(UINT nID, LPCTSTR lpz)
{
	ModifyMenu(nID, MF_BYCOMMAND|MF_OWNERDRAW, nID, lpz);
}
////////////////////////////////////////////////////////////////////////////////////////
//CPinkMenu
////////////////////////////////////////////////////////////////////////////////////////
IMPLEMENT_DYNCREATE(CPinkMenu, CMyMenu);

CPinkMenu::CPinkMenu(){}

CPinkMenu::~CPinkMenu()
{
	Detach();
	ASSERT(m_hMenu == NULL);
}


void CPinkMenu::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
 	BITMAP bm;
 	CBitmap bitmap;
 	bitmap.LoadBitmap(IDB_QMARK);//load question mark bitmap
 	bitmap.GetObject(sizeof (bm), &bm);
 
	CDC* pDC = CDC::FromHandle(lpDrawItemStruct->hDC);
 	
  	CRect rc(lpDrawItemStruct->rcItem);

 	if (lpDrawItemStruct->itemState & ODS_FOCUS)//if menuitem has focus
 		pDC->DrawFocusRect(&rc);//draw focus rect

 	//backround color is highlight color if selected,
 	//normal menu color if not
 	COLORREF cr = (lpDrawItemStruct->itemState & ODS_SELECTED) ?
 		::GetSysColor(COLOR_HIGHLIGHT) : ::GetSysColor(COLOR_MENU) ;

 	CBrush* pBrush = new CBrush(cr);
 	int nBkMode = pDC->SetBkMode(TRANSPARENT);//make transparent 
 	pDC->FillRect( &rc, pBrush);
 	delete pBrush;
 	
  	//text color is highlight color if selected
 	//purple if not (Example's owner-drawn menu)
 	cr = (lpDrawItemStruct->itemState & ODS_SELECTED) ?
		::GetSysColor(COLOR_HIGHLIGHTTEXT) : RGB(128, 0, 128);

	COLORREF oldCr = pDC->SetTextColor(cr);
	ASSERT(lpDrawItemStruct->itemState & ODS_CHECKED);
		
 	LPCTSTR lpz = (LPCTSTR)lpDrawItemStruct->itemData;
 	CString str = lpz;//menu string passed in itemData

	int xPos = 	GetSystemMetrics(SM_CXMENUCHECK) + (2 * CX_BORDER);
	int yPos = rc.top + CY_BORDER;
	pDC->TextOut(xPos, yPos, str);//print menu string

	//if menu item checked, add bitmap (true for Example's owner-drawn menu)
  	//only checked items are owner-drawn in Example view
 	if (lpDrawItemStruct->itemState & ODS_CHECKED)
	{
 		CDC dcMem;
 		dcMem.CreateCompatibleDC (pDC);
 		CBitmap* pOldBitmap = dcMem.SelectObject (&bitmap);
 		pDC->BitBlt((rc.left + CX_BORDER), (rc.top + CY_BORDER),
 			 bm.bmWidth, bm.bmHeight, &dcMem, 0 , 0, SRCAND);
 		dcMem.SelectObject(pOldBitmap);//select old object
 	}
 	pDC->SetBkMode(nBkMode);//set to previous bk mode
 	pDC->SetTextColor(cr);//set to previous txt color
 	
}
  
void CPinkMenu::ChangeMenuItem(UINT nID, LPCTSTR lpz)
{
	ModifyMenu(nID, MF_BYCOMMAND|MF_OWNERDRAW|MF_CHECKED, nID, lpz);

}

////////////////////////////////////////////////////////////////////////////////////////
//CVarMenu
////////////////////////////////////////////////////////////////////////////////////////
// Table data for initializing quantity menu
typedef struct {
	char* str;			// quantity name in menu
    char* strId;		// quantity type ID 
	DWORD concept;		// bitmask of problem types in which this item should be used
} PROBLEM_MENUITEMS; 

static const PROBLEM_MENUITEMS menuitems[] = 
{
// optional vector quantities come first. These are not offered for the usual
// variable definition menu so student is forced to draw all vectors

"Force", "force",
			(ID_PROB_FORCE|ID_PROB_CIRCMOTION|ID_PROB_ROTKINEMATICS|ID_PROB_FLUIDS|ID_PROB_EM), 			
"Acceleration",	"acceleration",    
			(ID_PROB_VECTOR|ID_PROB_FORCE|ID_PROB_KINEMATICS|ID_PROB_CIRCMOTION|ID_PROB_ROTKINEMATICS),
"Velocity",   "velocity",		  
			(ID_PROB_VECTOR|ID_PROB_KINEMATICS|ID_PROB_CIRCMOTION|ID_PROB_ROTKINEMATICS|ID_PROB_ENERGY|ID_PROB_FLUIDS),	
"Displacement",	"displacement",    			                   
			(ID_PROB_VECTOR|ID_PROB_KINEMATICS|ID_PROB_ROTKINEMATICS|ID_PROB_ENERGY),					
"Momentum", "momentum",
			(ID_PROB_MOMENTUM|ID_PROB_ROTKINEMATICS),
"Relative Position", "position",
			(ID_PROB_ROTKINEMATICS|ID_PROB_KINEMATICS),
"Torque",	"torque", (ID_PROB_ROTKINEMATICS | ID_PROB_EM),
"Relative Velocity", "relative-vel", ID_PROB_RELVEL,
"Electric Field", "E-field", ID_PROB_EM,
"Magnetic Field", "B-field", ID_PROB_EM,
// If you add vector quantity here, must increment FIRST_SCALAR_INDEX!

//Place separator between vectors and scalars:
"",		"MF_SEPARATOR",	0xFFFF,	// include for all concept flags

#define FIRST_SCALAR_INDEX  11  // index of first scalar quantity in this table, which follows:
//
// !!! Following scalar portion of the table now unused. 
// Now scalar quants added to menu from quantity table in CVarView::AddScalarVars, 
// based on feature sets loaded from features.tsv
//
"Mass",		"mass",							
	(ID_PROB_FORCE|ID_PROB_ENERGY|ID_PROB_CIRCMOTION|ID_PROB_ROTKINEMATICS), // for mom inertia
"Radius",	"radius",		  
	(ID_PROB_CIRCMOTION|ID_PROB_ROTKINEMATICS),
/* No longer offered in Andes2   
"Distance-between",	ID_VARIABLE_ADDDISTANCE,		 	ID_PROB_KINEMATICS,
*/          
"Distance traveled",   "distance",	 ID_PROB_KINEMATICS,
"Speed",    "speed", ID_PROB_KINEMATICS,
"Time",		"duration", 0xFFFFFFFF,  // always on 
"Period",	"period", (ID_PROB_CIRCMOTION),   
"Angle",	"angle",                       
    (ID_PROB_VECTOR|ID_PROB_FORCE|ID_PROB_KINEMATICS|ID_PROB_ENERGY|ID_PROB_CIRCMOTION|ID_PROB_ROTKINEMATICS), 
"Work",		"work", ID_PROB_WORK|ID_PROB_ENERGY,
"Power",	"power", ID_PROB_WORK|ID_PROB_ENERGY,
"Power",    "electric-power", ID_PROB_CIRCUITS,
"Coefficient of friction", "coef-friction", ID_PROB_FORCE,

// peculiar to rotational problems:
// Body dimensions needed in angmom, torque, for computing moment of inertia
// for now, show in all problems labelled rotkin.
"Moment of Inertia",	"moment-of-inertia",		ID_PROB_ROTKINEMATICS, 	
"Length",	"length", ID_PROB_ROTKINEMATICS,
"Width",	"width", ID_PROB_ROTKINEMATICS,

// peculiar to energy problems. KE also needed for momentum (elastic collisions)
// "",	"MF_SEPARATOR", ID_PROB_ENERGY,
"Energy",	"energy",    ID_PROB_ENERGY|ID_PROB_MOMENTUM,
"Compression Distance",		"compression",  ID_PROB_ENERGY,    			                     
"Spring Constant",   "spring-constant", ID_PROB_ENERGY,      	       
"Height",	"height",	ID_PROB_ENERGY,

// DC Circuits
"Current",	"current", ID_PROB_CIRCUITS,
"Voltage",	"voltage", ID_PROB_CIRCUITS,
"Resistance", "resistance", ID_PROB_CIRCUITS,
"Capacitance", "capacitance", ID_PROB_CIRCUITS,
"Charge",     "charge", ID_PROB_CIRCUITS|ID_PROB_EM,
"Potential", "potential", ID_PROB_EM,
"Stored Energy", "stored-energy", ID_PROB_CIRCUITS,
"Inductance",  "inductance", ID_PROB_CIRCUITS,
"Current Change Rate", "current-change", ID_PROB_CIRCUITS,
"Time Constant", "time-constant", ID_PROB_CIRCUITS,
// Optics
"Object Distance", "object-distance", ID_PROB_OPTICS,
"Image Distance", "image-distance", ID_PROB_OPTICS,
"Focal Length", "focal-length", ID_PROB_OPTICS,    
"Magnification",  "magnification", ID_PROB_OPTICS,
"Radius of Curvature", "radius-of-curvature", ID_PROB_OPTICS,
"Distance between Lenses", "lens-distance", ID_PROB_OPTICS,
};

static const int nMenuItems ARRAY_SIZE(menuitems);

IMPLEMENT_DYNCREATE(CVarMenu, CMyMenu);

CVarMenu::CVarMenu(){}

CVarMenu::~CVarMenu()
{
	Detach();
	ASSERT(m_hMenu == NULL);
}

// All we get back from a menu selection is a single integer command id. So we need 
// different ids for magnitude/force selection as for direction/force selection, from 
// which we can recover the parent attribute and the child vector type.
// We do this by generating new ids for the vector submenus in different ranges.
// Following are the bases of ranges used for these new commands.
// We use these bases plus the offset of original command id from ADD_VARIABLE base. 
// Note MFC Command ids normally have the high-bit  of 16-bit range set so start at 
// 0x8000 =  32768, vec cmds around 33000.
#define VEC_MAG_BASE   60000
#define VEC_DIR_BASE   60100
#define VEC_XC_BASE    60200
#define VEC_YC_BASE    60300
#define VEC_ZC_BASE    60400	

static struct {
	char* text;		// prefix to display in parent menu
	int base;		// base from which to displace submenu commands
	char* id;		// attribute id string to send to helpsys
} vecprops[] = {	// NB: must be in base number order for SplitQuantId
	"magnitude of", VEC_MAG_BASE, "mag",
	"direction of", VEC_DIR_BASE, "dir",
	"horizontal component of", VEC_XC_BASE, "xc",
	"vertical component of", VEC_YC_BASE,	"yc",
	// must be last for loop below:
	"z component of", VEC_ZC_BASE, "zc",	
};
static const int nVecProps ARRAY_SIZE(vecprops);

// Add menu items for given concept mask to created menu. 
void CVarMenu::AttachProblemMenu(DWORD dwConceptFlag, BOOL bIncludeVectors/*=FALSE*/)
{
	CMenu vecSubMenu;
	if (bIncludeVectors) {
		// only include final z-axis item if problem uses z-axis vectors
		int nProps = theApp.GetCurrentProblem()->UseZAxis() ? nVecProps : nVecProps - 1;
		for (int p = 0; p < nProps; p++) {
			// Build the appropriate sub-menu using displaced command ids coding
			// attribute + vec-cmd in one cmd id as attribute base + vec-cmd offset
			int nBase = vecprops[p].base;
			vecSubMenu.CreatePopupMenu();
			for (int v = 0; v < FIRST_SCALAR_INDEX-1; v++) {
				if (dwConceptFlag & menuitems[v].concept) {
					int nCmdId = CVarView::LookupId(menuitems[v].strId);
					ASSERT(nCmdId > ID_VARIABLE_ADDFIRST);
					int nDisplacedId = nBase + (nCmdId - ID_VARIABLE_ADDFIRST);
					vecSubMenu.AppendMenu(MF_STRING, nDisplacedId, menuitems[v].str);
				}
			}

			// detach hMenu and add it to parent menu (safe?)
			HMENU hSubMenu = vecSubMenu.Detach();
			AppendMenu(MF_POPUP|MF_STRING, (UINT) hSubMenu, vecprops[p].text);
		}

		AppendMenu(MF_SEPARATOR);
	}

	// use CVarView quantity table manager to add per-problem scalars
	CVarView::AddScalarVars(this, bIncludeVectors);

}

// split a selected quantity id from the quantity menu into base quantity id (returned) 
// plus optional vector attribute id strings. strVecProp is tag to send to the help system.
// strPrefix is human readable prefix. Strings unmodified if not a vector quantity.
int CVarMenu::SplitQuantId(int nID, CString &strVecProp, CString& strPrefix)
{
	// search down from greatest
	for (int i = nVecProps; --i >= 0;) {
		if (nID >= vecprops[i].base) {
			strVecProp = vecprops[i].id;
			strPrefix = vecprops[i].text;
			return ID_VARIABLE_ADDFIRST + (nID - vecprops[i].base);
		}
	}
	// get here => not a vector
	return nID;
}

// for log playback: convert string prop + vector type to current quantity code as above
int CVarMenu::MakeVectorPropCode(LPCSTR pszProp, LPCSTR pszVectorType)
{
	int nVecType = CVarView::LookupId(pszVectorType);
	if (nVecType < ID_VARIABLE_ADDFIRST) return -1;

	for (int p = 0; p < nVecProps; p++) {
		if (strcmpi(pszProp, vecprops[p].id)) {
			return vecprops[p].base + (nVecType - ID_VARIABLE_ADDFIRST);
		}
	}
	return -1;
}
