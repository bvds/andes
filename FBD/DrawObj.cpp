//////////////////////////////////////////////////////////////////////////
//
// Drawing objects
//
//////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <math.h>
    
#include "FBD.h"
#include "history.h"
#include "HelpIfc.h"
#include "MainFrm.h"
#include "ChildFrm.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "FBDView.h"
#include "EXView.h"
#include "HintView.h"
#include "ChatView.h"
#include "VarView.h"
#include "GreekOpts.h"

// Property dialogs
#include "LabelDlg.h"
#include "HypertxtDlg.h"
#include "rectdlg.h"
#include "EXTxtDlg.h"
#include "ChoiceDlg.h"
#include "AreaDlg.h"
#include "AuthDlg.h"
#include "VariableDlg.h"
#include "VecDlg.h"
#include "VecAVDlg.h"
#include "LabRadDlg.h"
#include "AngleDlg.h"
#include "EnergyDlg.h"
#include "TorqueDlg.h"
#include "VecPosDlg.h"
#include "VoltageDlg.h"
#include "CurrentDlg.h"
#include "ResistanceDlg.h"
#include "RelVelDlg.h"
#include "FieldDlg.h"
#include "ImpulseDlg.h"
#include "ProbDlg.h"
#include "UnitVectorDlg.h"
#include "TimeConstantDlg.h"
#include "DipoleDlg.h"
    
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
    
int Round(double f)	// Round double to nearest int (not in std lib??)
{
 	int sign = f >= 0 ? +1 : -1;
   	return (int) (f + sign * 0.5); // int conversion truncates fractional part
}
    
//////////////////////////////////////////////////////////////////////////
// CDrawObj: Generic drawing object implementation
//
// There is a fair bit of implementation in the base class -- mainly 
// appropriate for objects defined by a bounding rectangle. Unfortunately 
// this is not always so appropriate for our objects.
//
// Note especially the virtual functions for hit testing and dealing with
// resize handles -- these define a protocol used by the view, which all
// objects must implement. Resize handles are identified by integer indices.
// For a rectangular object they run from 1 to 8. 
//
// Specialized drawing objects will override these.
/////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CDrawObj, CObject, VERSIONABLE_SCHEMA | 4)
CDrawObj::CDrawObj()
{
	// Null rect is default position for empty drawing object
	m_position = CRect(0,0,0,0);
	// Object floats outside any document till Add()
	m_pDocument = NULL;	
	m_status = statusUnknown;	
	// flag initialized based on application mode in which created.
	m_flag = theApp.m_bAuthorMode ? TEACHER_OBJECT : STUDENT_OBJECT;

	m_pEXInfo = NULL;				// used by CVariables
	// Default constructors init CString & list-valued members to empty 
}			
    
CDrawObj::CDrawObj(const CRect& position)
	:m_position(position)
{
	m_pDocument = NULL;	// Object floats outside any document till Add()
	m_status = statusUnknown;

	// flag initialized based on application mode in which created.
	m_flag = theApp.m_bAuthorMode ? TEACHER_OBJECT : STUDENT_OBJECT;

	m_pEXInfo = NULL;
}
    
CDrawObj::CDrawObj(const CRect& position, const CString& name)
   	:m_position(position), m_strName(name)
{
	m_pDocument = NULL;		// obj floats outside any document till Add()
	m_status = statusUnknown;
	
	// flag initialized based on application mode in which created.
	m_flag = theApp.m_bAuthorMode ? TEACHER_OBJECT : STUDENT_OBJECT;

	m_pEXInfo = NULL;	
}
    
// Note on object serialization: Our first attempt did not serialize a version
// number for the CDrawObj base class, and so did not allow it to be versioned.
// This was changed and we generated a new document version 2. When serializing
// in the context of this document version, the child objects serialize the base class 
// header info (via SerializeClass) before calling the base class Serialize, so 
// the base class can get its class version number too. (It would probably have been 
// better to change the base class to do this in its Serialize function, but this
// was how it was done -- documentation does not explain how to version a base class).
//
// Note this has the result that the version of the containing doc affects the format 
// drawobjs use to serialize themselves. A complete object version spec now really 
// consists of: object version *in* a given document context. 
// E.g. Version 2 CAxes's in the context of Version 1 docs's will come out differently
// than Version 2 CAxes's in the context of Version 2 or later. (We probably should 
// have bumped all the object versions at the same time as we made the doc change.)
//
// This makes a difference because object serialization now happens even when 
// storing or loading objs themselves to/from the clipboard without serializing the
// whole containing document. We try to arrange the document version flag so that the 
// latest version is used in all serialization of objects other than those being
// read from old files.
// 
// Note that a back pointer to a containing document should be set in the archive 
// before attempting to load any serialized object. so that the objects can 
// initialize this correctly while loading -- and can also pick up the doc version.
// 
// This should not really be a problem in practice since older doc versions are
// going away, but it is worth noting the dependency on the document.
//
void CDrawObj::Serialize(CArchive& ar)
{
 	CObject::Serialize(ar);
  	if (ar.IsStoring())
   	{
   		// version 0 class info:
   		ar << m_position;
   		ar << m_strName;
   		//added in version 1
   		ar << m_flag;
   		//added in version 2
		//	ar << m_mark;	/* now unused */
   		// added version 3
   		ar << m_strId;
		if (m_pEXInfo)
			m_pEXInfo->m_relations.Serialize(ar);
    		
    }
    else	// loading
    {
    	// restore document back pointer from archive struct
    	m_pDocument = (CFBDDoc*) ar.m_pDocument;
    	ASSERT_VALID(m_pDocument);
    	ASSERT_KINDOF(CFBDDoc, m_pDocument);
    
		//Document versions >= 1 have serialized base class info so we can get
    	// our version number from the archive; else assume class version 0
    	UINT nClassVersion = 0;
    	if (m_pDocument->m_nVersion >= 1)
    		nClassVersion = ar.GetObjectSchema();
    
    	// Get version 0 members:
    	ar >> m_position;
    	ar >> m_strName;
    	m_status = statusUnknown;	// not serialized
    	// Get member data added in later versions or init them to defaults
    		
    	// Added version 1: WORD m_flag
    	m_flag = 0;
    	if (nClassVersion == 1) {
    		ar >>  m_flag;	
    	} 
    
    	// Obsolete ver2 stored two BYTES m_flag + m_mark; m_mark now unused.
    	if (nClassVersion == 2) {
    		BYTE byteTemp; 
    		ar >> byteTemp; m_flag = byteTemp;
    		ar >> byteTemp; // m_mark = bTemp;
    	}
    
    	// Version 3 and up: added m_strId (and revert to WORD m_flag)
    	if (nClassVersion >= 3) {
    		ar >> m_flag;
    		ar >> m_strId;
    	} else 
    		// For earlier versions, generate ID for drawobjs on loading
    		m_pDocument->GenerateId(this);
 //		CDocTemplate* pDocTemplate = m_pDocument->GetDocTemplate();
		if (m_pDocument->m_nProblemType==PROB_EXAMPLE){
//			&&(pDocTemplate!=theApp.m_ptmplExEdit))
				m_pEXInfo = new CEXInfo();
				if (nClassVersion >= 4)
					m_pEXInfo->m_relations.Serialize(ar);
		}
	
   		if (nClassVersion >= 5)
   			AfxThrowArchiveException(CArchiveException::badSchema);		
   	}
}
    
#ifdef _DEBUG
void CDrawObj::Dump(CDumpContext& dc) const
{
  	CObject::Dump(dc);
   	dc << "ID: " << m_strId << " Text: " << m_strName << "\n";
   	dc << "Pos: " << m_position << " Flag: " << m_flag << " Status: " << (int) m_status << "\n";
}
#endif // _DEBUG
    
// Destroy this object and free all its storage. !!!Why not in class destructors?
void CDrawObj::Delete()		
{
	//Delete Explain Info from example objects
	if (m_pEXInfo)
  		delete m_pEXInfo;

   	delete this;	// commits suicide, freeing object storage.
}

void CDrawObj::UpdateAngles()
{
	POSITION pos = m_Angles.GetHeadPosition();
	while (pos != NULL)
	{
		CDrawObj* pAng = m_Angles.GetNext(pos);
		((CAngle*)pAng)->UpdatePosition();
	}
}
    
    
BOOL CDrawObj::CanDuplicate()
{
   	return (FALSE);		// default is can't; specific classes will override
}
    
CDrawObj* CDrawObj::Clone()
{
   	// Shouldn't be called for generic drawobjs.
   	return NULL;
    
// Following code is a model for derived class implementations. A member-by-member 
// copy should be sufficient to clone instances for many  classes, but some will 
	// have to take further steps, in particular if they  contain pointers to 
   	// dynamically allocated memory.
/*
   	// create duplicate w/same properties 
   	CDrawObj* pClone = new CDrawObj(m_position);
  	pClone->m_strName = m_strName;
   	pClone->m_status = m_status;
    
   	return (pClone);
*/	
}
    
BOOL CDrawObj::OnEditProperties()	// returns T if OK'd possibly changed props
{
   	// currently no-op in base class, beef is in derived classes
   	return FALSE;
}
    
BOOL CDrawObj::CanEditProperties()
{
   	return FALSE;	// default is no props, specific classes will override
}
    
void CDrawObj::Draw(CDC* pDC)
{	
   	// No-op. Might get called by classes that don't draw themselves.
}
    
    
// render object in hidden state for eye-tracking example study mode
void CDrawObj::DrawMasked(CDC* pDC)
{
   	// default is just to draw normally. Classes for hidden objs 
   	// will override for special example mode behavior
   	Draw(pDC);
}
    
// Get color to represent the given status.
COLORREF CDrawObj::StatusColor()
{
   	if (m_pDocument) // allow for drawing objects dangling outside any document
   	{
   		// hack to use special highlight color if selected in example view.
   		CDocTemplate* pDocTemplate = (m_pDocument)->GetDocTemplate();
   		if ((m_pDocument->m_nProblemType == PROB_EXAMPLE)
   			&&(pDocTemplate!=theApp.m_ptmplExEdit))
   		{
   			CEXView* pView = theApp.GetEXView();
   			if (pView && (this == pView->m_pExpSel))
 				return (RGB(128, 0, 128));	//purple
  		}
   	}
    
   	switch (m_status) {
   	case statusError:
   		return (RGB(255, 0, 0));	// red
   
   	case statusCorrect:
   		return (RGB(0, 192, 0));	// betw dark green (128) and green (255)
   
   	default:
   		// This case happened for serialized objs since status not saved.
   		// Now fixed in deserialization, so shouldn't ever happen
   		TRACE("Status Color: Bad status value!\n"); 
   		m_status = statusUnknown;
   		// fall through...
   	case statusUnknown:
   		return (RGB(0, 0, 0));		// black
   	}
}
    
#define HANDLE_W 3		// Offset of resize handle rects from pt!!! in Device pixels
    
// Add selection highlighting to drawn object 
void CDrawObj::DrawSelectState(CDC* pDC, TrackerState state)
{
   	if (state == normal) 
   		return;
   	// else obj is selected: currently no active/inactive distinction
    
   	// Draw resize handles
   	int nHandleCount = GetHandleCount();
   	for (int nHandle = 1; nHandle <= nHandleCount; nHandle += 1) {
   		CPoint handle = GetHandle(nHandle);
   		pDC->PatBlt(handle.x - HANDLE_W, handle.y - HANDLE_W, 
   			        2*HANDLE_W + 1, 2*HANDLE_W + 1, DSTINVERT);
   	}
}
    
//
// Hit test functions. 
//
// DRAWCLI sample *only* used HitTest with the bSelected flag set to
// to test for resize handle grabs. Its ObjectAt function was implemented
// via the Intersects routine. We implement ObjectAt via HitTest.
//
// If bSelected, hit-codes start at one for the top-left
// and increment clockwise, 0 means no hit.
// If !bSelected, 0 = no hit, 1 = hit (anywhere)
//
// point is in logical coordinates
int CDrawObj::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
   	ASSERT_VALID(this);
   	
   	if (bSelected)	// checking for resize handle hit
   	{
   		ASSERT(pView != NULL);
    
   		int nHandleCount = GetHandleCount();
   		for (int nHandle = 1; nHandle <= nHandleCount; nHandle += 1)
   		{
   			// GetHandleRect returns in logical coords
   			CRect rc = GetHandleRect(nHandle,pView);
    			
   			/* DRAWCLI had:
   			if (point.x >= rc.left && point.x < rc.right &&
   				point.y <= rc.top && point.y > rc.bottom) 
   			// But we use coords in which y increases downwards) */
    
   			if (point.x >= rc.left && point.x < rc.right &&
   				point.y >= rc.top && point.y < rc.bottom)
   				return nHandle; 
   		}
   	}
   	else	// Checking for object hit
   	{	
   		/* DRAWCLI had:
   		if (point.x >= m_position.left && point.x < m_position.right &&
   			point.y <= m_position.top && point.y > m_position.bottom) 
   		// But we use coords in which y increases downwards. */
    
   		// Note not all our position rects are normalized. We use <= to
   		// enable hitting zero-width/height objects, e.g. horiz/vert lines
   		CRect normPos = m_position;
   		normPos.NormalizeRect();
   		if (point.x >= normPos.left && point.x <= normPos.right &&
   			point.y >= normPos.top && point.y <= normPos.bottom)
   			return 1;
   	}
   	return 0;
}
     
// Tests if position intersects given rect.
// Likely need to override this function to do good intersection 
// testing for some objects.
// rect must be in logical coordinates
BOOL CDrawObj::Intersects(const CRect& rect)
{
   	ASSERT_VALID(this);
    
   	CRect fixed = m_position;
   	fixed.NormalizeRect();
   	CRect rectT = rect;
   	rectT.NormalizeRect();
   	// 
   	// A rect is "empty" if either its width or height are zero. 
   	// Therefore test for non-empty intersection doesn't work for 
   	// degenerate zero-width or zero-height position rects we get for 
   	// vertical/horizontal lines for vectors and axes.
   	// So we widen these degenerate rects into proper windows rects.
   	if (fixed.left == fixed.right)
   		fixed.right += 1;
   	if (fixed.top == fixed.bottom)
   		fixed.bottom += 1;
   	return !(rectT & fixed).IsRectEmpty();
}
    
// Default bounding box is (fixed up) position rect. Objects must override if 
// that is not accurate, e.g. to account for line widths, labels.
CRect CDrawObj:: GetBoundingBox()  
{
   	CRect fixed = m_position;
   	fixed.NormalizeRect();
   	
   	// Fix up degenerate bounding rects for we get for vertical/horizontal lines
   	if (fixed.left == fixed.right)
  		fixed.right += 1;
   	if (fixed.top == fixed.bottom)
   		fixed.bottom += 1;
    	
   	return fixed;
}
    
//
// MoveTo: changes object position rect and marks object dirty.
//		   
// Like most modifiers, routine takes optional view parameter. If
// view is passed in, only invalidates in that view using pView->InvalObjInView
//
void CDrawObj::MoveTo(const CRect& position, CFBDView* pView)
{
   	ASSERT_VALID(this);
   
   	if (position == m_position)
   		return;
    
   	if (pView == NULL)
   	{
   		Invalidate();
   		m_position = position;
   		Invalidate();
   	}
   	else
   	{
   		pView->InvalObjInView(this);
   		m_position = position;
  		pView->InvalObjInView(this);
   	}
   	if (m_pDocument) m_pDocument->SetModifiedFlag();
}
    
// returns number of resize handles
int CDrawObj::GetHandleCount()
{
   	return 8;	// default is 8 around bounding boxes.
}
    
// GetHandle-- Returns location of resize handle with given index. 
// Return value is in logical coords.
CPoint CDrawObj::GetHandle(int nHandle)
{
   	ASSERT_VALID(this);
   	int x, y, xCenter, yCenter;
    
   	// Our position rects aren't all normalized. Some also rely on the orientation
   	// defined from an origin  coded in "(left, top)" into the quadrant determined by 
   	// the endpoint in "right, bottom". E.g. this determines the direction of a vector
   	// or X-axis or curved arc segment.
   	//
  	// As computed below, which handle ids correspond to which corners will vary with 
   	// orientation of the position rect. They start at the origin and move around in the
   	// order of the x direction then the y. That is OK as far as resizing goes, since the 
   	// variation in numbering is exactly compensated for by the corresponding code in
   	// the MoveHandleTo routine. But we also had to modify GetHandleCursor to
   	// understand these oriented handle ids.
    	
   	CRect normPos = m_position;		// position
   	/* normPos.NormalizeRect();	*/	
    	
   	// this gets the center regardless of left/right and top/bottom ordering
   	xCenter = normPos.left + normPos.Width() / 2;
   	yCenter = normPos.top + normPos.Height() / 2;
   
   	switch (nHandle)
   	{
   	default:
   		ASSERT(FALSE);
   
   	case 1:
   		x = normPos.left;
   		y = normPos.top;
   		break;
    
   	case 2:
   		x = xCenter;
   		y = normPos.top;
   		break;
    
   	case 3:
   		x = normPos.right;
   		y = normPos.top;
   		break;
    
   	case 4:
   		x = normPos.right;
   		y = yCenter;
   		break;
    
   	case 5:
   		x = normPos.right;
   		y = normPos.bottom;
   		break;
    
   	case 6:
   		x = xCenter;
   		y = normPos.bottom;
   		break;
    
   	case 7:
   		x = normPos.left;
   		y = normPos.bottom;
   		break;
    
   	case 8:
   		x = normPos.left;
   		y = yCenter;
   		break;
   	}
    
   	return CPoint(x, y);
}
    
// return rectangle of handle in logical coords
CRect CDrawObj::GetHandleRect(int nHandleID, CFBDView* pView)
{
   	ASSERT_VALID(this);
   	ASSERT(pView != NULL);
    
   	CRect rect;
   	// get the center of the handle in logical coords
   	CPoint point = GetHandle(nHandleID);
   	// convert to client/device coords
   	pView->DocToClient(point); 
   	// get CRect of handle in device pixel coords.
   	// Handles currently widen object by three device pixels. 
   	rect.SetRect(point.x-HANDLE_W, point.y-HANDLE_W, point.x+HANDLE_W, point.y+HANDLE_W);
   	// and convert back to logical
   	pView->ClientToDoc(rect);
    
   	return rect;
}
    
// return ID of cursor to use for given resize handle index
HCURSOR CDrawObj::GetHandleCursor(int nHandle)
{
   	ASSERT_VALID(this);
   
   	LPCTSTR id;
   	switch (nHandle)
   	{
   	default:
   		ASSERT(FALSE);
   
   	case 1:
   	case 5:
   		// signed width, height both positive or both negative means diagonal
   		// from origin in top-left is oriented down-right (SE) or up-left. (NW)
   		if (m_position.Width() * m_position.Height() >= 0)
   			id = IDC_SIZENWSE;
   		else
   			id = IDC_SIZENESW;
   		break;
   
   	case 2:
   	case 6:
   		id = IDC_SIZENS;
   		break;
    
   	case 3:
   	case 7:
   		// orientation opposite to that for handles 1
   		if (m_position.Width() * m_position.Height() >= 0)
   			id = IDC_SIZENESW;
   		else
   			id = IDC_SIZENWSE;
   		break;
    
   	case 4:
   	case 8:
   		id = IDC_SIZEWE;
   		break;
   	}
    
   	return AfxGetApp()->LoadStandardCursor(id);
}
    
    
// MoveHandleTo -- adjust position to move point specified by the given resize 
//                 handle index to the given point. 
//
// View arg used as in MoveTo, which does the work. 
// Point arg must be in logical coords.
void CDrawObj::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{
   	ASSERT_VALID(this);
   
   	CRect position = m_position;
   	switch (nHandle)
   	{
   	default:
   		ASSERT(FALSE);
   
   	case 1:
   		position.left = point.x;
   		position.top = point.y;
   		break;
   
   	case 2:
   		position.top = point.y;
   		break;
   
   	case 3:
   		position.right = point.x;
   		position.top = point.y;
   		break;
    
   	case 4:
   		position.right = point.x;
   		break;
    
   	case 5:
   		position.right = point.x;
   		position.bottom = point.y;
   		break;
    
   	case 6:
   		position.bottom = point.y;
   		break;
   
   	case 7:
   		position.left = point.x;
   		position.bottom = point.y;
   		break;
    
   	case 8:
   		position.left = point.x;
   		break;
   	}
   
   	MoveTo(position, pView);
}
    
// Mark object invalid in its document. Update w/hint sent to all views
void CDrawObj::Invalidate()
{
   	ASSERT_VALID(this);
	// May be called for dangling objects, e.g. if Move methods used on them.
	if (m_pDocument)
   		m_pDocument->UpdateAllViews(NULL,  HINT_UPDATE_DRAWOBJ, this);
}
    
// Compute drawn direction. Gets angle of directed position diagonal in integral 
// degrees  counter-clockwise from screen horizontal
int CDrawObj::GetDirection()
{
    
   	CPoint ptFrom = m_position.TopLeft();
   	CPoint ptTo = m_position.BottomRight();
	return GetDirection(ptFrom, ptTo);
  	
}

int CDrawObj::GetDirection(CPoint ptFrom, CPoint ptTo)
{
	const double pi = 3.1415926535;	// Not predefined somewhere??

	int dx = ptTo.x - ptFrom.x;
   	int dy = ptTo.y - ptFrom.y;
   	
   	// atan2 returns angle in radians in range -pi to pi.
   	// this is direction in device coordinates in which positive y
   	// is downward, hence a clockwise angle measure
   	double radiansClockwise = atan2(dy,  dx);
    
   	// convert to integral degrees in range -180 to 180 
   	int degreesClockwise = Round(radiansClockwise * (360.0/(2.0 * pi)));
    
   	// flip sign to get to conventional counter-clockwise measure for user
   	int degreesUser = - degreesClockwise;
   
   	// we use positive angles in range 0 to 360 (desirable?)
   	if (degreesUser < 0)
   		degreesUser += 360;
   
   	return(degreesUser);
}
    
// Rotate drawing to enforce given direction. 
// !!! GetDirection doesn't nec return exact same number as set here! 
void CDrawObj::SetDirection(int degreesUser, CFBDView* pView)
{
   	const double pi = 3.1415926535;
   	CPoint ptFrom = m_position.TopLeft();
   	CPoint ptTo   = m_position.BottomRight();
   	int dx = ptTo.x - ptFrom.x;
   	int dy = ptTo.y - ptFrom.y;
   	double hypo =sqrt ((dx*dx) + (dy*dy));
   	
   	int degreesClockwise = -degreesUser;	
   	double radiansClockwise = degreesClockwise* ((2.0 * pi )/(360.0));
   	double newdy =hypo * sin(radiansClockwise);
  	double newdx =hypo * cos(radiansClockwise);
    
   	CRect newPos = m_position;
   	ptTo.x = Round(newdx) + ptFrom.x;
   	ptTo.y = Round(newdy) + ptFrom.y;
  	newPos.BottomRight() = ptTo;
    
   	MoveTo(newPos, pView);

	if (!m_Angles.IsEmpty())
		UpdateAngles();
}
    
    
// 
// Helper gets a name identifying the type of the FBD object
//
// For classes like vectors and variables with tagged subtypes, this name 
// identifies the subtype, e.g. "Velocity", "Acceleration", etc.
// The name is used in diagnostics, in log entries, and to map objects from object 
// type to explain menu in EXView.
//  
// !! Unclear whether this needs to be human-friendly name (which might have spaces), 
// or internally used id (which probably shouldn't, for safety when sending to
// help system or in log entry.)
//
void CDrawObj::GetTypeName(CString& strType)
{
   	strType = GetRuntimeClass()->m_lpszClassName + 1; // class name after "C"
} 
    
//
// Return readable text describing object definition.
// should be usable in error messages. e.g "Tension Force on SB due to STRING";
CString CDrawObj::GetDef()   // base class shouldn't really be used
{
   	CString strTemp;
   	GetTypeName(strTemp);
   	return strTemp;			// default just prints type
}

// Get possible variant string to show on printout:
CString CDrawObj::GetPrintDef()   // base class shouldn't really be used
{
 	return GetDef();		// default is same as GetDef.
}

void CDrawObj::LogEntry()	// no-op, derived class must override
{
	ASSERT(FALSE);
}

BOOL CDrawObj::SetFromLogStr(LPCTSTR pszStr)	// no-op derived class must overide
{
	return FALSE;
}

CString CDrawObj::GetDrawObjLogPart()	// implementation helper for derived classes
{
	CString strTypeName;
	GetTypeName(strTypeName);
	CString strName = ValToArg(m_strName); // empty for Axes
	CString strResult;
	strResult.Format("%s %s %s %d %d %d %d %d", strTypeName, strName, m_strId, 
		m_status, m_position.left, m_position.top, m_position.right, m_position.bottom);
	return strResult;	
}

LPCTSTR CDrawObj::ParseDrawObjLogPart(LPCTSTR pszStr)
{
	char szName[80]; // may be empty for Axes
	char szId[80];

	// can skip subtype name, since only parsing generic drawobj properties
	if (sscanf(pszStr, "%*s %s %s %d %d %d %d %d", szName, szId, &m_status, 
		&m_position.left, &m_position.top, &m_position.right, &m_position.bottom) != 7)
		return NULL;
	if (! (0 <= m_status && m_status <= 2)) return NULL;

	m_strId = szId;
	m_strName = ArgToVal(szName);

	// return ptr to unparsed rest, i.e. just after the eighth space
	LPCSTR pszRest = pszStr;
	LPCSTR pSp;
	for (int nSp = 1; nSp <= 8; nSp++) {
		pSp = strchr(pszRest, ' ');
		if (pSp == NULL) return NULL;
		pszRest = pSp + 1;
	}
	return pszRest;
}


CString CDrawObj::GetLabelPrefix()
{
	return "";
}

BOOL CDrawObj::IsValid()
{
	
	CString str;
	CFBDDoc* pDoc = m_pDocument;//((CFBDDoc*)theApp.GetDocument());

	// check for existing drawn object with same definition
	CDrawObj* pObj = pDoc->GetMatchingObj(this, FALSE);//bMatchDef
	if (pObj != NULL)
	{
		str.Format(IDS_SAMEDEF_OBJ, pObj->GetDef(), pObj->m_strName);
		theApp.DoInfoMessage(str);
		return FALSE;
	}
	// Check if name in use for existing drawn object
	pObj = pDoc->GetMatchingObj(this, TRUE);//bMatchName
	if (pObj != NULL){
		CString def = pObj->GetDef();
		/*if ( (pObj->IsKindOf(RUNTIME_CLASS(CSystem))) && (m_strName[0] != 'm') )
			def = "body" + def.Mid(7); */
		str.Format(IDS_LABEL_NOTUNIQUE, m_strName, def);
		theApp.DoInfoMessage(str);
		return FALSE;
	}

	// Check if name in use for predefined variable (time point)
	CString strDef = pDoc->GetMatchingPredef(m_strName);
	if (! strDef.IsEmpty()) {
		str.Format(IDS_LABEL_PREDEFINED, m_strName, strDef);
		theApp.DoInfoMessage(str);
		return FALSE;
	}

	// Check if name in use for defined variable
	CVariable* pVar = pDoc->GetMatchingVar(this, TRUE/*bMatchName*/);
	if (pVar != NULL){//New variable compared to list of drawn objects
		if (HasSameDef(pVar)){
		/*	CFBDDoc* pDoc = ((CFBDDoc*)theApp.GetDocument());
			if (pDoc != NULL)
			{
				pDoc->UpdateAllViews(NULL, HINT_DELETE_VARIABLE, pVar);
				pDoc->RemoveVariable(pVar);
			}
			pVar->NotifyDelete();
			pVar->Delete();*/
			return TRUE;
			//if draw an object that has originally been declared as
			//a variable, delete the unnecessary variable
		}
		else{
			str.Format(IDS_LABEL_NOTUNIQUE, m_strName, pVar->m_strDef);
			theApp.DoInfoMessage(str);
			return FALSE;
		}
	}

	// Check if same quantity already defined by variable
	pVar = pDoc->GetMatchingVar(this, FALSE);//bMatchDef
	if (pVar != NULL)
	{
		str.Format(IDS_REDEFINE_OBJ, pVar->m_strName, pVar->m_strDef);
		theApp.DoInfoMessage(str);
		return FALSE;
	}
	return TRUE;
}

BOOL CDrawObj::HasSameName(CDrawObj* pObj)
{
	//Check object list to see if we already have drawn an object with the
	//same label

	if (pObj->m_strName.IsEmpty())	//and unlabelled objects
		return FALSE;

	CString strName = pObj->m_strName;

	if (strcmp(strName, m_strName) == 0){//AW: no longer case sensitive
	//if same name, check if different id
		if (strcmp(m_strId, pObj->m_strId)!=0)  // not same obj [as one being edited].
			return TRUE;
	}
/*
	if (pObj->IsKindOf(RUNTIME_CLASS(CSystem))){//check system mass label
		strName = "m" + strName;
		if (_stricmp(strName, m_strName) == 0){//case insensitive
			//if same name, check if same id
			if (strcmp(m_strId, pObj->m_strId)!=0)
				return TRUE;
		}
	}
*/

	return FALSE;

}
 
/*   
BOOL CDrawObj::HasSameName(CVariable* pVar)
{
	if (_stricmp(m_strName, pVar->m_strName) == 0){//case insensitive
	//if same name, check if same id
		if (strcmp(m_strId, pVar->m_strId)==0)
			return FALSE;//we are editing this variable
		else
			return TRUE;
	}
	return FALSE;
}

*/

BOOL CDrawObj::HasSameDef(CDrawObj* pObj)
{
	return FALSE;
}


BOOL  CDrawObj::HasSameDef(CVariable* pVar)
{
	return FALSE;
}

BOOL  CDrawObj::HasSameDir(CDrawObj* pObj)
{
	return TRUE;
}

void CDrawObj::SetSelectedPart(int hit)
{
	return;
}

int CDrawObj::GetSelectedPart()
{
	return 1;
}


int CDrawObj::GetHit()
{
	return 1;
}


CPoint CDrawObj::GetBtnPos(CRect btnPos)
{
	return CPoint(0, 0);
}

void CDrawObj::UpdateObj(CDrawObj* pObj)
{

	return;
}

/////////////////////////////////////////////////////////////////////////////////
//
// Group objects: these just batch up a bunch of objects in a list.
// m_position saves bounding box of group.
// Operations iterate over the list, possibly recursively.
//
///////////////////////////////////////////////////////////////////////////////// 
    
IMPLEMENT_SERIAL(CGroup, CDrawObj, VERSIONABLE_SCHEMA | 1);
    
CGroup::CGroup()
   	:CDrawObj(CRect(0, 0, 0, 0)) // construct w/null initial position rect
{	
   	// default constructor inits empty object list.
}
    
void CGroup::AddObj(CDrawObj* pObj) // add object to group & update bounds
{
   	m_objects.AddTail(pObj);
    	
   	// update position bounding box to include new object
   	CRect rcAdd = pObj->GetBoundingBox();
   	rcAdd.NormalizeRect();
   	m_position |= rcAdd;	// does a UnionRect into m_position. 
}
    
void CGroup::Delete()		// destroy a group object and free all its memory
{
   	// iterate to unlink and destroy each contained object
   	while (! m_objects.IsEmpty()) {
   		CDrawObj* pObj = m_objects.RemoveHead();
   		pObj->Delete();
   	}
   	// following does a "delete this" to self-destruct. 
   	CDrawObj::Delete();
}
    
void CGroup::Serialize(CArchive& ar) 
{
   	// Save class version over possible call to serializeClass
   	UINT nVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   
   	// check document version
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
   	// document versions >= 1 added serialization of base class descriptor
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	}
   	CDrawObj::Serialize(ar);
    
   	// currently just serialize embedded object list.
   	m_objects.Serialize(ar);
}
    
#ifdef _DEBUG
void CGroup::Dump(CDumpContext& dc) const
{
   	CDrawObj::Dump(dc);
   
   	dc << m_objects.GetCount() << " Members: {\n";
   	POSITION pos = m_objects.GetHeadPosition();
   	int nObj = 0;
   	while (pos != NULL)
   	{
   		CDrawObj* pObj = m_objects.GetNext(pos);
   		dc << "\n[" << ++nObj << "] ";
   		if (pObj == NULL)
   			dc << "NULL item in object list!!!\n" ;
   		else 
   			pObj->Dump(dc);
   	}
   	dc << "} End Group\n";
}
#endif
    
void CGroup::Draw(CDC* pDC)
{
   	POSITION pos = m_objects.GetHeadPosition();
   	while (pos != NULL) {
   		CDrawObj* pObj = m_objects.GetNext(pos);
   		pObj->Draw(pDC);
   	}
}
    
void CGroup::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{
   	// Not implemented. Could try to scale every object in group by the
   	// same proportion as the whole group. But since different objects use
   	// m_position and the handles differently, its not just a matter of using 
   	// MoveHandleTo for each one. Would need a method on each to scale 
   	// bounding box by same ratio and direction.
}
    
void CGroup::MoveTo(const CRect& rcNew, CFBDView* pView)
{
   	// Move each of the group members by offset from current pos
   	CPoint delta = (CPoint) (rcNew.TopLeft() - m_position.TopLeft());
    
   	POSITION pos = m_objects.GetHeadPosition();
   	while (pos != NULL) 
   	{
   		CDrawObj* pObj = m_objects.GetNext(pos);
   		ASSERT(pObj != NULL);
   		pObj->MoveTo(pObj->m_position + delta, pView);
   	}
    
   	// update whole group object's position itself
   	CDrawObj::MoveTo(rcNew, pView);
}
    	
BOOL CGroup::Intersects(const CRect& rect)
{
   	// See if it intersects any sub-object
   	POSITION pos = m_objects.GetHeadPosition();
   	while (pos != NULL){
  		CDrawObj* pObj = m_objects.GetNext(pos);
  		if (pObj->Intersects(rect))
   			return TRUE;
   	}
   	return FALSE;
}
    
int CGroup::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{	
   	// don't allow grab handle hit, since resizing not implemented
   	if (bSelected) return 0;
    
   	// See if point hit any sub-object
   	POSITION pos = m_objects.GetHeadPosition();
   	while (pos != NULL) {
   		CDrawObj* pObj = m_objects.GetNext(pos);
   		if (pObj->HitTest(point, pView, bSelected))
   			return TRUE;
   	}
   	return FALSE;
}
    
BOOL CGroup::CanDuplicate() { return TRUE; }
    
CDrawObj* CGroup::Clone()
{
   	// create new group object: 
   	CGroup* pCloneGroup = new CGroup();
   
   	// loop to clone each member in turn and add to new group object
   	// Members' clone method will take care of offsetting position for visibility
   	POSITION pos = m_objects.GetHeadPosition();
   	while (pos != NULL) 
   	{
  		CDrawObj* pObj = m_objects.GetNext(pos);
   		// Duplicate this 
		ASSERT(m_pDocument != NULL);
   		CDrawObj* pCloneMember = pObj->Clone();
   		if (pCloneMember == NULL)		// clone failed! ignore
   			continue;
   		pCloneGroup->AddObj(pCloneMember); 
   	}
/*  // make sure new group contains at least one member
    	if (pCloneGroup->m_objects.GetCount() < 1) {
    		delete pCloneGroup;
    		return NULL;
    	} */// for now, allow add of empty groups, since not sure callers handle null
    	
    
   	return (pCloneGroup);
}
    
//////////////////////////////////////////////////////////////////////////
//
//	Andes Physics Diagram objects
//
//////////////////////////////////////////////////////////////////////////
    
// 
// CCheckedObj: Base class for objects that can be entered by students encapsulates 
//              methods for checking diagram objects w/help system
//              
//
IMPLEMENT_DYNAMIC(CCheckedObj, CDrawObj)
   
CCheckedObj::CCheckedObj() : CDrawObj() {} // default constructor required by serialization
CCheckedObj::CCheckedObj(const CRect& position) :
   CDrawObj(position)
{
    	// this just to pass initializer param on up to CDrawObj
}
    
// CheckObject: Have object check its current state w/the help system, updating
//              status with result.
// Expected to be used to define a new object to the help system, but also in the
// default protocol for notifying the help system on changes in NotifyChange below.
//
// No-op in base class declaration, since depends on type.
    
// SplitResult: split help system result into return value and command
//
// We allow any status return value from a help system call to piggyback a command 
// string for the workbench to execute. This is mainly used by the conceptual helper 
// to send the command to popup a mini-lesson or rule query dialog.
// The command follows an optional ! in the returned string. Lisp DDE also
// renders string values like a Lisp printer, with quote marks.
// This routine splits the string into the two parts, leaving empty strings
// if part is not found.
void CCheckedObj::SplitResult(LPCTSTR pszResult, CString& strReturn, CString& strCmd)
{
   	if (! pszResult) 
   		return;		// assumes strings are already empty
    
   	// First extract result proper from surrounding quotes added
   	// by Lisp to returned strings (though not to T or NIL return vals).
   	CString strRawResult(pszResult);
   	CString strResult;
//   	if (strRawResult[0] == '\"')	// strip quotes added by Lisp
// 		strResult = strRawResult.Mid(1, strRawResult.GetLength() - 2);
// 	else
  	strResult = strRawResult;
   
   	// Check for optional "!" separating return value from attached command
   	int nSepPos = strResult.Find('!');
   	if (nSepPos != -1) {		// has command
   		strReturn = strResult.Left(nSepPos);
   		strCmd = strResult.Mid(nSepPos + 1); 
   	}else
   		strReturn = strResult;
	
}

void CCheckedObj::ParseResult(CString& strResult, CStringList& strErrs)
{
	CString strBuf;
//Check for optional ; list of errors in dialogs
	int nSepPos = strResult.Find(';');
   	if (nSepPos != -1) {		// has error list
   		strBuf = strResult.Mid(nSepPos + 1); 
   		strResult = strResult.Left(nSepPos);
   	}else{
		return;
	}

	char* buf = (char*) malloc(strlen(strBuf) + 1);	
	strcpy(buf, strBuf);
	char seps[]   = ";";
	char *token;
	// Non re-entrant strtok tokenizes in place by inserting NULs into buf
	token = strtok( buf, seps ); 
	while( token != NULL )
	{
		strBuf = token;
		strErrs.AddTail(strBuf);
		token = strtok( NULL, seps );	//	returns ptr to next token in buf
	}
	free((void*)buf);
}

// 
// ApplyStatus: Worker routine translates the status result from a check and sets
// it into a specified status variable; then executes any attached command from the
// help system. 
//
// This routine is static and uses the reference variable so it can be used by 
// non-Drawobjs, e.g. for equations or variables, to implement this common task.
// Note it does not invalidate the object after setting status (since doesn't know how).
// (!!! could return boolean set iff status changed).
// 
void CCheckedObj::ApplyStatus(LPCTSTR pszResult, Status& rStatus, CStringList& strErrs)
{
	strErrs.RemoveAll();
   	if (! pszResult) {		// failed to get result
   		rStatus = statusUnknown;
   		return;
   	}
   	CString strReturn, strCommand;
   	SplitResult(pszResult, strReturn, strCommand);
	ParseResult(strReturn, strErrs);
   
   	// set status from return value
   	if (strReturn.IsEmpty())
		rStatus = statusUnknown;
	else if (strReturn == "T") {
		rStatus = statusCorrect;
		theApp.MakeSound(sndCorrectEntry);
	}
	else {
		rStatus = statusError;
		theApp.MakeSound(sndErrorEntry);
	}
   		
	// if got a command, pass it to application's command interpreter
   	if (!strCommand.IsEmpty()) 
   		ExecuteCmd(strCommand);
}
    
void CCheckedObj::ApplyStatus(LPCTSTR pszResult)
{ 
	Status oldStatus = m_status;
	// Translate Lisp result into new status
	// Note piggybacked command might execute here!
	ApplyStatus(pszResult, m_status, m_errors);
	
	//Because I am now calling the check-object on the temporary object within the dialog,
	//I do not want to Invalidate this dialog (ie send an update hint for it)
  /*// redraw if status changed.
	if (m_status != oldStatus) 
 		Invalidate(); */
}
 
// Tell help sys to delete info indexed via original name (possibly diff from current)
// Used by default NotifyChange protocol to handle change notifications by deleting old 
// entry and defining new one with changed props. Should also be harmless if object
// not defined. (!!! Name may not matter anymore now that we use obj Ids in all calls)
void CCheckedObj::NotifyDelete(CString& strOldName)
{
   	// Default API is delete-object, override if different
   	if (! (strOldName.IsEmpty() && m_strId.IsEmpty()) ) // must have either label or ID
   		(void) HelpSystemSendf("(delete-object %s %s)", 
   							    STR2ARG(strOldName), STR2ARG(m_strId));
}
    
// Notify help system that object properties have changed
// OldName argument is defined name, since change may have affected label
void CCheckedObj::NotifyChange(CString& strOldName)
{
   	// Default is to handle changes by deleting old data and checking current
 //	NotifyDelete(strOldName);
   	CheckObject();
}
    
// Shorthand to use when object has simply been deleted 
void CCheckedObj::NotifyDelete()
{
	RemoveVarNames(m_strName);
   	NotifyDelete(m_strName);
}
    
// This implements standard protocol for property editing command:	
// returns T iff user OK'd possibly changed properties.	
BOOL CCheckedObj::OnEditProperties()
{
   	// Save name over possible changes
   	CString strOldName = m_strName;
    
	// Have object init a property editing dialogue. Dialog is dynamically allocated
 	// by object because it knows the type and how to init it with properties.
   	CDialog* pDlg = GetPropertyDlg();
   	if (pDlg == NULL)
   		return FALSE;
	if (! pDlg->IsKindOf(RUNTIME_CLASS(CDrawObjDlg))) {
		TRACE("OnEditProperties: dialog is not a CDrawObjDlg!\n");
		delete pDlg;
		return FALSE;
	}
	// run the dialog. If OK'd, dialog transfers possibly changed props into the object's 
	// members without notifying anyone.
	
	// When editing, status is unknown until rechecked
/*	if (m_status != statusUnknown) {
		m_status = statusUnknown;
		Invalidate();
	}*/

	// Run like modal dialog, but with hint window enabled. Needed to allow for detecting
	// errors while dialog is up and showing messages in hint windows with followups allowed.
	int nResult = ((CDrawObjDlg*)pDlg)->DoModalWithHints();

   	delete pDlg;
    
   	if (nResult != IDOK)	// user cancelled dialog
	{//do not want to check Newly created objects on Cancel, need to put this elsewhere
	//	CheckObject();
   		return FALSE;
	}
	// else OK'd possibly different props: must treat as change operator on object state
   	
   	// Changing props makes document dirty:
   	if (m_pDocument != NULL) 
   		m_pDocument->SetModifiedFlag();
    
  	// invalidate object in case change has visible effect (e.g. label, status)
   //Invalidate();

//*****Now we are checking in dialog
	// Now check updated asserted properties with help system. This may update status
//	NotifyChange(strOldName);
	
	// Moved this from above because removed invalidate call from CheckObject.
	// We are now checking a temporary object, and we don't want to send a update call
	// with our temp object as the hint.
	// So need to invalidate after object's status changed.
  	// invalidate object in case change has visible effect (e.g. label, status)
    Invalidate();
	UpdateVarNames(strOldName);

   	return TRUE;
}

void CCheckedObj::UpdateVarNames(CString strOldName)
{
	if (strOldName.IsEmpty())
		strOldName = m_strName;

	RemoveVarName(strOldName);

	((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(m_strName);
}

void CCheckedObj::RemoveVarName(CString strName)
{
	if (m_pDocument)
	{
		if (m_pDocument->m_strVarNames.IsEmpty())
			return;
		POSITION pos = m_pDocument->m_strVarNames.Find(strName);
		if (pos != NULL)
			m_pDocument->m_strVarNames.RemoveAt(pos);
	}

}

void CCheckedObj::RemoveVarNames(CString strName)
{
	RemoveVarName(strName);

}
//////////////////////////////////////////////////////////////////
// "Labels" = Text blocks. 
//
// These are possibly multi-line blocks of text, sized to enclose the
// text. The name "Label" is a misleading name, they can be used 
// arbitrarily, e.g. for problem text, not just to label things. In fact 
// they are not linked to objects at all as a label would be. But
// the type name CLabel is in all the document files so can't change it.
//
// Flags are relevant only in example mode to
// distinguish between visible and masked text, also to allow use of
// label objects to represent author-defined masked regions.
// The former use is obsolete given new example text items; the
// latter is really just a hack we did for quick and dirty implementation, 
// but is still being used.
//////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CLabel, CDrawObj, VERSIONABLE_SCHEMA | 2)
CLabel::CLabel() {}	// used by deserialization only, init all on load

/* not used  
CLabel::CLabel(const CRect& position)
    	:CDrawObj(position)
{
    	// following used for example mode text
    	m_bVisible = FALSE;
    	m_bMask = FALSE;
    	m_bHint = FALSE;	
}
*/

// create new given top left, text, and font
CLabel::CLabel(const CPoint& ptTopLeft, LPCSTR pszText, LOGFONT* pLogFont)
		// dummy extent is fixed up below. 
    	:CDrawObj(CRect(ptTopLeft, CSize(1,1)), pszText)
{
    	// following used only for example mode text and masks
    	m_bVisible = FALSE;
    	m_bMask = FALSE;
    	m_bHint = FALSE;

		if (pLogFont) {
			m_font.CreateFontIndirect(pLogFont);
		} else {
			LOGFONT lfText;
			CFBDDoc::GetDefaultFont(&lfText);
			m_font.CreateFontIndirect(&lfText);
		}

		RecalcExtent();
}
    
    
void CLabel::Serialize(CArchive& ar)
{
    	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
    	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
    	// document versions >= 1 added serialization of base class descriptor
    	if (nDocVersion >= 1) {
    		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
    	}
    
    	CDrawObj::Serialize(ar);
    	if (ar.IsStoring()){
    		// store object specific data
    		ar << m_bVisible;
    		ar << m_bMask;
			// added version 2:			
			SerializeFontInfo(ar);

    	} else {
    		// load object specific data
    	
    		if (nClassVersion >= 1) {
    			ar >> m_bVisible;
    			ar >> m_bMask;
				if (nClassVersion >= 2)
					SerializeFontInfo(ar);
				else
					m_font.CreatePointFont(120, "Arial");
    		}
    		else // else some earlier version
    		{
    			m_bVisible = FALSE;
    			m_bMask = FALSE;
    		}
    	}
    	m_bHint = FALSE;
}

// helper moves between cached font and serialized logfont info   
void CLabel::SerializeFontInfo(CArchive& ar)
{
	LOGFONT logfont;
	
	if (ar.IsStoring())
	{
		m_font.GetLogFont(&logfont);
		ar.Write(&logfont, sizeof(logfont) - sizeof(logfont.lfFaceName));
		// lfFaceName is stored as CString so it is UNICODE/ANSI independent
		ar << CString(logfont.lfFaceName);
		ar << logfont.lfHeight;
		ar << logfont.lfWeight;

	}
	else
	{
		ar.Read(&logfont, sizeof(logfont) - sizeof(logfont.lfFaceName));
		// lfFaceName must be read as a CString
		CString strFaceName;
		ar >> strFaceName;
		lstrcpy(logfont.lfFaceName, strFaceName);
		ar >> logfont.lfHeight;
		ar >> logfont.lfWeight;
		m_font.CreateFontIndirect(&logfont);
	}
}
   
    #ifdef _DEBUG
void CLabel::Dump(CDumpContext& dc) const
{
    	CDrawObj::Dump(dc);
    	// dc << "Name: " << m_strName << "\n";
}
    #endif // _DEBUG
    
void CLabel::Draw(CDC* pDC)
{
    	/*
    	pDC->TextOut(m_position.left, m_position.top, m_strName);
    	*/
    	
    	if (! m_bMask) 
    	{
    		// First call updates m_position, just to make sure
			// it encompasses text. Shouldn't be needed if RecalcExtent is
			// correctly used on any change, but can't hurt to be safe.
			CFont* pOldFont = pDC->SelectObject(&m_font);
    		pDC->DrawText(m_strName, m_position, DT_CALCRECT | DT_EXPANDTABS);
    		// Draw text in transparent mode
    		int oldBkMode = pDC->SetBkMode(TRANSPARENT);
    		
			// Draw using Greek Renderer, in case $-coded symbols in text
			// pDC->DrawText(m_strName, m_position, DT_LEFT);	
			CGreekText::DrawText(pDC, m_position, m_strName);

    		pDC->SetBkMode(oldBkMode);
			pDC->SelectObject(pOldFont);	
    	}
    	else	// a masked region for use in example study mode
    	{
    		// drawing a masked region 
    		// actually means to show all the objects formerly hidden beneath it.
    		// so we do nothing here.
    
    		// But if drawing into an FBDView for editing, should show mask so author
    		// can see where it is.
    		if (theApp.m_bAuthorMode) 
    		{
    			// Show authors the boundaries in some light mode
    			CPen penDashed(PS_DASH, 1, RGB(192, 192, 192));
    			CPen* pOldPen = pDC->SelectObject(&penDashed);
    			pDC->Rectangle(m_position);	// draws default white background
    			COLORREF oldColor = pDC->SetTextColor(RGB(192, 192, 192));
    			pDC->DrawText(m_strName, 7, m_position, DT_LEFT);
    			pDC->SetTextColor(oldColor);
    			pDC->SelectObject(pOldPen);
    		}
    	}
}
    
void CLabel::DrawSelectState(CDC* pDC, TrackerState state)
{
    	// for now, a box w/o resize handles (they obscure text and
    	// labels sized to fit.
    	if (! m_bMask)
    		pDC->DrawFocusRect(m_position);
    	else
    		// masks get resize handles so author can adjust size
    		CDrawObj::DrawSelectState(pDC, state);
/*
    	CPen penBorder(PS_DOT, 1, RGB(0,0,0));
    	CPen* pOldPen = pDC->SelectObject( &penBorder );
    	CGdiObject*  pOldObj = pDC->SelectStockObject(HOLLOW_BRUSH);
    	pDC->Rectangle(m_position);
    	pDC->SelectObject(pOldPen);
    	pDC->SelectObject(pOldObj);
    */
}
    
// Does most of the work for drawing in example mode
void CLabel:: DrawMasked(CDC* pDC)
{
   	// Visible text and masks currently invisible are drawn as normal:
   	if ((m_bMask && ! m_bVisible) 
   		 || (! m_bMask && m_bVisible) ) {
   		Draw(pDC);
   		return;
   	}
    
   	// else have hidden item to mask
   	CBrush brushMask;
   	if (theApp.m_nMaskMode == MM_LIGHTEN) // lighten so hard too read 
   	{  
   		// first draw the text
   		if (! m_bMask)
   			Draw(pDC);
   		// then lighten
   		CPen penMask;
   		int nGreyLevel = theApp.m_nGreyLevel;
   		COLORREF color = RGB(nGreyLevel, nGreyLevel, nGreyLevel);
   		(void) brushMask.CreateSolidBrush(color);
   		(void) penMask.CreatePen(PS_SOLID, 1, color);
   		// This method gotten by experiment.
   		// merges in filled rectangle with pixels on screen to lighten?
   		int oldROP = pDC->SetROP2(R2_MERGEPEN);
   		CBrush* pOldBrush = pDC->SelectObject(&brushMask);
   		CPen* pOldPen = pDC->SelectObject(&penMask);
   		pDC->Rectangle(m_position);
   		pDC->SelectObject(pOldBrush);
   		pDC->SelectObject(pOldPen);
   		pDC->SetROP2(oldROP);
   	}
   	else	// hide object by drawing opaque mask in front
   	{
   		COLORREF maskColor;
   		if (m_bHint)
			maskColor = RGB(225, 192, 225);//lilac/pink
   		else 
			maskColor = RGB(225, 225, 225);//light gray
    	(void) brushMask.CreateSolidBrush(maskColor);
    	int oldROP = pDC->SetROP2(R2_COPYPEN); 
    	/* int oldBkMode = pDC->SetBkMode(OPAQUE); */
    	CBrush* pOldObj = pDC->SelectObject(&brushMask);
    	pDC->Rectangle(m_position); 
    	pDC->SelectObject(pOldObj);
    	pDC->SetROP2(oldROP);
    	/* pDC->SetBkMode(oldBkMode); */
/*  Old method: 
    	//  draw grey-filled rectangle
    	COLORREF oldBkColor = pDC->GetBkColor();
    	pDC->FillSolidRect(m_position, RGB(192,192,192));
    	pDC->SetBkColor(oldBkColor); */	
    }  
}
    
BOOL CLabel::OnEditProperties()
{
   	CLabelDlg dlg;
	
	// Edit existing properties with dialog
   	dlg.m_strText = m_strName;
   	dlg.m_bVisible = m_bVisible;
   	dlg.m_bMask = m_bMask;
	m_font.GetLogFont(&dlg.m_logFont);
   	if (dlg.DoModal() != IDOK)
   		return FALSE;

    // update text and flags with new values:
   	m_strName = dlg.m_strText;
  	m_bMask = dlg.m_bMask;
	m_bVisible = m_bMask? TRUE : dlg.m_bVisible;
	// N.B.: update cached font in case it was changed
    m_font.DeleteObject();
	m_font.CreateFontIndirect(&dlg.m_logFont);
	
	// doc now dirty
	ASSERT(m_pDocument != NULL);
   	m_pDocument->SetModifiedFlag();

   	// This operation may involve a resize since text has changed.  
	// Mark the old position as dirty...
	Invalidate();
	// ...calcuate the new extent, updating m_position width/height
	RecalcExtent();
	// ... and mark the new position dirty as well
   	Invalidate();

   	return TRUE;
}

// update width, height in m_position from text and font
void CLabel::RecalcExtent()
{
	CClientDC dc(NULL);
	CFBDDoc::SetLogicalUnits(&dc);
	CFont* pOldFont = dc.SelectObject(&m_font);
	(void) dc.DrawText(m_strName, m_position, DT_CALCRECT|DT_EXPANDTABS); 
	dc.SelectObject(pOldFont);
}

//////////////////////////////////////////////////////////////////////////
//
// HyperLinks 
// 
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CHyperLnk, CDrawObj, VERSIONABLE_SCHEMA | 1)
CHyperLnk::CHyperLnk() {}					// used by serialization only
    
CHyperLnk::CHyperLnk(const CRect& position, LPCSTR pszText)
   	:CDrawObj(position, pszText)
{
	m_posBeg = 0;
	m_posEnd = 0;
   	
}

void CHyperLnk::SetLinkType(char tag)
{
	switch(tag) 									
	{
		case 'd': case 'D': 
			m_nType = ID_POPUP;
			break;
		case 'l': case 'L':
			m_nType = ID_JUMP;
			break;
		case 'h': case 'H':
			m_nType = ID_HELPCALL;
			break;
		default:
			TRACE("Hyperlnk::SetLinkType -- Unknown tag %c\n", tag);
			break;
	}
}
 
void CHyperLnk::Serialize(CArchive& ar)
{
   	// If loading: save class version over SerializeClass
   	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
    	
   	// document versions >= 1 added serialization of base class descriptor
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	}
   	CDrawObj::Serialize(ar);
   	//
   	// We don't call CLabel base class because may be getting rid of these
   	// fields in future (really only for example study).
   	/* CLabel::Serialize(ar); */
   	if (ar.IsStoring())
   	{
		ar << (DWORD) m_nType;
		ar << m_strLink;
		
   	} 
   	else 
   	{
		DWORD wTemp;
   		ar >> wTemp; m_nType = (int)wTemp;
		ar >> m_strLink;
	
   	}
	SerializeFontInfo(ar);
   	
}

void CHyperLnk::SerializeFontInfo(CArchive& ar)
{
	LOGFONT logfont;
	

	if (ar.IsStoring())
	{
		m_font.GetLogFont(&logfont);
		ar.Write(&logfont, sizeof(logfont) - sizeof(logfont.lfFaceName));
		// lfFaceName is stored as CString so it is UNICODE/ANSI independent
		ar << CString(logfont.lfFaceName);
		ar << logfont.lfHeight;
		ar << logfont.lfWeight;

	}
	else
	{
		ar.Read(&logfont, sizeof(logfont) - sizeof(logfont.lfFaceName));
		// lfFaceName must be read as a CString
		CString strFaceName;
		ar >> strFaceName;
		lstrcpy(logfont.lfFaceName, strFaceName);
		ar >> logfont.lfHeight;
		ar >> logfont.lfWeight;
		m_font.CreateFontIndirect(&logfont);

	}

}

    
#ifdef _DEBUG
void CHyperLnk::Dump(CDumpContext& dc) const
{
   	CDrawObj::Dump(dc);
   	dc << "Name: " << m_strName << "Type: " << m_nType << "Link: " << m_strLink << "\n";
}
#endif // _DEBUG

void CHyperLnk::Draw(CDC* pDC)
{
   	//Set font to Underline
	LOGFONT logFont;
	m_font.GetLogFont(&logFont);
	logFont.lfUnderline = TRUE;
	m_font.DeleteObject();//delete any old font
	m_font.CreateFontIndirect(&logFont);
   	// First call updates position in case text was changed by
   	// editing properties.
	CFont* pOldFont = pDC->SelectObject(&m_font);
	
   	pDC->DrawText(m_strName, m_position, DT_CALCRECT);
	
    COLORREF oldColor;
	if (m_nType == ID_POPUP)//popup definitions are blue
		oldColor = pDC->SetTextColor(RGB(0, 0, 128));
	else // if (m_nType == ID_JUMP)//links to lessons are purple
		oldColor = pDC->SetTextColor(RGB(128, 0, 128));
	// else if (m_nType == ID_HELPCALL) // help button links are "teal"
	// olddColor = pDC->SetTextColor(RGB(0, 128, 128));  
 
	// Draw text in transparent mode
   	int oldBkMode = pDC->SetBkMode(TRANSPARENT);
    pDC->DrawText(m_strName, m_position, DT_LEFT);
     	
	pDC->SelectObject(pOldFont);
    pDC->SetTextColor(oldColor);
	pDC->SetBkMode(oldBkMode);
    
}

int CHyperLnk::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
	if ((bSelected)) // testing for resize handle grab: 
		return CDrawObj::HitTest(point, pView, bSelected);//handle it in base class
	// see if hit hyper rect
	if (m_position.PtInRect(point))
		return 1;
	return 0;
    
}
void CHyperLnk::HyperActivate(CPoint point, CView* pView)
{
	if (m_nType == ID_JUMP)			// minilesson "jump"
	{
		theApp.ShowLesson(m_strLink);
	}
	else if (m_nType == ID_POPUP)	// popup glossary definition
	{	
		if (pView->IsKindOf(RUNTIME_CLASS(CHintView)))
			((CHintView*)pView)->PopupDef(m_strLink, point);
		else if (pView->IsKindOf(RUNTIME_CLASS(CChatView)))
			((CChatView*)pView)->PopupDef(m_strLink, point);
		// else what? not contained in one of our hint views
	} 
	else if (m_nType == ID_HELPCALL) // helpsys request
	{ 
		// delegate to mainframe to dispatch command by helpsys name
		CMainFrame* pMainFrame = theApp.GetMainFrame();
		if (pMainFrame) pMainFrame->DoHelpRequest(m_strLink);	
	}
}

BOOL CHyperLnk::OnEditProperties()
{
	CHypertxtDlg dlg(this);
   	if (dlg.DoModal() != IDOK)
   		return FALSE;
	//else get properties from dialog
	m_font.DeleteObject();
	m_font.CreateFontIndirect(&dlg.m_logFont);
	m_nType = dlg.m_nHyperType;
	if (m_nType == ID_POPUP)
		m_strLink = dlg.m_strDef;
	else // ID_JUMP or ID_HELPCALL
		m_strLink = dlg.m_strLink;
	m_strName = dlg.m_strText;

	// need a DC to recalculate size from text!
   	// Could just set a flag to show its invalid, but for now
   	// we just recalculate every time before we draw.
   	ASSERT(m_pDocument != NULL);
   	m_pDocument->SetModifiedFlag();
   	Invalidate();
   	return TRUE;

   
}
    
//////////////////////////////////////////////////////////////////////////
//
// Example mode text items. Note a subclass of text labels, mainly for 
// testing in EXView.
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CEXText, CLabel, VERSIONABLE_SCHEMA | 3)
CEXText::CEXText() {}					// used by serialization only
    
CEXText::CEXText(const CRect& position, LPCSTR pszText)
   	:CLabel(CPoint(position.left, position.top), pszText)
{
   	m_bMask = FALSE;
}
    
void CEXText::Serialize(CArchive& ar)
{
   	// If loading: save class version over SerializeClass
   	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
    	
   	// document versions >= 1 added serialization of base class descriptor
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	}
   	CDrawObj::Serialize(ar);
   	//
   	// We don't call CLabel base class because may be getting rid of these
   	// fields in future (really only for example study).
   	/* CLabel::Serialize(ar); */
   	if (ar.IsStoring())
   	{
   		// CLabel class members
   		ar << m_bVisible;
   		ar << m_bMask;
   		// CEXText class members, version 1
   		ar << m_strMenu;
   		// added in version 2: CEXText-specific author-specified ID.
   		// removed in version 3 to use generic CDrawObj ID field for this
   		/* ar << m_strId; */
   	} 
   	else 
   	{
   		// CLabel base class members
   		ar >> m_bVisible;
   		ar >> m_bMask;
   		// CEXText class members, version 1
   		ar >> m_strMenu;
   		// added in version 2: optional author-specified ID for EXText items only
   		// taken out of EXText obj v.3 to use generic DrawObj ID field for this.
   		// Version 2 classes may have two IDs, the one generated in DrawObj base class,
   		// plus one specified by author. Here we update the drawobj ID to use the 
   		// author's in preference, if it was specified.
   		if (nClassVersion == 2) {
   			CString strAuthorsId;
   			ar >> strAuthorsId;	// not part of class data anymore
   			if (! strAuthorsId.IsEmpty() )
   				m_strId = strAuthorsId;
   		}
   	}
   	m_bHint = FALSE;
}
    
#ifdef _DEBUG
void CEXText::Dump(CDumpContext& dc) const
{
   	CDrawObj::Dump(dc);
   	// dc << "Name: " << m_strName << "\n";
}
#endif // _DEBUG
    
BOOL CEXText::OnEditProperties()
{
   	CEXTextDlg dlg;
   	dlg.m_strText = m_strName;
   	dlg.m_bVisible = m_bVisible;
   	dlg.m_strMenu = m_strMenu;
   	dlg.m_strId = m_strId;
   	if (dlg.DoModal() != IDOK) 
   		return FALSE;
   	{
   		m_strName = dlg.m_strText;
   		m_bVisible = dlg.m_bVisible;
   		m_strMenu = dlg.m_strMenu;
   		m_strId = dlg.m_strId;
    
   		ASSERT(m_pDocument != NULL);
   		m_pDocument->SetModifiedFlag();
   		Invalidate();
   	}
   	return TRUE;
}
    
void CEXText:: Draw(CDC* pDC)
{
   	CLabel::Draw(pDC);
   	// Todo: Show authors difference between example text and normal
   #if 0
   	if (theApp.m_bAuthorMode) {
   		CGdiObject* pOldObj = pDC->SelectStockObject(NULL_BRUSH);
   		pDC->Rectangle(m_position);
   		pDC->SelectObject(pOldObj);
   	}
   #endif 0
}
    
CPoint CEXText::GetBtnPos(CRect btnPos)
{
	return CPoint(m_position.right, m_position.top);
}

void CEXText::GetTypeName(CString & strType)
{
	strType = m_strMenu;
}
////////////////////////////////////////////////////////////////////////////
// Generic Graphics objects.
//
// DrawRect: is a graphics shape defined by a bounding box, with the exact
// shape in a subtype member.
//
// DrawPoly: polygons or Bezier curves defined by an array of points.
//
////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CDrawRect, CDrawObj, VERSIONABLE_SCHEMA | 2);
CDrawRect::CDrawRect() {}				// used by serialization
    
CDrawRect::CDrawRect(Shape shape, const CRect& position)
    :CDrawObj(position)
{
    m_bPen = TRUE;
    m_logpen.lopnStyle = PS_INSIDEFRAME;
    m_logpen.lopnWidth.x = 1;
    m_logpen.lopnWidth.y = 1;
    m_logpen.lopnColor = RGB(0, 0, 0);
    
    m_bBrush = TRUE;
    m_logbrush.lbStyle = BS_SOLID;
    m_logbrush.lbColor = RGB(192, 192, 192);
    m_logbrush.lbHatch = HS_HORIZONTAL;
    
   	m_nShape = shape;
    
    // make arcs default to dotted since mainly for trajectory drawing
    if (m_nShape == arc || m_nShape == arc2)
    	m_logpen.lopnStyle = PS_DOT;
}
    
// special id prefix used to signal answer boxes:
const char CDrawRect::c_szAnswerPrefix[] = "Answer";
    
void CDrawRect::Serialize(CArchive& ar)
{
   	UINT nVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
   	// document versions >= 1 added serialization of base class descriptor
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	}
   	CDrawObj::Serialize(ar);
   	if (ar.IsStoring())
   	{
   		ar << (WORD) m_bPen;
   		ar.Write(&m_logpen, sizeof(LOGPEN));
   		ar << (WORD) m_bBrush;
   		ar.Write(&m_logbrush, sizeof(LOGBRUSH));
   
   		ar << (WORD) m_nShape;

   		if (IsAnswerBox())		// Added v2: store answer box status
   			ar << (WORD) m_status; 
   	}
   	else
   	{
   		m_pDocument = (CFBDDoc*) ar.m_pDocument;
   		WORD wTemp;
   		ar >> wTemp; m_bPen = (BOOL) wTemp;
   		ar.Read(&m_logpen, sizeof(LOGPEN));
   		ar >> wTemp; m_bBrush = (BOOL) wTemp;
   		ar.Read(&m_logbrush, sizeof(LOGBRUSH));
  
   		ar >> wTemp; m_nShape = (Shape) wTemp;
    
   		// Added v2 CDrawRect: For answer boxes, get saved status 
   		if (IsAnswerBox() && nVersion >= 2){
   			ar >> wTemp; 
   			m_status = (Status) wTemp; 
   		} 
   	}
}
    
#ifdef _DEBUG
void CDrawRect::Dump(CDumpContext& dc) const
{
   	CDrawObj::Dump(dc);
	CString strType;
	GetTypeName(strType);
   	dc << "Shape: " << strType << "\n";
}
#endif // _DEBUG
    
void CDrawRect::Draw(CDC* pDC)
{
   	ASSERT_VALID(this);
    
   	CBrush brush, *pOldBrush;
   	CPen pen, *pOldPen;
   	/* if (!brush.CreateBrushIndirect(&m_logbrush))
   		return; */
   	/* if (!pen.CreatePenIndirect(&m_logpen))
   		return; */
   	if (m_bBrush && brush.CreateBrushIndirect(&m_logbrush))
   		pOldBrush = pDC->SelectObject(&brush);
   	else
   		pOldBrush = (CBrush*)pDC->SelectStockObject(NULL_BRUSH);
   
   	if (m_bPen && pen.CreatePenIndirect(&m_logpen))
   		pOldPen = pDC->SelectObject(&pen);
   	else
   		pOldPen = (CPen*)pDC->SelectStockObject(NULL_PEN);
    
   	CRect rect = m_position;
   	switch (m_nShape)
   	{
   	case rectangle:
   		pDC->Rectangle(rect);
   		break;
   
   	case roundRect:
   		pDC->RoundRect(rect, CPoint(16, 16));
   		break;
   
   	case ellipse:
   		pDC->Ellipse(rect);
   		break;
  
   	case line:
   		if (rect.top > rect.bottom)
   		{
   			rect.top -= m_logpen.lopnWidth.y / 2;
   			rect.bottom += (m_logpen.lopnWidth.y + 1) / 2;
  		}
   		else
   		{
   			rect.top += (m_logpen.lopnWidth.y + 1) / 2;
   			rect.bottom -= m_logpen.lopnWidth.y / 2;
   		}
   
   		if (rect.left > rect.right)
   		{
   			rect.left -= m_logpen.lopnWidth.x / 2;
   			rect.right += (m_logpen.lopnWidth.x + 1) / 2;
   		}
   		else
   		{
   			rect.left += (m_logpen.lopnWidth.x + 1) / 2;
   			rect.right -= m_logpen.lopnWidth.x / 2;
   		}
    
   		pDC->MoveTo(rect.TopLeft());
   		pDC->LineTo(rect.BottomRight());
   		break;
    
   	case arc: {
   		// user has drawn a "directed" rect from origin at left,top to right, bottom.
   		// We take this box to be one quadrant of the bounding 
   		// box for a quarter elliptical arc.  Note width and height are signed and
   		// bounding box apparently does not need to be normalized.
   		CRect bounding(rect.left - rect.Width(),
   			           rect.top,
   					   rect.right,
   					   rect.bottom + rect.Height() );
   		// arc is always drawn counterclockwise. 
   		// sign of Width + Height codes direction of x and y motion in screen coords
   		// both pos or neg means we want to go clockwise and must invert endpoints
   		if (rect.Width() * rect.Height() > 0)
   			pDC->Arc(bounding,  rect.BottomRight(), rect.TopLeft());
   		else
   			pDC->Arc(bounding, rect.TopLeft(), rect.BottomRight());
   	} break;
    
   	case arc2: {
   		// draw arc with inverse concavity from first arc tool
   		CRect bounding(rect.left, 
   			           rect.top - rect.Height(),
   					   rect.right + rect.Width(),
   					   rect.bottom);
   		if (rect.Width() * rect.Height() > 0)
   			pDC->Arc(bounding,  rect.TopLeft(), rect.BottomRight());
   		else
   			pDC->Arc(bounding, rect.BottomRight(), rect.TopLeft());
    
    		
   	} break;
   	}// end switch
    
   	pDC->SelectObject(pOldBrush);
   	pDC->SelectObject(pOldPen);
    
   	// When printing, superimpose associated text. This is a hack so marker rects used to
   	// mark edit-control positions for Answer boxes will print the student-entered text.
	if (pDC->IsPrinting() || theApp.m_bAuthorMode) // also display in author mode.
	{
   		int oldBkMode = pDC->SetBkMode(TRANSPARENT);
		int oldColor = pDC->SetTextColor(StatusColor()); // draw using status color
		CFont* pFontPrev = pDC->GetCurrentFont();		// save in case changed
		// for printing, show error status even on monochrome by using italic variant font
		CFont fontError;								// don't put inside block!
		if (pDC->IsPrinting() && m_status == statusError) {
			LOGFONT logfont;
			pFontPrev->GetLogFont(&logfont);
			logfont.lfItalic = TRUE;				// Italicize to show
			fontError.CreateFontIndirect(&logfont);
			pDC->SelectObject(&fontError);
		}

		CRect rcText = m_position;
   		rcText.NormalizeRect();
		// Note text may overflow rectangle.
   		pDC->TextOut(rcText.left + 1, rcText.top + 1, m_strName); // slight offset from border
   		
		pDC->SelectObject(pFontPrev);
		pDC->SetTextColor(oldColor);
		pDC->SetBkMode(oldBkMode);
	}
}
    
BOOL CDrawRect::CanDuplicate() { return TRUE; }
    
CDrawObj* CDrawRect::Clone()
{
   	// create duplicate w/same properties 
   	CDrawRect* pClone = new CDrawRect(m_nShape, m_position);
   	pClone->m_strName = m_strName;
   	pClone->m_status = m_status;
   
   	pClone->m_bPen = m_bPen;
   	pClone->m_logpen = m_logpen;
   	pClone->m_bBrush = m_bBrush;
   	pClone->m_logbrush = m_logbrush;
   

   	return (pClone);
}
    
BOOL CDrawRect::OnEditProperties()
{
   	CPropertySheet sheet( _T("Shape Properties") );
   	CRectDlg dlg(this);
   	CAuthorDlg dlgAuth(this);	// construct outside block !
   
   	sheet.AddPage( &dlg );
   	if (theApp.m_bAuthorMode)
   		sheet.AddPage(&dlgAuth);
   
   	if (sheet.DoModal() != IDOK)
   		return FALSE;
   
   	Invalidate();	// redundant? 
   	if (m_pDocument) m_pDocument->SetModifiedFlag();
   
   	return TRUE;
}
    
void CDrawRect::GetTypeName(CString& strType) const
{
	switch (m_nShape)
   	{
   	case rectangle: strType = "Rectangle";	break;
   	case roundRect: strType = "RoundRect";	break;
   	case ellipse:	strType = "Ellipse";	break;
   	case line:		strType = "Line";		break;
   	case arc: 
	case arc2:		strType	= "Arc";		break;
   	default:		strType = "Shape";		break;
	}
}
//
// Polygon Drawing Objects. 
//
// These derive from CDrawRect only because they have the same brush and pen 
// attributes and property dialog box.  These attributes probably
// should be moved into a common base class from which both derive.
//
// Same class with flag used to draw Bezier curves, which also use arrays
// of points.
//
    
IMPLEMENT_SERIAL(CDrawPoly, CDrawRect, VERSIONABLE_SCHEMA | 1)
    
CDrawPoly::CDrawPoly()		// no-arg constructor required by serialization
   	:CDrawRect()
{
   	m_nShape = polygon;
   	m_points = NULL;
   	m_nPoints = 0;
   	m_nAllocPoints = 0;
   
   	m_bBezier = FALSE;
}
    
CDrawPoly::CDrawPoly(const CRect& position, BOOL bBezier)
   	: CDrawRect(polygon, position)
{
   	m_points = NULL;
   	m_nPoints = 0;
   	m_nAllocPoints = 0;
   	m_bPen = TRUE;
   	m_bBrush = FALSE;
    
   	m_bBezier = bBezier;
   	// make Bezier line style default to dotted since 
   	// mainly for trajectory drawing
   	/* if (bBezier)
   		m_logpen.lopnStyle = PS_DOT; */
}
    
CDrawPoly::~CDrawPoly()
{
   	if (m_points != NULL)
   		delete[] m_points;
}
    
void CDrawPoly::Serialize( CArchive& ar )
{
   	// save class version over base class info
   	UINT nVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   	// For document versions >= 1: serialize base class info before its data
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawRect));
   	}
   	CDrawRect::Serialize( ar );
    
   	if( ar.IsStoring() )
   	{
   		ar << (WORD) m_nPoints;
   		ar << (WORD) m_nAllocPoints;
   		for (int i = 0;i< m_nPoints; i++)
   			ar << m_points[i];
   		// added version 1: bBezier flag
   		ar << m_bBezier;
   	}
   	else
   	{
   		// Get version 0 info
   		WORD wTemp;
   		ar >> wTemp; m_nPoints = wTemp;
   		ar >> wTemp; m_nAllocPoints = wTemp;
   		m_points = new CPoint[m_nAllocPoints];
   		for (int i = 0;i < m_nPoints; i++)
   			ar >> m_points[i];
   		// get version 1 info
   		if (nVersion == 1)
   			ar >> m_bBezier;	// else set to default in constructor
   	}
}
    
void CDrawPoly::Draw(CDC* pDC)
{
   	ASSERT_VALID(this);
    
   	CBrush brush;
   	if (!brush.CreateBrushIndirect(&m_logbrush))
   		return;
   	CPen pen;
   	if (!pen.CreatePenIndirect(&m_logpen))
   		return;
   
   	CBrush* pOldBrush;
   	CPen* pOldPen;
    
   	if (m_bBrush)
   		pOldBrush = pDC->SelectObject(&brush);
   	else
   		pOldBrush = (CBrush*)pDC->SelectStockObject(NULL_BRUSH);
    
   	if (m_bPen)
   		pOldPen = pDC->SelectObject(&pen);
   	else
   		pOldPen = (CPen*)pDC->SelectStockObject(NULL_PEN);
    
   	if (! m_bBezier)
   		pDC->Polygon(m_points, m_nPoints);
   	else {
   		DrawCurve(pDC);
   	}
    
   	pDC->SelectObject(pOldBrush);
   	pDC->SelectObject(pOldPen);
}
    
void CDrawPoly::DrawCurve(CDC* pDC)	// do the drawing for a series of path segments
{
/*	pDC->PolyBezier(m_points, m_nPoints);*/
    
   	// We need 3N + 1 points for a curve. Trailing segment might not be complete
   	// while adding. (When adding at first are 3 pts of an incomplete segment)
   	if (m_nPoints < 2) return;	// nothing to draw
    
   	int nLeftOver = (m_nPoints -1) % 3;	//number of points after complete curves 
    
   	// Draw complete curve prefix (possibly none).
   	pDC->PolyBezier(m_points, m_nPoints - nLeftOver);
    
   	// draw incomplete tail from last point of curve to end as polygon
   	// That's nLeftOver + 1 pts, to include last one on curve
   	int nTail = nLeftOver + 1;
   	CPen penHull;
   	penHull.CreatePen(PS_DOT, 1, RGB(128, 128, 128));
   	CPen* pOldPen = pDC->SelectObject(&penHull);
    		
   	pDC->Polyline(&m_points[m_nPoints - nTail], nTail);
   	pDC->SelectObject(pOldPen);
}
    
// position must be in logical coordinates
void CDrawPoly::MoveTo(const CRect& position, CFBDView* pView)
{
   	ASSERT_VALID(this);
   	if (position == m_position)
   		return;
    
   	if (pView == NULL)
   		Invalidate();
   	else
   		pView->InvalObjInView(this);
    
   	for (int i = 0; i < m_nPoints; i += 1)
   	{
   		m_points[i].x += position.left - m_position.left;
   		m_points[i].y += position.top - m_position.top;
   	}
    
   	m_position = position;
   
   	if (pView == NULL)
   		Invalidate();
   	else
   		pView->InvalObjInView(this);
   	if (m_pDocument)	// may be used while manipulating dangling object
		m_pDocument->SetModifiedFlag();
}
    
int CDrawPoly::GetHandleCount()
{
   	return m_nPoints;
}
    
CPoint CDrawPoly::GetHandle(int nHandle)
{
   	ASSERT_VALID(this);
    
   	ASSERT(nHandle >= 1 && nHandle <= m_nPoints);
   	return m_points[nHandle - 1];
}
    
HCURSOR CDrawPoly::GetHandleCursor(int )
{
   	return AfxGetApp()->LoadStandardCursor(IDC_ARROW);
}
    
// point is in logical coordinates
void CDrawPoly::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{
   	ASSERT_VALID(this);
   	ASSERT(nHandle >= 1 && nHandle <= m_nPoints);
  	if (m_points[nHandle - 1] == point)
   		return;
    
   	m_points[nHandle - 1] = point;
   	RecalcBounds(pView);
    
   	if (pView == NULL)
   		Invalidate();
   	else
   		pView->InvalObjInView(this);
   	if (m_pDocument) m_pDocument->SetModifiedFlag();
}
    
// rect must be in logical coordinates
BOOL CDrawPoly::Intersects(const CRect& rect)
{
   	ASSERT_VALID(this);
   	CRgn rgn;
   	rgn.CreatePolygonRgn(m_points, m_nPoints, ALTERNATE);
   	return rgn.RectInRegion(rect);
}
    
CDrawObj* CDrawPoly::Clone()
{
   	ASSERT_VALID(this);
    
   	CDrawPoly* pClone = new CDrawPoly(m_position);
   	pClone->m_bPen = m_bPen;
   	pClone->m_logpen = m_logpen;
   	pClone->m_bBrush = m_bBrush;
   	pClone->m_logbrush = m_logbrush;
   	pClone->m_points = new CPoint[m_nAllocPoints];
   	memcpy(pClone->m_points, m_points, sizeof(CPoint) * m_nPoints);
   	pClone->m_nAllocPoints = m_nAllocPoints;
   	pClone->m_nPoints = m_nPoints;
   	pClone->m_bBezier = m_bBezier;
   	ASSERT_VALID(pClone);
    
   	return pClone;
}
    
// point is in logical coordinates
void CDrawPoly::AddPoint(const CPoint& point, CFBDView* pView)
{
   	ASSERT_VALID(this);
   	if (m_nPoints == m_nAllocPoints)
   	{
   		CPoint* newPoints = new CPoint[m_nAllocPoints + 10];
   		if (m_points != NULL)
   		{
   			memcpy(newPoints, m_points, sizeof(CPoint) * m_nAllocPoints);
   			delete[] m_points;
   		}
   		m_points = newPoints;
   		m_nAllocPoints += 10;
   	}
    
   	if (m_nPoints == 0 || m_points[m_nPoints - 1] != point)
   	{
   		m_points[m_nPoints++] = point;
   		if (!RecalcBounds(pView))
   		{
   			if (pView == NULL)
   				Invalidate();
   			else
   				pView->InvalObjInView(this);
   		}
   		if (m_pDocument)
   			m_pDocument->SetModifiedFlag();
   	}
}
    
BOOL CDrawPoly::RecalcBounds(CFBDView* pView) // returns T if changed bounds
{
   	ASSERT_VALID(this);
    
   	if (m_nPoints == 0)
   		return FALSE;
    
   	CRect bounds(m_points[0], CSize(0, 0));
   	for (int i = 1; i < m_nPoints; ++i)
   	{
   		if (m_points[i].x < bounds.left)
   			bounds.left = m_points[i].x;
   		if (m_points[i].x > bounds.right)
   			bounds.right = m_points[i].x;
   		if (m_points[i].y < bounds.top)
   			bounds.top = m_points[i].y;
   		if (m_points[i].y > bounds.bottom)
   			bounds.bottom = m_points[i].y;
   	}
    
   	if (bounds == m_position)
   		return FALSE;
    
   	if (pView == NULL)
   		Invalidate();
   	else
   		pView->InvalObjInView(this);
    
   	m_position = bounds;
  
   	if (pView == NULL)
   		Invalidate();
   	else
   		pView->InvalObjInView(this);
    
   	return TRUE;
}

void CDrawPoly::GetTypeName(CString& strType)
{
	if (m_bBezier) strType = "Curve";
	else strType = "Polygon";
}  

///////////////////////////////////////////////////////////////////////////////
// Multiple choice question items
///////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CChoice, CDrawObj, VERSIONABLE_SCHEMA | 2);
CChoice::CChoice()	// used by serialization, will init members on load.
{ 
}		
    
CChoice::CChoice(const CRect& position) :
    	CDrawObj(position)
{
   	m_bCorrect = FALSE;
   	m_bChosen = FALSE;
}
    
void CChoice::Serialize(CArchive& ar)
{
   	// Get class version and save over call to base class serialize
   	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   	// in doc versions >= 1, derived classes prepend base class descriptor
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	}
   	CDrawObj::Serialize(ar);
    
   	if (ar.IsStoring())
   	{
   		/* ar << m_bBeginGroup; // stored w/version 0 only */
   		ar << m_bCorrect;
   		ar << m_bChosen;
		// add version 2: red-green status (not serialized in CDrawObj base!)
		ar << (WORD) m_status;
   	}
   	else // loading
   	{
   		if (nClassVersion == 0) {
   			BOOL bBeginGroup;	// stored in version 0, but now unused
   			ar >> bBeginGroup;
   		}
   		ar >> m_bCorrect;
   		ar >> m_bChosen;

		if (nClassVersion >= 2) {
			WORD wTemp;
			ar >> wTemp; m_status = (Status) wTemp;
		} // else leave default status = unknown
   	}
}
    
void CChoice::Draw(CDC* pDC)
{
   	// in author mode, draw as text so author can move this around easily.
   	// Also render this way if we are printing.
	// student mode: control will do its own drawing in views
   	if (! (theApp.m_bAuthorMode || pDC->IsPrinting()))
		return;

   	// save DC attributes in case changed below
   	int oldColor = pDC->SetTextColor(StatusColor()); // draw using status color
	CFont* pFontPrev = pDC->GetCurrentFont();		 // save in case changed

	// for printing, show error status even on monochrome by using italic variant font
	CFont fontError;								// don't put inside block!
	if (pDC->IsPrinting() && m_status == statusError) {
		LOGFONT logfont;
		pFontPrev->GetLogFont(&logfont);
		logfont.lfItalic = TRUE;				// Italicize to show
		fontError.CreateFontIndirect(&logfont);
		pDC->SelectObject(&fontError);
	}
	
	// Draw label preceded by three underline characters
   	CString strDrawn = "___" + m_strName;
   	// First call is to recalc position in case changed
   	pDC->DrawText(strDrawn, m_position, DT_CALCRECT);
   	pDC->DrawText(strDrawn, m_position, DT_LEFT);

	// If checked, superimpose an "X" in the middle of the underline
	if (m_bChosen) {
		int oldBkMode = pDC->SetBkMode(TRANSPARENT);
    	pDC->DrawText("_X", m_position, DT_LEFT);
    	pDC->SetBkMode(oldBkMode);
	}

	// restore DC attributes we might have changed
	pDC->SelectObject(pFontPrev);
	pDC->SetTextColor(oldColor);
}
    
void CChoice::DrawSelectState(CDC* pDC, TrackerState state)
{
   	// no resize handles, sinced sized to content. Use focus rect to show selected
   	pDC->DrawFocusRect(m_position);
}
    
BOOL CChoice::OnEditProperties()
{
   	CChoiceDlg dlg;
    
   	dlg.m_strText = m_strName;
   	dlg.m_bChosen = m_bChosen;
   	dlg.m_bCorrect = m_bCorrect;
   	if (dlg.DoModal() != IDOK)
   		return FALSE;
   	m_strName = dlg.m_strText;
   	m_bChosen = dlg.m_bChosen;
   	m_bCorrect = dlg.m_bCorrect;
    
   	if (m_pDocument != NULL)
   		m_pDocument->SetModifiedFlag();
    
   	// Position rect now invalid, needs recalc. 
   	Invalidate();		// sends hint which should update old position rect
  	// !!! This not perfectly accurate, won't include area for prefix we added when drawing.
    
   	CClientDC dc(NULL);
   	dc.DrawText("___" + m_strName, m_position, DT_CALCRECT);
    
   	Invalidate();		// updates new position rect
   	return TRUE;
}
    
BOOL CChoice::CanEditProperties() 
{ 
	return theApp.m_bAuthorMode; 
}

////////////////////////////////////////////////////////////////////////////////////
// DocArea: Delimited screen areas: really just a kind of rectangle used to define 
// some special area. m_strName used to hold a string saying what kind.
////////////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CDocArea, CDrawObj, VERSIONABLE_SCHEMA | 1);
CDocArea::CDocArea()	// MFC uses to create before loading data from archive 
{}
    
CDocArea::CDocArea(const CRect& position)
   : CDrawObj(position)
{
   	m_bBorder = TRUE;
}
    
#include "Motion.h"	// ugly -- needed to update linked MotionDiagrams on destruction
const char *CDocArea::c_szMotion = "Motion Diagram";
    
CDocArea::~CDocArea()
{
   	// For motion diagram areas: find ruler referencing us and update
   	// when we go away, so its not left with dangling reference.
   	// (we can be deleted first by document cleanup, or by author).
   	if (m_strName == c_szMotion)
   	{
   		if (! m_pDocument) return;
   		POSITION pos = m_pDocument->GetObjects()->GetHeadPosition();
   		while (pos != NULL) 
   		{
   			CDrawObj* pObj = m_pDocument->GetObjects()->GetNext(pos);
   			if ( pObj->IsKindOf(RUNTIME_CLASS(CMotionDiagram)) ) 
   			{
   				CMotionDiagram* pRuler = (CMotionDiagram*) pObj;
   				if (pRuler->m_pArea == this)
   					pRuler->m_pArea = NULL;
   			}
   		}
   	} 
}
    
void CDocArea::Serialize(CArchive& ar)
{
   	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	}
   	CDrawObj::Serialize(ar);
   	if (ar.IsStoring())
   	{	
   		// added version 1
   		ar << m_bBorder;
   	}
   	else
   	{	// get version 1 info
   		if (nClassVersion >= 1)
   			ar >> m_bBorder;
   		else 
   			m_bBorder = TRUE;
   	}
}
    
void CDocArea::Draw(CDC* pDC)
{
 	// Show authors the boundaries in light grey so they know it's there.	
	// Similarly for student-added motion diagram boundaries, but use dark grey
	// to be sure it is clearly visible.
 	if (theApp.m_bAuthorMode || m_strName == c_szMotion)
 	{
 		CPen penDashed(PS_SOLID, 1, 
			(m_strName == c_szMotion) ? RGB(128, 128, 128) /* dark grey */
 									  : RGB(192, 192, 192) /* light grey */);
 		CPen* pOldPen = pDC->SelectObject(&penDashed);
  		CBrush* pOldBrush = (CBrush*) pDC->SelectStockObject(NULL_BRUSH);
   		pDC->Rectangle(m_position);	
		pDC->SelectObject(pOldBrush);
 		pDC->SelectObject(pOldPen);
    
		// Show authors the name text (=area type) in grey too
 		if (theApp.m_bAuthorMode) {
 			COLORREF oldColor = pDC->SetTextColor(RGB(192, 192, 192));
 			pDC->DrawText(m_strName, m_position, DT_LEFT);
 		pDC->SetTextColor(oldColor);
 		}
  	}
  	if (m_bBorder) // draw border around region, applies in either mode
   	{
   		// draw unfilled rectangle
   		pDC->SelectStockObject(NULL_BRUSH);
   		pDC->Rectangle(m_position);
   	}
}
    
BOOL CDocArea::OnEditProperties()
{
   	CAreaDlg dlg;
   	dlg.m_strName = m_strName;
   	dlg.m_bBorder = m_bBorder;
   	if (dlg.DoModal() != IDOK)
   		return FALSE;
   	m_strName = dlg.m_strName;
   	m_bBorder = dlg.m_bBorder;
   
   	if (m_pDocument) m_pDocument->SetModifiedFlag();
   	Invalidate();
   	return TRUE;
}
    
BOOL CDocArea::CanEditProperties() 
{ 
	return theApp.m_bAuthorMode; 
}

//////////////////////////////////////////////////////////////////////////////////
//
// Embedded pictures imported from image files
//
// OLE Picture object wants to load data via an OLE IStream interface. OLE
// provides built-in support for creating an IStream interface on a global memory
// block so we first load data into a global memory block and create the picture
// from that. 
//
// Note: The OLE picture object will convert a compressed jpg or gif file into a bitmap 
// on loading. Saving this picture back into our problem via IPicture->SaveAsFile would result 
// in a problem file very much larger than the original image file, since we would lose 
// the GIF or JPEG compression and save the uncompressed bitmap instead. For that reason, in order 
// to keep the size of our our problem files down, our CDrawPicture object retains the original
// original file  data in memory even AFTER the Picture object is created. This is for the purpose of
// serialization to/from our problem files only. It is the original file data that we save to/load from 
// our own problem document. Keeping this data block around is  is a minor waste of memory in the 
// case where original is in an uncompressed format like a bitmap  or metafile, 
// but we just do it in all cases for the simplicity of uniform code.
//
//////////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CDrawPicture, CDrawObj, VERSIONABLE_SCHEMA | 1);


CDrawPicture::CDrawPicture()
{
	// initialize empty picture object. Should only be temporary, to be filled in
	// by serialization or by loading from picture file.
	m_pPicture = NULL;
	m_cbFileSize = 0;
	m_hFileData = NULL;
}

// Generic routine to get gulp file contents into a global memory block
// returns: hGlobal, filling in cbSize on success, 
// THROWS: CFileException on file error 
//         CMemoryExecption in unlikely event of memory error
HGLOBAL FileToHGlobal(LPCTSTR szFile, int& cbSize)
{
	// open file & get its size (throws exception on failure)
	CFile fileSrc(szFile, CFile::modeRead | CFile::shareDenyWrite);
	cbSize = fileSrc.GetLength();
	
	// alloc global memory based on file size
	HGLOBAL hGlobal = ::GlobalAlloc(GMEM_MOVEABLE, cbSize);
	LPVOID pvData = ::GlobalLock(hGlobal);
	if (pvData == NULL) 
		AfxThrowMemoryException();

	// read file and store in global memory
	int nRead = fileSrc.Read(pvData, cbSize);
	if (nRead != cbSize) 
		AfxThrowFileException(CFileException::endOfFile, -1, szFile);

	// release lock on memory:
	if (::GlobalUnlock(hGlobal) == 0 && GetLastError() != NO_ERROR) {
			TRACE("Global Unlock failed! error=%d\n", GetLastError());
	}

	return hGlobal;
}
    
// factory method creates a new CDrawPicture object from file
// Throws: CFileException on file reading error
//         CMemoryExecption in unlikely event of memory error
//         COleException on other OLE failure to create
CDrawPicture* CDrawPicture::CreateFromFile(LPCTSTR pszPathName)
{
	// Make sure OLE picture obj can be created before allocating C++ CDrawPicture,
	// Will throw out of this routine on failure.

	// Gulp file contents into global memory block:
	int cbFileSize = 0;
	HGLOBAL hGlobal = FileToHGlobal(pszPathName, cbFileSize);

	// Get an OLE IStream* on the global memory.
	// IMPORTANT: set fDeleteOnRelease to FALSE to preserve memory for later
	LPSTREAM pstm = NULL;
	HRESULT hr = CreateStreamOnHGlobal(hGlobal,/*fDeleteOnRelease=*/FALSE, &pstm);
	_ASSERTE(SUCCEEDED(hr) && pstm);

	// Create OLE Picture from image data stream, getting IPicture
	IPicture* pPictureTemp = NULL;  // short-lived temp, no Release needed
	hr = ::OleLoadPicture(pstm, cbFileSize, /*important:*/FALSE, IID_IPicture, 
		                  (LPVOID *) &pPictureTemp);
	pstm->Release();	// now done with stream
	if (!SUCCEEDED(hr)) {
		AfxThrowOleException(hr);
	}
	
	// Create new DrawPic holding file data
	CDrawPicture* pDrawPic = new CDrawPicture();
	pDrawPic->m_pPicture = pPictureTemp; // no AddRef needed 
	pDrawPic->m_hFileData = hGlobal;
	pDrawPic->m_cbFileSize = cbFileSize;
	
	// Get image dimensions, reported in OLE's HIMETRIC units.
	OLE_XSIZE_HIMETRIC hmWidth, hmHeight;
	hr = pDrawPic->m_pPicture->get_Width(&hmWidth);
	_ASSERTE(SUCCEEDED(hr));
	hr = pDrawPic->m_pPicture->get_Height(&hmHeight);
	_ASSERTE(SUCCEEDED(hr));
	TRACE("New Picture: Width=%d, Height=%d HIMETRIC", hmWidth, hmHeight);

	// Set width and height of bounding rectangle in page coordinates
	// Leave top, left as ctor initialized (probably default 0,0 but that could change.)
	pDrawPic->m_position.right = pDrawPic->m_position.left 
		                          + MulDiv(hmWidth, nLUsPerInch, 2540);
   	pDrawPic->m_position.bottom = pDrawPic->m_position.top 
		                          + MulDiv(hmHeight, nLUsPerInch, 2540);
	CRect rcPos = pDrawPic->m_position;
	TRACE("Placed at l=%d,t=%d,r=%d,b=%d (W=%d, H=%d page units)\n", 
		rcPos.left, rcPos.top, rcPos.right, rcPos.bottom, rcPos.Width(), rcPos.Height());

	return pDrawPic;
}


void CDrawPicture::Serialize(CArchive& ar)
{
	LPVOID pvData = NULL;

	 // If loading: Get class version and save over call to base class serialize
   	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   
	// Write base class descriptor and serialize it
   	ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	CDrawObj::Serialize(ar);
    
   	if (ar.IsStoring())
   	{
		// get pointer to file data
   		ASSERT(m_hFileData);
		pvData = ::GlobalLock(m_hFileData);
		if (pvData == NULL) {
			TRACE("Couldn't lock image data, error=%d\n", GetLastError()); 
			ASSERT(FALSE);
			return;
		}
		// write size-prefixed data block
		ar << m_cbFileSize;
   		ar.Write(pvData, m_cbFileSize);
		::GlobalUnlock(m_hFileData);
   	}
   	else
   	{
   		ar >> m_cbFileSize;
		ASSERT(m_cbFileSize != 0);
		// allocate hglobal buffer and fill with image data
		m_hFileData = ::GlobalAlloc(GMEM_MOVEABLE, m_cbFileSize);
		pvData = ::GlobalLock(m_hFileData);
		int nRead = ar.Read(pvData, m_cbFileSize);
		::GlobalUnlock(m_hFileData);
		if (nRead != m_cbFileSize) {
			AfxThrowArchiveException(CArchiveException::badSchema);
		}
	
		// create the image from the hglobal data
		// Get an OLE IStream* on the global memory
		LPSTREAM pstm = NULL;
		HRESULT hr = CreateStreamOnHGlobal(m_hFileData, FALSE, &pstm);
		_ASSERTE(SUCCEEDED(hr) && pstm);
		// Create OLE Picture from image data stream, getting IPicture
		hr = ::OleLoadPicture(pstm, m_cbFileSize, FALSE, IID_IPicture, 
							(LPVOID *) &m_pPicture);
		_ASSERTE(SUCCEEDED(hr));	
		pstm->Release();	// now done with stream
	}
}

void CDrawPicture::Draw(CDC* pDC)
{
	HRESULT hr;
	OLE_XSIZE_HIMETRIC hmWidth, hmHeight;
	hr = m_pPicture->get_Width(&hmWidth);
	_ASSERTE(SUCCEEDED(hr));
	hr = m_pPicture->get_Height(&hmHeight);
	_ASSERTE(SUCCEEDED(hr));

	m_pPicture->Render(*pDC, // destination rect in DC's logical coords: 
					    m_position.left, m_position.top, 
		                m_position.Width(), m_position.Height(),
						// source portion of image in HIMETRIC coordinates: we want
						// it all, inverting orientation because our y points down
						0, hmHeight, hmWidth, -hmHeight,
						// Whole drawing window extent: This is needed only when 
						// rendering the picture *into* a metafile DC.
						NULL
						);
}

void CDrawPicture::DrawSelectState(CDC* pDC, TrackerState state)
{
	// highlight image border when selected
	pDC->DrawFocusRect(m_position);
	// base class will draw resize handles
	CDrawObj::DrawSelectState(pDC, state);
}

CDrawPicture::~CDrawPicture()
{
	if (m_hFileData) {
		::GlobalFree(m_hFileData);
	}
	if (m_pPicture) {
		m_pPicture->Release();
		m_pPicture = NULL;
	}
}

//////////////////////////////////////////////////////////////////////////////////
//
// OLE Embedded Items
//
//////////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(COleDrawObj, CDrawObj, VERSIONABLE_SCHEMA | 1);
    
BOOL COleDrawObj::c_bShowItems = FALSE; // set to false to suppress border
    
COleDrawObj::COleDrawObj() : m_extent(0,0)
{
   	m_pClientItem = NULL;
}
    
COleDrawObj::COleDrawObj(const CRect& position)
   	: CDrawObj(position), m_extent(0, 0)
{
   	m_pClientItem = NULL;
}

COleDrawObj::~COleDrawObj()
{
   	if (m_pClientItem != NULL)	  // [shouldn't happen?] ClientItem still exists
   	{
   		m_pClientItem->Release(); // Ensure release OLE srvr app.
   		m_pClientItem = NULL;	  // Note client item obj may still be in doc
   	}
}
    
void COleDrawObj::Delete()
{
	// First delete associated ClientItem
   	if (m_pClientItem != NULL)
   	{
   		m_pClientItem->Delete();// Deletes ClientItem and any native data from doc
   		m_pClientItem = NULL;
   	}
   	CDrawObj::Delete();			// destroys drawobj and deletes this
}
    
void COleDrawObj::Serialize( CArchive& ar )
{
   	ASSERT_VALID(this);
   	// Get class version and save over call to base class serialize
   	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
   	// in doc versions >= 1, derived classes prepend base class descriptor
   	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
   	if (nDocVersion >= 1) {
   		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
   	}
   	CDrawObj::Serialize(ar);
    
   	if (ar.IsStoring())
   	{
   		ar << m_extent;
   		ar << m_pClientItem;
   	}
   	else
   	{
   		ar >> m_extent;
   		ar >> m_pClientItem;
   		m_pClientItem->m_pDrawObj = this;
   	}
}
    
CDrawObj* COleDrawObj::Clone(CFBDDoc* pDoc)
{
   	ASSERT_VALID(this);
    
   	AfxGetApp()->BeginWaitCursor();
    
   	COleDrawObj* pClone = NULL;
   	COleDrawItem* pItem = NULL;
   	TRY
   	{
   		// perform a "deep copy" -- need to copy COleDrawObj and the COleDrawItem
   		//  that it points to.
   		COleDrawObj* pClone = new COleDrawObj(m_position);
   		COleDrawItem* pItem = new COleDrawItem(m_pDocument, pClone);
   		if (!pItem->CreateCloneFrom(m_pClientItem))
   			AfxThrowMemoryException();
   
   		pClone->m_pClientItem = pItem;
   		ASSERT_VALID(pClone);
    }
   	CATCH_ALL(e)
   	{
   		pItem->Delete();
   		pClone->m_pClientItem = NULL;
   		pClone->Delete();
   		AfxGetApp()->EndWaitCursor();
   
   		THROW_LAST();
   	}
   	END_CATCH_ALL
    
   	AfxGetApp()->EndWaitCursor();
   	return pClone;
}
    
void COleDrawObj::Draw(CDC* pDC)
{
   	ASSERT_VALID(this);
    
   	COleDrawItem* pItem = m_pClientItem;
   	if (pItem != NULL)
   	{
   		// draw the OLE item itself, scaled so as to fit in position rectangle
   		pItem->Draw(pDC, m_position);
   
   		// don't draw tracker in print preview or on printer
   		if (!pDC->IsPrinting())
   		{
   			// use a CRectTracker to draw the standard effects
   			CRectTracker tracker;
   			tracker.m_rect = m_position;
   			pDC->LPtoDP(tracker.m_rect);
   
   			if (c_bShowItems)	// optionally highlights OLE items w/border
   			{
   				// put correct border depending on item type
   				if (pItem->GetType() == OT_LINK)
   					tracker.m_nStyle |= CRectTracker::dottedLine;
   				else
   					tracker.m_nStyle |= CRectTracker::solidLine;
   			}
   
   			// put hatching over the item if it is currently open
   			if (pItem->GetItemState() == COleClientItem::openState ||
   				pItem->GetItemState() == COleClientItem::activeUIState)
   			{
  				tracker.m_nStyle |= CRectTracker::hatchInside;
   			}
   			tracker.Draw(pDC);
   		}
   	}
}
    
void COleDrawObj::OnOpen(CFBDView* pView)
{
   	AfxGetApp()->BeginWaitCursor();
   	m_pClientItem->DoVerb(		
   		GetKeyState(VK_CONTROL) < 0 ? OLEIVERB_OPEN : OLEIVERB_PRIMARY, pView);
   	AfxGetApp()->EndWaitCursor();
}
    
BOOL COleDrawObj::OnEditProperties()
{
   	// using COlePropertiesDialog directly means no scaling
   	COlePropertiesDialog dlg(m_pClientItem, 100, 100, NULL);
    
   	return dlg.DoModal() == IDOK;
}
    
// position is in logical
void COleDrawObj::MoveTo(const CRect& position, CFBDView* pView)
{
   	ASSERT_VALID(this);
    
   	if (position == m_position)
   		return;
    
   	// call base class to update position
   	CDrawObj::MoveTo(position, pView);
    
   	// update position of in-place editing session on position change
   	if (m_pClientItem->IsInPlaceActive())
   		m_pClientItem->SetItemRects();
}
    
// 
// Note no special handling of MoveHandleTo for resizing in container when not active.
// Resizing of presentation size comes for free from default CDrawObj: dragging the
// selection handles changes the size of the m_position rectangle with MoveTo. This
// is the area into which the object's presentation is rendered. If in-place editing is
// initiated, this changed position will be passed to the server via the item's
// OnGetItemPosition override. 
//
// Thee upshot is that resizing *scales* the embedded item's presentation, but may not 
// update the intrinsic natural extent of the object. 
// This is not always very intuitive -- sometimes user really wants e.g. more spreadsheet
// cells or more columns of text. (Depends on server whether you can do this at all?)
    
/////////////////////////////////////////////////////////////////////////////
// COleDrawItem implementation
/////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(COleDrawItem, COleClientItem, VERSIONABLE_SCHEMA | 1)
    
COleDrawItem::COleDrawItem(CFBDDoc* pContainer, COleDrawObj* pDrawObj)
   	: COleClientItem(pContainer)
{
   	m_pDrawObj = pDrawObj;
}
    
COleDrawItem::~COleDrawItem()
{
   	if (m_pDrawObj != NULL)
   		m_pDrawObj->m_pClientItem = NULL;
}

// Notification: OLE item has changed
void COleDrawItem::OnChange(OLE_NOTIFICATION nCode, DWORD dwParam)
{
   	ASSERT_VALID(this);
    
   	COleClientItem::OnChange(nCode, dwParam);
    
   	switch(nCode)
   	{
   	case OLE_CHANGED_STATE:
   	case OLE_CHANGED_ASPECT:
   		m_pDrawObj->Invalidate();
   		break;
   	case OLE_CHANGED:
   		UpdateFromServerExtent(); // extent may have changed
   		m_pDrawObj->Invalidate();
   		break;
   	}
}

// Notification: item has been resized during in-place editing.
BOOL COleDrawItem::OnChangeItemPosition(const CRect& rectPos)	
{
   	ASSERT_VALID(this);
    
   	CFBDView* pView = GetActiveView();
   	ASSERT_VALID(pView);
   	CRect rect = rectPos;		// pos comes in client-area coords
   	pView->ClientToDoc(rect);
    
   	if (rect != m_pDrawObj->m_position)
   	{
   		// invalidate old rectangle
   		m_pDrawObj->Invalidate();
   
   		// update to new rectangle
   		m_pDrawObj->m_position = rect;

   		/* GetExtent(&m_pDrawObj->m_extent); */
   		GetCachedExtent(&m_pDrawObj->m_extent); /* docs say to use this in handlers? */
    
   		// and invalidate new rectangle
   		m_pDrawObj->Invalidate();
   
   		// mark document as dirty
   		GetDocument()->SetModifiedFlag();
   	}
   	// Base class will update internal data
   	return COleClientItem::OnChangeItemPosition(rectPos);
}

// MFC requesting item pos in container's client-area before in-place editing
void COleDrawItem::OnGetItemPosition(CRect& rPosition)
{
   	ASSERT_VALID(this);
    
   	// if position is not initialized, update w/natural extent of item from server
   	if (m_pDrawObj->m_position.IsRectEmpty())
   		UpdateFromServerExtent();
   
   	// set from m_position, which is in document coordinates
   	CFBDView* pView = GetActiveView();
   	ASSERT_VALID(pView);
   	rPosition = m_pDrawObj->m_position;
   	pView->DocToClient(rPosition);
} 
   
void COleDrawItem::Serialize(CArchive& ar)
{
   	ASSERT_VALID(this);
    
   	// Call base class first to read in COleClientItem data.
   	// Note: this sets up the m_pDocument pointer returned from
  	//  COleDrawItem::GetDocument, therefore it is a good idea
   	//  to call the base class Serialize first.
   	COleClientItem::Serialize(ar);
   	// (No need for use to prepend base class descriptor, since it's an MFC base 
   	// class not one of our versionable base classes.
   
  	// now store/retrieve data specific to COleDrawItem
   	if (ar.IsStoring())
   	{
   		// TODO: add storing code here
   	}
   	else
   	{
   		// TODO: add loading code here
   	}
}

// Helper updates item data when natural extent of item may have changed in server.    
BOOL COleDrawItem::UpdateFromServerExtent()
{
   	CSize newExtent;
   	// Changed to use GetCachedExtent instead, as per tutorial and docs.
   	if (!GetCachedExtent(&newExtent) || newExtent == m_pDrawObj->m_extent)
   		return FALSE;       
    
   	// if new object (i.e. m_extent is empty) update position to reflect server's extent
   	if (m_pDrawObj->m_extent == CSize(0, 0))
   	{
   		// OLE passes sizes in HIMETRIC units (hundredths of a millimeter).
    		
   	/*  DRAWCLI converted from HIMETRIC to its logical LOENGLISH units, hundredths of
   	    an inch using 25.4 mm = 1 in. Minus sign because y axis negative down:
   		m_pDrawObj->m_position.right =
   			m_pDrawObj->m_position.left + MulDiv(size.cx, 10, 254);
   		m_pDrawObj->m_position.bottom =
   			m_pDrawObj->m_position.top - MulDiv(size.cy, 10, 254); */
   
   		// Convert to our units 
   		m_pDrawObj->m_position.right =
   			m_pDrawObj->m_position.left + MulDiv(newExtent.cx, 96, 2540);
   		m_pDrawObj->m_position.bottom =
   			m_pDrawObj->m_position.top + MulDiv(newExtent.cy, 96, 2540);
   	}
   	// else if extent changed and not active: scale current position rect by
   	// newExtent/oldExtent. (Position may already have been scaled by resizing in container)
   	else if (!IsInPlaceActive() && newExtent != m_pDrawObj->m_extent)
   	{
   		m_pDrawObj->m_position.right = m_pDrawObj->m_position.left +
   			MulDiv(m_pDrawObj->m_position.Width(), newExtent.cx, m_pDrawObj->m_extent.cx);
   		 m_pDrawObj->m_position.bottom = m_pDrawObj->m_position.top +
   			MulDiv(m_pDrawObj->m_position.Height(), newExtent.cy, m_pDrawObj->m_extent.cy);
   	}
    
   	// Save the new extent.
   	m_pDrawObj->m_extent = newExtent;
    
   	// redraw with the new size/position
   	m_pDrawObj->Invalidate(); 
    	
   	// !!! SetModifiedFlag(); ?Will this be done by caller?
    
   	return TRUE;
}
    
void COleDrawItem::OnActivate()
{
   	// allow only one inplace active item per frame
   	CView* pView = GetActiveView();
   	ASSERT_VALID(pView);
   	COleClientItem* pItem = GetDocument()->GetInPlaceActiveItem(pView);
   	if (pItem != NULL && pItem != this)
   		pItem->Close();
   
   	COleClientItem::OnActivate();
}
    
void COleDrawItem::OnDeactivateUI(BOOL bUndoable)
{
   	COleClientItem::OnDeactivateUI(bUndoable);
   
   	// hide the object if it is not an outside-in object
   	DWORD dwMisc = 0;
   	m_lpObject->GetMiscStatus(GetDrawAspect(), &dwMisc);
   	if (dwMisc & OLEMISC_INSIDEOUT)
   		DoVerb(OLEIVERB_HIDE, NULL);
}
    
/////////////////////////////////////////////////////////////////////////////
// COleDrawItem diagnostics
    
#ifdef _DEBUG
void COleDrawItem::AssertValid() const
{
   	COleClientItem::AssertValid();
}
    
void COleDrawItem::Dump(CDumpContext& dc) const
{
   	COleClientItem::Dump(dc);
}
#endif
    
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CEXInfo helper structure
IMPLEMENT_DYNCREATE(CEXInfo, CObject)
   
CEXInfo::CEXInfo()
{//nothing explained initially
   	m_bExplained = FALSE;
}
  
#ifdef _DEBUG
void CEXInfo::Dump(CDumpContext& dc) const
{
  	CObject::Dump(dc);
   	dc << "String rule: " << m_strRule << "\n";
   	dc << "Pos: " << m_planItemPos << " rule" << m_rule << " Explained?: " << (int) m_bExplained << "\n";
}
#endif // _DEBUG  
/////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CVariable, CCheckedObj, VERSIONABLE_SCHEMA | 6);

const char *CVariable::c_szAllForces = "all forces";
const char *CVariable::c_szNCForces = "non-conservative forces";

CVariable::CVariable()
{
	m_position= CRect(0, 0, 0, 0);
}
    
void CVariable::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	// document versions >= 1 added serialization of base class descriptor
	
	if (ar.IsStoring())
	{						 // store object specific data
		if (nDocVersion >= 1) {
			ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
		}
		CDrawObj::Serialize(ar);

		ar << m_strDef;
		ar << m_strForceType;
		ar << m_strQuantName;
		ar << m_strObject;
		ar << m_strTime;
		ar << m_strAgent;
		ar << m_nType;
		// added v5 -- serialize status (again). Status is rechecked on normal open, 
		// so not needed in that case, but it is desirable to store it so can have
		// state "snapshot" files. Can print these from shell w/o rechecking
		ar << (WORD) m_status;
		// added v6:
		ar << m_strValue;
	} 
	else 
	{	
		if (nClassVersion < 3)// initially variables were not DrawObj's
			CObject::Serialize(ar);// they were derived from CObject
		else
		{
			if (nDocVersion >= 1) {
			ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
			}
			CDrawObj::Serialize(ar);
		}
		WORD temp;// load object specific data
		if (nClassVersion == 0){	
			ar >> m_strName;
			ar >> m_strDef;
			ar >> m_strId;
			ar >> WORD(temp);//	m_bDrawObj
			ar >> WORD(temp);
			m_status = (Status)temp;
		}
		else if (nClassVersion >= 1){
			ar >> m_strDef;
			if (nClassVersion == 1)
				ar >> WORD(temp);//m_bDrawObj
		}
		ar >> m_strForceType;
		ar >> m_strQuantName;
		ar >> m_strObject;
		ar >> m_strTime;
		ar >> m_strAgent;
		if (nClassVersion < 4)
			m_nType = -1;
		else
			ar >> m_nType;
		if (nClassVersion >= 5){
			ar >> temp; m_status = (Status)temp;
		}
		// We changed some m_nType codes in Andes 8.1.6, causing a bug reading old solutions.
		// To avoid any such problems in the future, recalculate m_nType from m_strQuantName string
		// on every load, so integer codes are not really persistent and can be changed freely.
		// Nuisance: CVariableDlg always stores friendly type name (e.g. "object distance") in m_strQuantName. 
		// Not sure if custom variable dialogs are consistent in this. So also check for typeid.
		// Could pass document concept flags to disambiguate
    	// ((CFBDDoc*) ar.m_pDocument)->m_wConcept;
		m_nType = CVarView::ValueToId(m_strQuantName);	 // lookup by friendly quantity name
		if (m_nType <= 0) 
			m_nType = CVarView::LookupId(m_strQuantName); // not found: try lookup by strTypeId
		ASSERT(m_nType != -1);		// should throw runtime error?
	
		// if angle variable, recalc angle degrees.

		if (nClassVersion >= 6) {
			ar >> m_strValue;
		}
		// fixup for reading older variables:
		// older version of time constant used agent arg; new version uses list in body arg
		if (m_strQuantName == "time constant" && ! m_strAgent.IsEmpty())
			m_strObject += " " + m_strAgent;
	}
}

void CVariable::LogEntry()
{
	// Get text type id if available via CVarView table lookup
	CString strTypeId = CVarView::LookupTypeId(m_nType);
	if (strTypeId.IsEmpty())
		strTypeId.Format("%d", m_nType);

	// strQuantName and strDef are redundant given type and args, but we originally
	// had no easy methods to rederive them from the other fields, so just logged them.
	CString strQuantName = ValToArg(m_strQuantName);
	CString strObject = ValToArg(m_strObject);
	CString strTime = ValToArg(m_strTime);
	CString strAgent = ValToArg(m_strAgent);
	CString strForceType = ValToArg(m_strForceType);
	CString strDef = ValToArg(m_strDef);
	// added (v6 serialization):
	CString strValue = ValToArg(m_strValue);

	LogEventf(EV_VAR_ENTRY, "%s %s %s %d %s %s %s %s %s %s %s", 
		strTypeId, m_strName,  m_strId, m_status, 
		strForceType, strQuantName, strObject, strAgent, strTime, strDef, strValue);
	// need to log angle number for angle variables ? 
}

BOOL CVariable::SetFromLogStr(LPCTSTR pszStr)
{
	char szTypeName[80];
	char szName[80];
	char szId[80];
	char szForceType[80];
	char szQuantName[80];	
	char szObject[80]; 
	char szAgent[80];
	char szTime[80];
	char szDef[255];	// can be long, but should still be plenty
	char szValue[80];
	int nStatus;

	// 11th arg added later, absent from older logs.
	int nArgs = sscanf(pszStr,"%s %s %s %d %s %s %s %s %s %s %s", 
		szTypeName, szName, szId, &nStatus,
		szForceType, szValue, szObject, szAgent, szTime, szDef, szValue);
	if (nArgs < 10)
		return FALSE;
	
	m_nType = CVarView::LookupId(szTypeName);
	m_strName = ArgToVal(szName);
	m_strId = ArgToVal(szId);
	ASSERT(0 <= nStatus && nStatus <= 2);
	m_status = (Status) nStatus;
	m_strForceType = ArgToVal(szForceType);
	m_strQuantName = ArgToVal(szQuantName);
	m_strObject = ArgToVal(szObject);
	m_strAgent = ArgToVal(szAgent);
	m_strTime = ArgToVal(szTime);
	m_strDef = ArgToVal(szDef);
	if (nArgs >= 11)
		m_strValue = ArgToVal(szValue);

	return TRUE;
}

// Get the command sent to helpsys to check this variable entry.
// This will be sent as an *argument* when picking a quantity
CString CVariable::GetCheckCmd()
{
	CString strCmd;	// final command string result

	// strQuantId = quantity type id used by helpsys
	CString strQuantId = m_strQuantName;	// initial default = friendly quantity name (may have spaces)!
	CString strObject = m_strObject;
	CString strTime = m_strTime;
	CString strAgent = m_strAgent;
	CString strForceType = m_strForceType;

	// look for correct quant id from CVarView table
	CString strTemp = CVarView::LookupTypeId(m_nType);
	if (! strTemp.IsEmpty())
		strQuantId = strTemp;

	// change any spaces in quant-id to hyphens
	strQuantId.Replace(" ", "-");

	// Urgh, need some quantity-specific adjustments to arguments.
	// Also, subtype not always set in dialog.
	if (m_nType == ID_VARIABLE_ADDTIME){
		strTime.Format("%s to %s", m_strObject, m_strTime);
		strObject = "";
	} else if (m_nType == ID_VARIABLE_ADDNRG) {
		// delete last word in energy type to send subtype:
		//    Total Mechanical => "Total"; "Translational" [kinetic], "Rotational" [kinetic],
		//    "Elastic" [potential], "Gravtiational" [Potential], "Electric" [potential].
		//    "Electric Dipole [potential]", "Magnetic Dipole [potential]"
		CString strBeg;
		int nPos = strForceType.ReverseFind(' ');
		if (nPos > 0) {
			strBeg = strForceType.Left(nPos);
			strForceType = strBeg;
			// for backwards compat, continue to send this as "Kinetic"
			if (strForceType.CompareNoCase("Translational") == 0)
				strForceType = "Kinetic";
			// no spaces in dipole subtypes
			strForceType.Replace(" ", "-");
		}
	} else if (m_nType == ID_VARIABLE_ADDRESISTANCE
		    || m_nType == ID_VARIABLE_ADDCAPACITANCE){
		// listify body arg (wrap in parens) if tagged as equivalent
		// NB: don't want list-valued arg wrapped in vbars in call (else it reads as symbol, not list)
		// handled below when command string is built
 		if (m_strForceType == "equiv")
			strObject = "(" + m_strObject + ")";
	} else if (m_nType == ID_VARIABLE_ADDTIMECONSTANT) {
			strObject = "(" + m_strObject + ")";
	} else if (m_nType == ID_VARIABLE_ADDPROBABILITY) {
			CProbDlg::EventNameToHelpFormat(strObject);
	}

	if (m_nType == ID_VARIABLE_ADDANGLE)	// uses special API call
	{		
		// translate axes specs if either arg is an axis code.
		// Upcase symbols for backwards compatibility with old Andes helpsys
		for (int i=0; i < nAxesStrs; i++)
		{
			if (m_strObject == axes[i].strDef) {
				strObject = axes[i].strHelp;
				strObject.MakeUpper();
			}
			if (m_strAgent == axes[i].strDef) {
				strAgent = axes[i].strHelp;
				strAgent.MakeUpper();
			}
		}
		// calculate angle from angle of sides as needed, using public static method in
		// angle dialog. Use string to send "NIL" if unknown.
		CString strDeg = "NIL";
		int nDegrees = CAngleDlg::GetAngleBetween(m_strObject, m_strAgent);
		if (nDegrees != -1)		// means unknown
			strDeg.Format("%d", nDegrees);
		// Changed 9/25/01 (6.0.3): wrap label args in vbars to preserve case
		strCmd.Format( "(define-angle-variable \"%s\" %s |%s| |%s| %s)", 
						STR2ARG(m_strName), 
						strDeg,
						STR2ARG(strObject),
						STR2ARG(strAgent), 
						m_strId);
	}
	else
	{
			// NB: don't want list-valued object arg wrapped in vbars in call (else it reads as symbol, not list)
			// all other args should always be symbols (hopefully). Exception: probability, in which compound
			// event name can begin with "("
			CString strObjectArg = STR2ARG(strObject);
			if (strObjectArg[0] != '(' || m_nType == ID_VARIABLE_ADDPROBABILITY)
				strObjectArg = "|" + strObjectArg + "|";
			// For value, NIL => no given value assertion; empty string => asserts unknown value
			// Note don't want quotes around NIL value arg if it is sent.
			CString strValueArg = "\"" + LISPSTR(m_strValue) + "\"";
			strCmd.Format( "(define-variable \"%s\" |%s| |%s| %s |%s| |%s| %s %s)",
			STR2ARG(m_strName),   // !!! use LISPSTR if non-empty
			STR2ARG(strForceType),
			STR2ARG(strQuantId),
			strObjectArg,
			STR2ARG(strTime), 
			STR2ARG(strAgent),
			STR2ARG(m_strId),
			strValueArg); 
	}
	// finally return result
	return strCmd;
}


void CVariable::CheckObject()
{
	ApplyStatus(HelpSystemExecf(GetCheckCmd()));
}
    

CString CVariable::GetDef()
{
	return m_strDef;
}
    
// return appropriate dialog for editing variable based on type.
// Special dialogs for vectors -- also used for drawn dialogs 
// Special dialogs for some scalars. Else get the generic variable dialog.
// Returned dialog is dynamically allocated, must be deleted by caller.
// !!! This should be done as virtual function on subclasses !!!
CDialog* CVariable::GetPropertyDlg()
{
	// Vector dialogs are all special:
	if (m_nType == ID_VARIABLE_ADDFORCE)
		return new CVectorDlg(this);
	else if (m_nType == ID_VARIABLE_ADDTORQUE)
		return new CTorqueDlg(this);
	else if (m_nType == ID_VARIABLE_ADDRELPOS)
		return new CVecPosDlg(this);
	else if (m_nType == ID_VARIABLE_ADDRELVEL)
		 return new CRelVelDlg(this);
	else if (m_nType == ID_VARIABLE_ADDEFIELD)
		 return new CFieldDlg(this);
	else if (m_nType == ID_VARIABLE_ADDBFIELD)
		 return new CFieldDlg(this, /*bMagnetic=*/TRUE); 
	else if (m_nType == ID_VARIABLE_ADDIMPULSE)
		return new CImpulseDlg(this);
	else if (m_nType == ID_VARIABLE_ADDUNITVECTOR)
		return new CUnitVectorDlg(this);
	else if (m_nType == ID_VARIABLE_ADDMAGDIPOLE)
		return new CDipoleDlg(this, /*bMagnetic*/TRUE);
	else if (m_nType == ID_VARIABLE_ADDELECDIPOLE)
		return new CDipoleDlg(this, /*bMagnetic*/FALSE);
	else if (IsLinearVector() || IsAngularVector() ||
			 m_nType == ID_VARIABLE_ADDSPEED) 
	{
		CVectorMoveDlg* pDlg = new CVectorMoveDlg(this);
		if (m_nType == ID_VARIABLE_ADDVELOCITY ||
			m_nType == ID_VARIABLE_ADDANGVELOCITY)
			pDlg->m_strDescription = "Velocity";
		else if (m_nType == ID_VARIABLE_ADDACCELERATION ||
				 m_nType == ID_VARIABLE_ADDANGACCELERATION)
			pDlg->m_strDescription = "Acceleration";
		else if (m_nType == ID_VARIABLE_ADDDISPLACEMENT ||
				 m_nType == ID_VARIABLE_ADDANGDISPLACEMENT)
			pDlg->m_strDescription = "Displacement";
		else if (m_nType == ID_VARIABLE_ADDMOMENTUM ||
			     m_nType == ID_VARIABLE_ADDANGMOMENTUM)
			pDlg->m_strDescription = "Momentum";
		else if (m_nType == ID_VARIABLE_ADDSPEED)
			pDlg->m_strDescription = "Speed";
		return pDlg;
	}
	// some scalar variables use built-in custom dialogs:
	//else if (m_nType == ID_VARIABLE_ADDRADIUS)
	//	return new CRadiusDlg(this);
	else if (m_nType == ID_VARIABLE_ADDANGLE)
		return new CAngleDlg(this);
	else if ( m_nType == ID_VARIABLE_ADDNRG )
		return new CEnergyDlg(this);
	else if ( m_nType == ID_VARIABLE_ADDVOLTAGE)
		return new CVoltageDlg(this);
	else if ( m_nType == ID_VARIABLE_ADDCURRENT)
		return new CCurrentDlg(this);
	else if ( m_nType == ID_VARIABLE_ADDRESISTANCE)
		return new CResistanceDlg(this);
	else if ( m_nType == ID_VARIABLE_ADDCAPACITANCE)
		return new CCapacitanceDlg(this); 
	else if (m_nType == ID_VARIABLE_ADDPROBABILITY)
		return new CProbDlg(this);
	else if (m_nType == ID_VARIABLE_ADDTIMECONSTANT)
		return new CTimeConstantDlg(this);
	else // use generic static variable dialog
		return new CVariableDlg(this);
}

CString CVariable::GetLabelPrefix()
{
	// energy is a special case  (move this to dialog?? Maybe problem to adjust prefix
	// after default sets empty prefix in InitDlg/InitLabel ?)
	if (m_nType == ID_VARIABLE_ADDNRG)		
	{
		if (!m_strForceType.IsEmpty())
		{
			if (m_strForceType.CompareNoCase("Translational Kinetic") == 0)		  
				return "K";
			if (m_strForceType.CompareNoCase("Rotational Kinetic") == 0)		  
				return "Kr";
			else if (m_strForceType.CompareNoCase("Total Mechanical") == 0)  
				return "ME";
			else if (m_strForceType.CompareNoCase("Potential") == 0)   // (now unused?)
				return "U";
			else if (m_strForceType.CompareNoCase("Gravitational Potential") == 0) 
				return "Ug";
			else if (m_strForceType.CompareNoCase("Elastic Potential") == 0)    
				return "Us";
			else if (m_strForceType.CompareNoCase("Electric Potential") == 0)
				return "Ue";
			else if (m_strForceType.CompareNoCase("Electric Dipole Potential") == 0)
				return "Ue";
			else if (m_strForceType.CompareNoCase("Magnetic Dipole Potential") == 0)
				return "Um";
		}
		return "";
	}
	// else get it from quantity info table in CVarView
	return CVarView::LookupPrefix(m_nType);
}

BOOL CVariable::HasComponents()
{
	return( 
		 (m_nType == ID_VARIABLE_ADDACCELERATION) 
	  || (m_nType == ID_VARIABLE_ADDVELOCITY)
	  || (m_nType == ID_VARIABLE_ADDFORCE) 
	  || (m_nType == ID_VARIABLE_ADDDISPLACEMENT)
	  || (m_nType == ID_VARIABLE_ADDMOMENTUM)
	  || (m_nType == ID_VARIABLE_ADDANGVELOCITY)
	  || (m_nType == ID_VARIABLE_ADDANGACCELERATION)
	  || (m_nType == ID_VARIABLE_ADDANGDISPLACEMENT)
	  || (m_nType == ID_VARIABLE_ADDANGMOMENTUM)
	  || (m_nType == ID_VARIABLE_ADDTORQUE)
	  || (m_nType == ID_VARIABLE_ADDRELPOS)
	  || (m_nType == ID_VARIABLE_ADDEFIELD)
	  || (m_nType == ID_VARIABLE_ADDBFIELD)
	  || (m_nType == ID_VARIABLE_ADDIMPULSE)
	  || (m_nType == ID_VARIABLE_ADDUNITVECTOR)
	  || (m_nType == ID_VARIABLE_ADDMAGDIPOLE)
	  || (m_nType == ID_VARIABLE_ADDELECDIPOLE)
	);
}


CDrawObj* CVariable::Clone()
{
	// create duplicate w/same properties 
	CVariable* pClone = new CVariable();

	pClone->m_strId = m_strId;	// Initially, use same id, if added to document, 
								// different id will be generated

	pClone->m_strName = m_strName;
	pClone->m_status = m_status;

	pClone->m_nType = m_nType;
	pClone->m_strForceType = m_strForceType;
	pClone->m_strQuantName = m_strQuantName;
	pClone->m_strObject = m_strObject;
	pClone->m_strAgent = m_strAgent;
	pClone->m_strTime = m_strTime;
    pClone->m_strValue = m_strValue;
	pClone->m_strDef = m_strDef;

	return (pClone);
}

// T if var has same def as distinct Var so conflicts. 
// F in case compared to self
BOOL CVariable::HasSameDef(CVariable* pVar)
{
	// Cheap test -- check if constructed definition string is the same
	if (_stricmp(m_strDef, pVar->m_strDef) == 0){//case insensitive
	//if same name, check if same id
		if (strcmp(m_strId, pVar->m_strId)==0)
			return FALSE;//we are editing this variable
		else
			return TRUE;	
	}
	return FALSE;
}

//
BOOL CVariable::HasSameDef(CDrawObj* pObj)
{
	if (pObj->HasSameDef(this))
		return TRUE;
	
	return FALSE;
}


BOOL CVariable::IsValid()
{
	CString str;
	CFBDDoc* pDoc = ((CFBDDoc*)theApp.GetDocument());

	// Check if name in use for predefined variable (time point)
	CString strDef = pDoc->GetMatchingPredef(m_strName);
	if (! strDef.IsEmpty()) {
		str.Format(IDS_LABEL_PREDEFINED, m_strName, strDef);
		theApp.DoInfoMessage(str);
		return FALSE;
	}

	CVariable* pVar = pDoc->GetMatchingVar(this, TRUE/*bMatchName*/);
	if (pVar != NULL){
		if (pVar->m_status == statusError)
		{
			str.Format(IDS_REPLACE_BADVAR, m_strName, pVar->m_strDef);
			if ((theApp.DoInfoMessage(str, MB_OKCANCEL)) == IDCANCEL)
				return FALSE;
			else{
		/*		if (pDoc != NULL)
				{
					pDoc->UpdateAllViews(NULL, HINT_DELETE_VARIABLE, pVar);
					pDoc->RemoveVariable(pVar);
				}
				pVar->NotifyDelete();
				pVar->Delete();*/
				return TRUE;
			}
		}
		else{
			str.Format(IDS_LABEL_NOTUNIQUE, m_strName, pVar->m_strDef);
			theApp.DoInfoMessage(str);
			return FALSE;
		}
	}
	pVar = pDoc->GetMatchingVar(this, FALSE/*bmatchDef*/);
	if (pVar != NULL)
	{
		str.Format(IDS_SAMEDEF_VAR, m_strDef, pVar->m_strName);
		theApp.DoInfoMessage(str);
		return FALSE;
	}
	CDrawObj* pObj = pDoc->GetMatchingObj(this, TRUE/*bMatchName*/);
	if (pObj!= NULL)
	{
		if (pObj->HasSameDef(this)){
			return TRUE;
		}
		else
		{
			CString def = pObj->GetDef();
			/*if ( (pObj->IsKindOf(RUNTIME_CLASS(CSystem))) && (pObj->m_strName[0] != 'm') )
				def = "body" + def.Mid(7); */
			str.Format(IDS_LABEL_NOTUNIQUE, m_strName, def);
			theApp.DoInfoMessage(str);
			return FALSE;
		}
	}
	pObj = pDoc->GetMatchingObj(this, FALSE /*bMatchDef*/); 
	if (pObj != NULL){
		CString strName = pObj->m_strName;
		if (pObj->IsKindOf(RUNTIME_CLASS(CSystem)))
			//defining a mass variable, overrides system def
			return TRUE;
			/*strName = "m" + strName;*/
		str.Format(IDS_REDEFINE_VAR, strName, m_strDef);
		theApp.DoInfoMessage(str);
		return FALSE;
	}
	return TRUE;

}

void CVariable::UpdateObj(CDrawObj* pObj)
{
	ASSERT_KINDOF(CVariable, pObj);
	CVariable* pTempVar = (CVariable*)pObj;
	// type code may change in dialog if changed angular/linear
	m_nType = pTempVar->m_nType;
	m_strName = pTempVar->m_strName;
	m_strQuantName = pTempVar->m_strQuantName;
	m_strDef = pTempVar->m_strDef;
	m_strForceType = pTempVar->m_strForceType;
	m_strObject = pTempVar->m_strObject;
	m_strTime = pTempVar->m_strTime;
	m_strAgent = pTempVar->m_strAgent;
	m_status = pTempVar->m_status;
	m_strValue = pTempVar->m_strValue;

	/*
			// Log them all for good measure
	LogEventf(EV_PROPS_VARIABLE, "%s name |%s| value |%s| type |%s| body |%s| agent |%s|" , 
				 m_strId,  m_strName, m_strQuantName, m_strForceType,  m_strObject,  m_strAgent);

*/
}

void CVariable::UpdateVarNames(CString strOldName)
{
	
	if (strOldName.IsEmpty())
		strOldName = m_strName;

	RemoveVarNames(strOldName);

	// [doesn't happen any more since now all vectors must be drawn]
	// update component names for x-y plane vector variable. 
	if ( (m_nType == ID_VARIABLE_ADDACCELERATION) 
		|| (m_nType == ID_VARIABLE_ADDVELOCITY)
		|| (m_nType == ID_VARIABLE_ADDFORCE) 
		|| (m_nType == ID_VARIABLE_ADDDISPLACEMENT)
		|| (m_nType == ID_VARIABLE_ADDMOMENTUM)
		|| (m_nType == ID_VARIABLE_ADDIMPULSE)
		|| (m_nType == ID_VARIABLE_ADDUNITVECTOR)
		|| (m_nType == ID_VARIABLE_ADDRELPOS)
		|| (m_nType == ID_VARIABLE_ADDEFIELD))
	{
		if (m_pDocument && m_pDocument->IsAxesDrawn())
		{
			CString strNewXVar = m_strName + "_x";
			CString strNewYVar = m_strName + "_y";
			((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(strNewXVar);
			((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(strNewYVar);
		}
	}
	((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(m_strName);
}

void CVariable::RemoveVarNames(CString strOldName)
{
	RemoveVarName(strOldName);
	
	// [doesn't happen any more since now all vectors must be drawn]
	// update component names for x-y plane vector variable. 
	if ( (m_nType == ID_VARIABLE_ADDACCELERATION) 
		|| (m_nType == ID_VARIABLE_ADDVELOCITY)
		|| (m_nType == ID_VARIABLE_ADDFORCE) 
		|| (m_nType == ID_VARIABLE_ADDDISPLACEMENT)
		|| (m_nType == ID_VARIABLE_ADDMOMENTUM)
		|| (m_nType == ID_VARIABLE_ADDIMPULSE)
		|| (m_nType == ID_VARIABLE_ADDRELPOS)
		|| (m_nType == ID_VARIABLE_ADDEFIELD)
		|| (m_nType == ID_VARIABLE_ADDUNITVECTOR) 
	   )
	{
		CString strOldXVar = strOldName + "_x";
		CString strOldYVar = strOldName + "_y";
		RemoveVarName(strOldXVar);
		RemoveVarName(strOldYVar);
	}
}










