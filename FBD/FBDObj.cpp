///////////////////////////////////////////////////////////////////////////
//
// FBDObj.cpp -- Drawing objects used in Free Body Diagrams
//
// Implements CVector, CAxes, CSystem, CAngle
//
/////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <math.h>
    
#include "FBD.h"
#include "history.h"
#include "HelpIfc.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "FBDView.h"
#include "EXView.h"
#include "VarView.h"
#include "MainFrm.h"
#include "Motion.h"			// needed for vectors contained in motion diagrams
#include "GreekOpts.h"

// Property dialogs
#include "VecDlg.h"
#include "VecAVDlg.h"
#include "VecCpDlg.h"
#include "TorqueDlg.h"
#include "VecPosDlg.h"
#include "AxesDlg.h"
#include "AngleDlg.h"
#include "SysDlg.h"
#include "LabRadDlg.h"
#include "RelVelDlg.h"
#include "FieldDlg.h"
#include "ImpulseDlg.h"
#include "UnitVectorDlg.h"
    
//////////////////////////////////////////////////////////////////////////
// Vectors
//
// Vector is defined to be from m_position.TopLeft() to BottomRight()
// Note this means m_position need not be normalized and is not a true
// bounding box.
//
// Currently use one class for all vector types.
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CVector, CCheckedObj, VERSIONABLE_SCHEMA | 10);
CVector::CVector()			//used by serialization only
   	: CCheckedObj()
{ 
	m_nVectorType = VECTOR_FORCE;
	m_bAngular = FALSE;
	m_bDecomposed = FALSE;
	m_posLabel= CRect(0, 0, 0, 0);
	m_nZDir = ZDIR_NONE;
//	m_pAngle = NULL;
}					
    
CVector::CVector(const CRect& position)
   	:CCheckedObj(position)
{	
   	m_bDecomposed = FALSE;
   	m_nVectorType = VECTOR_FORCE; // initial default
	m_bAngular = FALSE;
	m_posLabel = CRect(0, 0, 0, 0);
	m_nZDir = ZDIR_NONE;
//   	m_pAngle = NULL;
}
    
CVector::~CVector()
{
}
    
void CVector::Serialize(CArchive& ar)
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
	if (ar.IsStoring())		// store object specific data
	{
		// version 1 info:
		ar << (WORD) m_nVectorType;
		ar << m_strBody; 
		ar << m_strAgent;
		ar << (WORD) m_bDecomposed;
		// added in version 2
		ar << (WORD) m_status;
		ar << m_strOrientation;
		ar << m_strTime;
		// put out stype specific fields (urgh, should just dump them always)
		// if we read a vector of that type from a file, file should have saved the appropriate subtype 
		// info as well. 
		if ((m_nVectorType == VECTOR_FORCE)
			|| (m_nVectorType == VECTOR_ACCELERATION) // added v7 for vel, accel
			|| (m_nVectorType == VECTOR_VELOCITY)
			|| (m_nVectorType == VECTOR_TORQUE) 	  // added new case v9 for torque
			|| (m_nVectorType == VECTOR_UNITVECTOR))  // added new case v10 for unit vector
			ar << m_strForceType;					  // OK, no saved torques < v9
		else if (m_nVectorType == VECTOR_COMPONENT){
			ar << m_strCompDir;
			ar << m_strCompOf;
		}
		m_Angles.Serialize(ar);
		//added version 6
		if (m_nVectorType != VECTOR_COMPONENT)
			m_Comps.Serialize(ar);
		// added version 8
		ar << (WORD) m_nZDir;
		// added version 9
		ar << (WORD) m_bAngular;
	} 
	else // load object specific data
	{
		WORD wTemp;
		CAngle* wpTempAngle;
		if (nVersion == 1) {
			ar >> wTemp; m_nVectorType = (int) wTemp;
			ar >> m_strBody;
			ar >> m_strAgent;
			ar >> wTemp; m_bDecomposed = (BOOL) wTemp;
		} else if (nVersion == 2) {
			ar >> wTemp; m_nVectorType = (int) wTemp;
			ar >> m_strBody;
			ar >> m_strAgent;
			ar >> wTemp; m_bDecomposed = (BOOL) wTemp;
			ar >> wTemp; m_status = (Status) wTemp; // !!! ensure valid
			if (nDocVersion <= 3)//changed order of enumeration in doc version 4
			{
				if(m_status == statusError)
					m_status = statusUnknown;
				else if (m_status == statusUnknown)
					m_status = statusCorrect;
				else //m_status == statusCorrect
					m_status = statusError;
			}
		}else if (nVersion == 3){
			ar >> wTemp; m_nVectorType = (int) wTemp;
			ar >> m_strBody;
			ar >> m_strAgent;
			ar >> wTemp; m_bDecomposed = (BOOL) wTemp;
			ar >> wTemp; m_status = (Status) wTemp; // !!! ensure valid
			ASSERT(nDocVersion > 3);
			ar >> wpTempAngle;//used to be m_pAngle
		}else if (nVersion == 4){
			ar >> wTemp; m_nVectorType = (int) wTemp;
			ar >> m_strBody;
			ar >> m_strAgent;
			ar >> wTemp; m_bDecomposed = (BOOL) wTemp;
			ar >> wTemp; m_status = (Status) wTemp; // !!! ensure valid
			ASSERT(nDocVersion > 3);
			ar >> wpTempAngle;//used to be m_pAngle
			ar >> m_strOrientation;
			ar >> m_strTime;
			if (m_nVectorType == VECTOR_FORCE)
				ar >> m_strForceType;
			else if (m_nVectorType == VECTOR_COMPONENT){
				ar >> m_strCompDir;
				ar >> m_strCompOf;
			}
		}else if (nVersion >= 5){
			ar >> wTemp; m_nVectorType = (int) wTemp;
			ar >> m_strBody;
			ar >> m_strAgent;
			ar >> wTemp; m_bDecomposed = (BOOL) wTemp;
			ar >> wTemp; m_status = (Status) wTemp; // !!! ensure valid
			ASSERT(nDocVersion > 3);
			ar >> m_strOrientation;
			ar >> m_strTime;
			if (m_nVectorType == VECTOR_FORCE)
				ar >> m_strForceType;
			else if (m_nVectorType == VECTOR_COMPONENT){
				ar >> m_strCompDir;
				ar >> m_strCompOf;
			}
			else if ( ((m_nVectorType == VECTOR_ACCELERATION)
						|| (m_nVectorType == VECTOR_VELOCITY)
						|| (m_nVectorType == VECTOR_TORQUE))
					&& (nVersion >= 7) ) // avg/inst for accel, vel; Net/Ind for torque
				ar >> m_strForceType;
			else if ( (m_nVectorType == VECTOR_UNITVECTOR)
				     && (nVersion >= 10) )
			    ar >> m_strForceType;

			m_Angles.Serialize(ar);
			
			if (nVersion >= 6) {
				if (m_nVectorType != VECTOR_COMPONENT)
					m_Comps.Serialize(ar);
			}
			if (nVersion >= 8) {
				ar >> wTemp; m_nZDir = (int) wTemp;
			} 
			if (nVersion >= 9) {
				ar >> wTemp; m_bAngular = (BOOL) wTemp;
			}
		}else {		// assume some earlier version, nothing saved
			m_bDecomposed = FALSE;
			m_nVectorType = VECTOR_FORCE;
			m_nZDir = ZDIR_NONE;
			// note strings initted to empty by default member-wise constructor
		}
		
	}
}
  
#ifdef _DEBUG
void CVector::Dump(CDumpContext& dc) const
{
	CDrawObj::Dump(dc);
	// dc << "Name: " << m_strName << "\n";
}
#endif // _DEBUG
    
const char *CVector::c_szAllSources = "all sources";

// Encapusaltes threshhold we use to decide if user drew zero-length object:
BOOL CVector::IsZeroMag()
{
#define MIN_SIZE 5			// Minimum dimension, logical units.
    
	return  (abs(m_position.Width()) <= MIN_SIZE && 
		     abs(m_position.Height()) <= MIN_SIZE);
}
    
// Return drawn magnitude in logical units.
int CVector::GetDrawnMagnitude()
{
	if (IsZeroMag() || IsZAxisVector())
		return 0;

	return (int) sqrt(m_position.Width() * m_position.Width()
					  + m_position.Height() * m_position.Height());
}

// Compute arrowhead coordinates. Trigonometry adapted from Joel
// Martin's OLAE interface code, with slightly different parameters. 
void CVector::CalcArrowPts(CPoint ptFrom, CPoint ptTo, int lineWidth, 
						   CPoint& ptEndShaft, CPoint arrowhead[3])
{
	// Line goes from ptFrom = (x1, y1) to ptTo = (x2, y2)
	int x1 = ptFrom.x; int y1 = ptFrom.y;  
	int x2 = ptTo.x; int y2 = ptTo.y;
	int dx = x2 - x1;					// slope of line is dy/dx
	int dy = y2 - y1;
	
	// Width and height of arrowhead is function of line width. Parameter constants
	// are in Logical units = 96ths of a logical inch.
	int height = lineWidth + 10;				// height of arrowhead triangle
	double halfbase = (lineWidth / 2.0) + 6.0;	// half base of arrowhead

	double hyp = sqrt(height * height - halfbase * halfbase);
	double phi = asin(halfbase / hyp);
	double theta = atan2(fabs((double) dy), fabs((double) dx));
	double alpha = theta + phi;
	double beta = theta - phi;
	int xa = (int) (hyp * cos(alpha));
	int ya = (int) (hyp * sin(alpha));
	int xb = (int) (hyp * cos(beta));
	int yb = (int) (hyp * sin(beta));
	// Calc endpoint of the shaft part of the arrow 
	int xc = (int) floor ((xa + xb) / 2);
	int yc = (int) floor ((ya + yb) / 2);
	int xsign = (x2 >= x1) ?  1 : -1;
	int ysign = (y2 >= y1) ?  1 : -1;
	ptEndShaft = CPoint(x2 - xsign * xc, y2 - ysign* yc);

	// Fill in polygon with points for arrowhead triangle w/apex at x2, y2
	arrowhead[0] = CPoint(x2, y2);
	arrowhead[1] = CPoint(x2 - (xsign * xa), y2 - (ysign * ya));
	arrowhead[2] = CPoint(x2 - (xsign * xb), y2 - (ysign * yb));	
} 

// Calc quadrilateral bounding a wide line
void CVector::CalcLinePts(CPoint ptFrom, CPoint ptTo, int lineWidth, 
						   CPoint linePoints[4])
{
	int x1 = ptFrom.x; int y1 = ptFrom.y;  
	int x2 = ptTo.x; int y2 = ptTo.y;
	int dx = x2 - x1;					// slope of line is dy/dx
	int dy = y2 - y1;
	double angle= atan2((double)dy,(double)dx);

	// Compute line polygon coordinates.  
	int xShift=(int)(lineWidth*sin(angle));
	int yShift=(int)(lineWidth*cos(angle));
	linePoints[0]=CPoint(x1-xShift, y1+yShift);
	linePoints[1]=CPoint(x1+xShift, y1-yShift);
	linePoints[2]=CPoint(x2+xShift, y2-yShift);
	linePoints[3]=CPoint(x2-xShift, y2+yShift);
} 

void CVector::Draw(CDC* pDC)
{	
	// Z-Axis vector is special case
	if (this->IsZAxisVector()) {
		DrawZAxisVector(pDC);
		return;
	}

	// Vector goes from ptFrom = (x1, y1) to ptTo = (x2, y2)
	CPoint ptFrom = m_position.TopLeft();
	CPoint ptTo  = m_position.BottomRight();
	
	// Calculate points we need:
	// We set different line widths to show differences between vector types
	int lineWidth = nVelocityWidth;		// default value used for most vectors
	if (m_nVectorType == VECTOR_COMPONENT)
		lineWidth = nComponentWidth;	// narrower lines for components
	else if (m_nVectorType == VECTOR_FORCE) 
		lineWidth = nVectorWidth;		// wider lines for forces

	CPoint ptEndShaft;	
	CPoint arrowhead[3];
	CalcArrowPts(ptFrom, ptTo, lineWidth, ptEndShaft, arrowhead);

	// Now do the drawing:
	
	// First draw the shaft of the arrow (drawn as wide line)
	CPen penVector;
	if (! penVector.CreatePen(PS_INSIDEFRAME, lineWidth, StatusColor()))
			return ;
	CPen* pOldPen = pDC->SelectObject( &penVector );
	pDC->MoveTo(ptFrom.x, ptFrom.y);
	pDC->LineTo(ptEndShaft.x, ptEndShaft.y);

	// Now draw the arrowhead: 
	CPen penArrow;
	if (! penArrow.CreatePen(PS_INSIDEFRAME, nArrowLineWidth, StatusColor()))
			return;
	(void) pDC->SelectObject( &penArrow );	// original pen saved above
	// We distinguish accelerations from others (velocities) by using filled arrowhead
	CBrush brushSolid, *pOldBrush;
	if (m_nVectorType == VECTOR_ACCELERATION) {		
		brushSolid.CreateSolidBrush(StatusColor());
		pOldBrush = pDC->SelectObject(&brushSolid);
	}

	if (!IsZeroMag())			// don't draw arrowhead for zero-length vectors
		pDC->Polygon(arrowhead, 3);

	if (m_nVectorType == VECTOR_ACCELERATION) 
		 pDC->SelectObject(pOldBrush);

	// add mark if vector has been labelled resolved
	if (m_bDecomposed) 
		DrawMark(pDC, lineWidth);
	
	// Now draw the label. 
	DrawLabel(pDC);
	
	// finished drawing
	pDC->SelectObject(pOldPen);

	// As side effect of drawing, update cached GDI region for use when hit-testing.
	// Hit-test region is slightly wider than the line to make it easier to select.
	CPoint linePoints[4];
	CalcLinePts(ptFrom, ptTo, lineWidth + 2, linePoints);
	// assemble region into vectorRgn from parts: 
	m_vectorRgn.DeleteObject();		// free any previous region (safe if none)
	m_vectorRgn.CreatePolygonRgn(linePoints, 4, ALTERNATE);
	// Merge in arrowhead region 
	CRgn arrowRgn;					// Temp (destructor frees GDI obj at block end) 
	arrowRgn.CreatePolygonRgn(arrowhead, 3, ALTERNATE); 
	m_vectorRgn.CombineRgn(&arrowRgn, &m_vectorRgn, RGN_OR); 
}
    
void CVector::DrawMark(CDC* pDC, int lineWidth)
{
	int cx = m_position.left + m_position.Width()/2;
	int cy = m_position.top + m_position.Height()/2;
	// simple means of putting X at midpoint to mark decomposed vectors
	// !!! hard to see when same color as vectpr
	CPen penMark;
	if (! penMark.CreatePen(PS_SOLID, lineWidth, RGB(0,0,0)))
		return;
	(void) pDC->SelectObject( &penMark ); 
	pDC->MoveTo(cx - 5, cy - 5);
	pDC->LineTo(cx + 5, cy + 5);
	pDC->MoveTo(cx -5, cy + 5);
	pDC->LineTo(cx +5, cy - 5);
}
    
#if 0
// Sets m_posLabel origin based on vector pos; extent based on text.
void CVector::PlaceLabel(CDC* pDC)
{
	CString strLabel = m_strName;
	// We add "=0" to the label when displaying to show degenerate NULL vectors.
	if (IsZeroMag())
		strLabel += "=0";

	// We save label bounding box for hit testing. Need to recalc even if empty
	// Note this member var not valid until vector is drawn
	
	// Quick and dirty label placement computations:
	// Place label offset from vector midpoint
	int xMid = m_position.left + m_position.Width() /* /2 */ ;
	int yMid = m_position.top + m_position.Height() /* /2 */ ;
	int xsign = (m_position.Width() >= 0) ? 1 : -1;
	int ysign = (m_position.Height() >= 0) ? 1 : - 1;
	CSize extText = pDC->GetTextExtent(strLabel);
	const int delta = 6;
	
	if (xsign == 1 && ysign == -1) { 
		m_posLabel = CRect( CPoint(xMid + delta,  
			                   yMid + delta), extText);
	} 
	else if (xsign == 1 && ysign == 1) {
		m_posLabel = CRect( CPoint(xMid + delta,  
			                       yMid - (delta + extText.cy)), extText);
	} 
	else if (xsign == -1 && ysign == 1) {
		m_posLabel = CRect( CPoint(xMid - (delta + extText.cx),  
    			                  yMid - (delta + extText.cy)), extText);
	} 
	else if (xsign == -1 && ysign == -1) {
		m_posLabel = CRect( CPoint(xMid - (delta + extText.cx),  
			                       yMid + delta), extText);
	}
	/* // Place offset a bit from endpoint
	m_posLabel = CRect(CPoint(m_position.right + 10, m_position.bottom + 10),
			       pDC->GetTextExtent(m_strName)); */
	/*// Place offset right and down from midpoint
	m_posLabel = CRect(CPoint(m_position.left + m_position.Width()/2, 
							  m_position.left + m_position.Width()/2),
							pDC->GetTextExtent(strLabel)); */
}
#endif 
    
void CVector::DrawLabel(CDC* pDC)
{
	CString strLabel = m_strName;
	// To show degenerate NULL vectors, we add "=0" to the label when drawn.
	// The label is the main visible representation of a zero-length vector.
	if (IsZeroMag())
		strLabel += "=0";
	else if (m_nVectorType == VECTOR_UNITVECTOR) // add =1 to unit vectors
		strLabel += "=1";

	// Recalc label position from vector position, caching bounding box in member var 
	// for use in hit testing. Need to update even if empty. Note this member not valid 
	// until vector is drawn.
	
	// don't draw anything if no label assigned yet
	if (strLabel.IsEmpty()) {
		m_posLabel.SetRectEmpty(); 
		return;
	}

	// else have non-empty label:
	CSize extText = pDC->GetTextExtent(strLabel);	// may contain Greek tags !!!

	// For special z-axis or zero-length vector symbols:
	// just use old code so no change. !!!Simpler code should be possible. 
	// This makes label offset depend on orientation of bounding box, which is somewhat
	// arbitrary for zero-length. 
	if (IsZeroMag() || IsZAxisVector()) 
	{
		// Quick and dirty label placement computations:
		// Formerly: placed label slight offset from vector midpoint
 		// Now, place near arrowhead. Offset y towards horizontal, x away from endpoint
		int xMid = m_position.left + m_position.Width() /* /2 */ ;
		int yMid = m_position.top + m_position.Height() /* /2 */ ;
		int xsign = (m_position.Width() >= 0) ? 1 : -1;
		int ysign = (m_position.Height() >= 0) ? 1 : - 1;
		const int delta = 6;			// offset from point on vector line.
		if (xsign == 1 && ysign == -1) { 
			m_posLabel = CRect( CPoint(xMid + delta,  
									   yMid + delta), extText);
		} 
		else if (xsign == 1 && ysign == 1) {
			m_posLabel = CRect( CPoint(xMid + delta,  
									   yMid - (delta + extText.cy)), extText);
		} 
		else if (xsign == -1 && ysign == 1) {
			m_posLabel = CRect( CPoint(xMid - (delta + extText.cx),  
    									   yMid - (delta + extText.cy)), extText);
		} 
   		else if (xsign == -1 && ysign == -1) {
   			m_posLabel = CRect( CPoint(xMid - (delta + extText.cx),  
   									   yMid + delta), extText);
   		}
		
	} 
	else // normal vector with an arrow
	{
		// new: center text on vector midpoint. Note text bounding box can include a lot
		// of space on top, if chars don't ascend much, so can appear off center.
		// !!! should make sure there is enough room for label -- very small arrow can
		// get completely obscured!
		int xMid = m_position.left + m_position.Width()/2 ;
		int yMid = m_position.top + m_position.Height()/2 ;
		m_posLabel = CRect(CPoint(xMid - extText.cx/2, yMid - extText.cy/2),
			               extText);
	}
 
#if 0 // seems fewer overlaps if offset from vector line some.
	// position using Axis label placement method. 
 	CPoint ptText = CAxes::GetLabelPos(CPoint(xMid, yMid), 
 						m_position.Width(), m_position.Height(), extText);
 	m_posLabel = CRect(ptText, extText);
#endif
 
   	/* // Place offset a bit from endpoint
	m_posLabel = CRect(CPoint(m_position.right + 10, m_position.bottom + 10),
			               pDC->GetTextExtent(m_strName)); */
   	/*// Place offset right and down from midpoint
   	m_posLabel = CRect(CPoint(m_position.left + m_position.Width()/2, 
   							  m_position.left + m_position.Width()/2),
   							pDC->GetTextExtent(strLabel)); */
   	if (! strLabel.IsEmpty()) 
   	{
   		COLORREF oldColor;
		// Draw NULL vector labels in color to provide feedback. Otherwise,
		// draw label in black and show status by shaft color only, for legibility.
		if (IsZeroMag()) oldColor = pDC->SetTextColor(StatusColor()); 

		// change to draw opaque for legibility over vector shaft
		//int oldMode = pDC->SetBkMode(TRANSPARENT);

		// Need special function if it contains greek letters
		if (strLabel.Find('$') != -1) {
			CGreekText::DrawText(pDC, m_posLabel, strLabel);
		} else
			pDC->TextOut(m_posLabel.left, m_posLabel.top, strLabel);
	
		//pDC->SetBkMode(oldMode); 
		if (IsZeroMag()) pDC->SetTextColor(oldColor); 
	}
}

void CVector::DrawZAxisVector(CDC *pDC)
{
	// First draw hollow circular boundary
	CPen penVector;
	if (! penVector.CreatePen(PS_INSIDEFRAME, 2, StatusColor()))
			return ;
	CBrush brNull;
	brNull.CreateStockObject(NULL_BRUSH);
	CPen* pOldPen = pDC->SelectObject( &penVector );
	CBrush* pOldBrush = pDC->SelectObject(&brNull);
	
	pDC->Ellipse(m_position);
	
	// Side effect: update cached region used for hit testing.
	m_vectorRgn.DeleteObject();
	m_vectorRgn.CreateEllipticRgnIndirect(&m_position);

	int xMid = m_position.left + m_position.Width()/2; 
	int yMid = m_position.top + m_position.Height()/2;
	if (m_nZDir == ZDIR_INTO) {
		// draw X in circle (tail of arrow)
		int d = abs(m_position.Width()/4);	// offset of x from midpoint
		pDC->MoveTo(xMid - d, yMid - d);
		pDC->LineTo(xMid + d, yMid + d);
		pDC->MoveTo(xMid - d, yMid + d);
		pDC->LineTo(xMid + d, yMid - d);
	} else if (m_nZDir == ZDIR_OUTOF) {
		// draw dot as small circle in center of circle (head of arrow)
		CBrush brushSolid(StatusColor());
		pDC->SelectObject(&brushSolid);
		pDC->Ellipse(xMid-2, yMid-2, xMid+2, yMid+2);
	}

	// Now draw the label
	DrawLabel(pDC);

	// Restore dc state
	pDC->SelectObject(pOldBrush);
	pDC->SelectObject(pOldPen);
}

void CVector::SetZDir(int nZDir)
{
	// remember if it was originally a Z-Axis vector
	BOOL bWasZAxisVector = IsZAxisVector();
	// update the state
	m_nZDir = nZDir;

	// if changed between nonZDir and ZDir, also update position rect to reflect
	if (bWasZAxisVector && !IsZAxisVector())		// Z to non-Z
	{		
		Invalidate();
		// make it zero-length at old origin 
		m_position.right = m_position.left;
		m_position.bottom = m_position.top;
	} 
	else if (!bWasZAxisVector && IsZAxisVector()) // non-Z to Z
	{
		Invalidate();
		// center dot on old origin (is endpoint more natural?)
		int x = m_position.left;
		int y = m_position.top;
		const int r = ZVEC_RADIUS;
		m_position = CRect(x-r, y-r, x+r, y+r);
	}

	// redraw new vector
	Invalidate();
}
  
void CVector::DrawSelectState(CDC* pDC, TrackerState state)
{
	if (state == normal)	// not selected, nothing to do
		return;

	// Only draw handles for non-NULL vectors:
	if (! IsZeroMag() )
		CDrawObj::DrawSelectState(pDC, state);

	// Highlight label with focus rect to show selection.
	pDC->DrawFocusRect(m_posLabel);
}
    
// 
// This function was used by select tool for hit testing,
// called from CFBDDoc::ObjectAt(). But we changed things to use 
// HitTest for that, so now only used for net select testing.
//
BOOL CVector::Intersects(const CRect& rect)
// rect in logical coordinates. 
{
	if (CDrawObj::Intersects(rect))
		return (TRUE);
	
	// else check if hit on vector label 
	if (!m_strName.IsEmpty()) {
		CRect fixed = m_posLabel;
		fixed.NormalizeRect();
		CRect rectT = rect;
		rectT.NormalizeRect();
		return !(rectT & fixed).IsRectEmpty();
	}
	// get here if no label
	return (FALSE);
}
    
int CVector::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
	if (bSelected) {// testing for resize handle grab: 
		  // do the work in base class
		  int nHit = CDrawObj::HitTest(point, pView, bSelected);
		  // if hit handle of zero-length vector, always return handle 2,
		  // so resizing it will drag out endpoint, not origin
		  if (nHit && IsZeroMag()) 
			  nHit = 2;
		  return nHit;
	}

	// test if hit vector arrow:
	if (! IsZeroMag()) // do only for non-NULL vectors
	{
		// Ensure region exists and vector hasn't been resized to a point
		if (m_vectorRgn.m_hObject == NULL)
			// ??? following was done in existing code, but not clear it's correct -AW
			return CDrawObj::HitTest(point, pView, bSelected);//handle it in base class
		
		// test if hit vector region
		if (m_vectorRgn.PtInRegion(point))
			return TRUE;
	}
    	
	// else check if hit vector label
	if (!m_strName.IsEmpty()) {
		CRect posLabel = m_posLabel;
		posLabel.NormalizeRect();
		if (posLabel.PtInRect(point))
			return TRUE;
	}
    	
	return FALSE;	// if got to here => no hit.
}
    
CRect CVector::GetBoundingBox()
{
	// !!! if valid, use cached regions to compute bounding rect.
	// following is approximate, doesn't account for line width or arrowhead extension
	// base class returns fixed up normalized position rect
	return CDrawObj::GetBoundingBox() | m_posLabel;		// union of the rects
}
    
// Line-like objects such as vectors, guidelines, and axes, should have only two resize 
// handles. Following takes advantage of the fact that line handle #2 behaves the same 
// as generic draw obj handle #5. The code translates the handle id and delegates to 
// the generic implementation.
// !!! Might be worth it to put this in a common base class. For now it's repeated
// for each line-like object that can use this code.
int CVector::GetHandleCount()
{
	// Can't resize Z-Axis vectors, so don't show handles for them.
	if (IsZAxisVector()) return 0;

	return (2);
}
    
CPoint CVector::GetHandle(int nHandle)
{	
	return CDrawObj::GetHandle(nHandle == 2 ? 5 : nHandle);
}
    
HCURSOR CVector::GetHandleCursor(int nHandle)
{
	return CDrawObj::GetHandleCursor(nHandle == 2 ? 5 : nHandle);
}
    
void CVector::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{
		
	CDrawObj::MoveHandleTo(nHandle == 2 ? 5 : nHandle, point, pView);

	if (nHandle == 1)  
	{
		theApp.GetDocument()->DeleteObjList(&m_Angles);
	}
	else
		UpdateAngles();

}
    
//
// Hook in vector's MoveTo method updates status bar to show orientation
//
void CVector::MoveTo(const CRect& position, CFBDView* pView)
{
	CDrawObj::MoveTo(position, pView);

	// update angle status pane
	CMainFrame* pFrame = (CMainFrame*) AfxGetApp()->m_pMainWnd;
	CString str;
	str.Format("%d degrees", GetDirection());
	pFrame->SetAngleText(str);
}
    
//
// Property editing dialogs: For each type, we popup the appropriate dialog 
// to collect new properties. 
//
// !!! Now that we have so much sub-type-specific switches in the vector
// class, it is looking like it would be worth it to break it into sub-classes.
// for each type. Could also try inheritance on the dialog classes. 
// 
CDialog* CVector::GetPropertyDlg()
{
	if (m_nVectorType == VECTOR_FORCE)
		return new CVectorDlg(this);
	else if (m_nVectorType == VECTOR_VELOCITY ||
			 m_nVectorType == VECTOR_DISPLACEMENT ||
			 m_nVectorType == VECTOR_ACCELERATION ||
			 m_nVectorType == VECTOR_MOMENTUM) 
		return new CVectorMoveDlg(this);
	// else if (m_nVectorType == VECTOR_COMPONENT)  
	//	return new CVectorCompDlg(this);
	else if (m_nVectorType == VECTOR_IMPULSE) 
		return new CImpulseDlg(this);
	else if (m_nVectorType == VECTOR_TORQUE)
		return new CTorqueDlg(this);
	else if (m_nVectorType == VECTOR_POSITION)
		return new CVecPosDlg(this);
	else if (m_nVectorType == VECTOR_RELVEL)
		return new CRelVelDlg(this);
	else if (m_nVectorType == VECTOR_EFIELD)
		return new CFieldDlg(this);
	else if (m_nVectorType == VECTOR_BFIELD)
		return new CFieldDlg(this, /*bMagnetic*/TRUE);
	else if (m_nVectorType == VECTOR_UNITVECTOR)
		return new CUnitVectorDlg(this);

	// else fall through:
	TRACE("CVector::GetPropertyDlg:: Bad vector type d\n", m_nVectorType); 
	return NULL;
}

// Get definition string used in variable window & error messages
CString CVector::GetDef()
{
	CString strTimePart; // default empty
	if (!m_strTime.IsEmpty()){
		if (_stricmp(m_strForceType, "average")==0)
			strTimePart.Format("during %s", m_strTime);
		else
			strTimePart.Format("at time %s", m_strTime);
	}
	CString strBodyPart; // includes preposition. Why?
	if (!m_strBody.IsEmpty())
		strBodyPart.Format("of %s", m_strBody);
	// optional modifier prefix includes space, so empty prefix will be OK below
	CString strMod = m_bAngular ? "Angular " : "";
	CString strDef;
	if (m_nVectorType == VECTOR_FORCE) {
		if (!m_strBody.IsEmpty())
			strBodyPart.SetAt(1, 'n');	// change "of" to "on" in body part
		CString strAgentPart;
		if (!m_strAgent.IsEmpty())
			strAgentPart.Format("due to %s", m_strAgent);
		strDef.Format("%s Force %s %s %s",
		   	m_strForceType, strBodyPart, strTimePart, strAgentPart);
	} else if (m_nVectorType == VECTOR_VELOCITY) {
		strDef.Format("%s %sVelocity %s %s", m_strForceType, strMod, strBodyPart, strTimePart);
	} else if (m_nVectorType == VECTOR_ACCELERATION) {
		strDef.Format("%s %sAcceleration %s %s", m_strForceType, strMod, strBodyPart, strTimePart);
	} else if (m_nVectorType == VECTOR_DISPLACEMENT) {
		strDef.Format("%sDisplacement %s %s", strMod, strBodyPart, strTimePart);
	} else if (m_nVectorType == VECTOR_MOMENTUM) {
		strDef.Format("%sMomentum %s %s", strMod, strBodyPart, strTimePart);
	} else if (m_nVectorType == VECTOR_COMPONENT) {
		strDef.Format("Component of %s along %s", m_strCompOf, m_strCompDir);
	} else if (m_nVectorType == VECTOR_POSITION) {
		strDef.Format("Relative Position %s with respect to %s",  strBodyPart, m_strAgent);
	} else if (m_nVectorType == VECTOR_RELVEL) {
		strDef.Format("Relative Velocity %s with respect to %s",  strBodyPart, m_strAgent);
	} else if (m_nVectorType == VECTOR_TORQUE) {
		// Like force, may be Net or individual torque
			if (!m_strForceType.IsEmpty()) 	
			strDef.Format("Net Torque on %s about %s %s", m_strBody, m_strAgent, strTimePart);
		else
			strDef.Format("Torque about %s from force at %s %s",  m_strAgent, m_strBody, strTimePart);
	} else if (m_nVectorType == VECTOR_EFIELD) {
		strDef.Format("Electric Field at %s due to %s",  m_strBody, m_strAgent);
	} else if (m_nVectorType == VECTOR_BFIELD) {
		strDef.Format("Magnetic Field at %s due to %s",  m_strBody, m_strAgent);
	} else if (m_nVectorType == VECTOR_IMPULSE) {
		strDef.Format("Impulse on %s due to %s", m_strBody, m_strAgent);
	} else if (m_nVectorType == VECTOR_UNITVECTOR) {
		// type is "Normal" "Towards" "Away From"
		if (m_strForceType == "Normal") 
			strDef.Format("Unit vector normal to %s %s", m_strBody, strTimePart);
		else strDef.Format("Unit vector at %s pointing %s %s %s",
			                m_strBody, m_strForceType, m_strAgent, strTimePart);
	} else
		strDef = "Vector";
	strDef.TrimRight();	
	return strDef;
}

// Get definition string used on printed solution
// Main diff: we leave off "average" vs. "instantaneous" (redundant w/time pt. vs 
// interval) to save scarce space in column. Any others?
CString CVector::GetPrintDef()
{
	CString strTimePart; // default empty
	if (!m_strTime.IsEmpty())
		strTimePart.Format("at time %s", m_strTime);

    // optional modifier prefix includes space
	CString strMod = m_bAngular ? "Angular " : "";
	
	CString strDef;
	if (m_nVectorType == VECTOR_FORCE) {
		if (m_strAgent.IsEmpty())	// net force
			strDef.Format("%s Force on %s %s", m_strForceType, m_strBody, strTimePart);
		else
			strDef.Format("%s Force on %s due to %s %s",
		       	m_strForceType, m_strBody, m_strAgent, strTimePart);
	} else if (m_nVectorType == VECTOR_VELOCITY) {
		strDef.Format("%sVelocity of %s %s", strMod, m_strBody, strTimePart);
	} else if (m_nVectorType == VECTOR_ACCELERATION) {
		strDef.Format("%sAcceleration of %s %s", strMod, m_strBody, strTimePart);
    } else if (m_nVectorType == VECTOR_DISPLACEMENT) {
		strDef.Format("%sDisplacement of %s %s", strMod, m_strBody, strTimePart);
	} else if (m_nVectorType == VECTOR_MOMENTUM) {
		strDef.Format("%sMomentum of %s %s", strMod, m_strBody, strTimePart);
	} else if (m_nVectorType == VECTOR_COMPONENT) {
		strDef.Format("Component of %s along %s", m_strCompOf, m_strCompDir);
	} else if (m_nVectorType == VECTOR_POSITION) {
		strDef.Format("Relative Position of %s with respect to %s %s", m_strBody, m_strAgent, strTimePart);
	} else if (m_nVectorType == VECTOR_RELVEL) {
		strDef.Format("Relative Velocity of %s with respect to %s %s", m_strBody, m_strAgent, strTimePart);
	} else if (m_nVectorType == VECTOR_TORQUE) {
		if (!m_strForceType.IsEmpty()) 	
			strDef.Format("Net Torque on %s about %s %s", m_strBody, m_strAgent, strTimePart);
		else
			strDef.Format("Torque about %s from force at %s %s",  m_strAgent, m_strBody, strTimePart);
	} else if (m_nVectorType == VECTOR_EFIELD) {
		strDef.Format("Electric Field at %s due to %s %s", m_strBody, m_strAgent, strTimePart);
	} else if (m_nVectorType == VECTOR_BFIELD) {
		strDef.Format("Magnetic Field at %s due to %s %s", m_strBody, m_strAgent, strTimePart);
	} else if (m_nVectorType == VECTOR_IMPULSE) {
		strDef.Format("Impulse on %s due to %s %s", m_strBody, m_strAgent, strTimePart);
	} else 
		strDef = GetDef();
    
	// cleanup trailing white space 
	strDef.TrimRight();

	// add direction indication
	if (! IsZeroMag() ) {
		CString strDir = " (? deg)";	// shown if direction unset
		if 	(IsZAxisVector()) {
			strDir = CString(" (Z ") 
					   + (m_nZDir == ZDIR_OUTOF ? "0" : (m_nZDir == ZDIR_INTO ? "180" : "?"))
					   + ")";
		}
		else if (!UnknownDir())
			strDir.Format(" (%d deg)", GetDirection());

		strDef += strDir;
	}

	return strDef;
}

// Record entry state in single a log line
void CVector::LogEntry()
{
	CString strDrawObj = GetDrawObjLogPart();
	CString strRest;
	// note replay parsing wants vector type at beginning, since can't
	if (m_nVectorType == VECTOR_COMPONENT) {
		CString strCompDir = ValToArg(m_strCompDir);
		strRest.Format("%s %s %s", m_strCompOf, strCompDir, m_strOrientation);
	} else { // just log it all, some possibly empty
		CString strType = ValToArg(m_strForceType);
		CString strAgent = ValToArg(m_strAgent);
		CString strBody = ValToArg(m_strBody);
		CString strTime = ValToArg(m_strTime);
		// !! orientation may be empty or non-numeric string for unknown -- Bug thru 7.0.0
		CString strDir = ValToArg(m_strOrientation);
		// code Z Axis directions w/negative value (-1, -2, -3) in direction field..
		if (IsZAxisVector())
			strDir.Format("-%d", (int) m_nZDir);
		strRest.Format("%s %s %s %s %s %d", strType, strAgent, strBody, strTime, 
			strDir, m_bDecomposed);
	}
	LogEventf(EV_FBD_ENTRY, "%s %s", strDrawObj, strRest);
}

// Set object state from entry log line; inverse of LogEntry
BOOL CVector::SetFromLogStr(LPCTSTR pszStr)
{
	LPCSTR pszRest = ParseDrawObjLogPart(pszStr);
	if (pszRest == NULL) return FALSE;

	// Get vector type from base drawobj typename
	char szTypeName[32];
	sscanf(pszStr, "%s", szTypeName);
	CString strTypeName(szTypeName);
	
	// identify angular bit by "ang-" prefix in typename
	if (_strnicmp(strTypeName, "Ang-", 4) == 0) {
		m_bAngular = TRUE;
		// change to unmodified typename for following
		strTypeName = strTypeName.Mid(4);
	}
	
	if (strTypeName == "Force") m_nVectorType = VECTOR_FORCE;
	else if (strTypeName == "Velocity") m_nVectorType = VECTOR_VELOCITY;
	else if (strTypeName == "Acceleration") m_nVectorType = VECTOR_ACCELERATION;
	else if (strTypeName == "Component") m_nVectorType = VECTOR_COMPONENT;
	else if (strTypeName == "Displacement") m_nVectorType = VECTOR_DISPLACEMENT;
	else if (strTypeName == "Momentum") m_nVectorType = VECTOR_MOMENTUM;
	else if (strTypeName == "Impulse") m_nVectorType = VECTOR_IMPULSE;
	else if (strTypeName == "Torque") m_nVectorType = VECTOR_TORQUE;
	else if (strTypeName == "Position") m_nVectorType = VECTOR_POSITION;
	else if (strTypeName == "Relative-Vel") m_nVectorType = VECTOR_RELVEL;
	else if (strTypeName == "Relative-Velocity") m_nVectorType = VECTOR_RELVEL;
	else if (strTypeName == "E-field") m_nVectorType = VECTOR_EFIELD;
	else if (strTypeName == "B-field") m_nVectorType = VECTOR_BFIELD;
	else if (strTypeName == "Unit-Vector") m_nVectorType = VECTOR_UNITVECTOR;

	// set the CVector specific stuff 
	if (m_nVectorType == VECTOR_COMPONENT) {
		char szCompDir[80]; char szCompOf[80]; char szOrientation[10];
		if (sscanf(pszRest, "%s %s %s", szCompOf, szCompDir, szOrientation) != 3) return FALSE;
		m_strCompOf = szCompOf;
		m_strCompDir = ArgToVal(szCompDir);
		m_strOrientation = szOrientation;
		// !!! must find base vector objects and restore their links to components.
		// Possible they may not exist yet if objects out of order -- would have to
		// patch up after all objects added.
		CDrawObj* pObj = theApp.GetDocument()->FindByName(m_strCompOf);
		if (pObj && pObj->IsKindOf(RUNTIME_CLASS(CVector))) {
			((CVector*)pObj)->m_Comps.AddTail(this);
		}
	} else {
		char szForceType[80]; char szBody[80]; char szAgent[80];
		char szTime[80]; char szOrientation[10];
		int nArgs = sscanf(pszRest, "%s %s %s %s %s %d", szForceType, szAgent, szBody, szTime, 
								szOrientation, &m_bDecomposed);
		// workaround bug causing missing dir field if unknown thru Andes7.0.0
		if (nArgs != 6 && nArgs != 5) return FALSE;
		m_strForceType = ArgToVal(szForceType);
		m_strAgent = ArgToVal(szAgent);
		m_strBody = ArgToVal(szBody);
		m_strTime = ArgToVal(szTime);
		// m_strOrientation = ArgToVal(szOrientation);
		if (nArgs == 6) // we have a dir slot, not a buggy older log.  
		{
			// translate negative orientations (-1, -2, -3) into zdir codes
			if (szOrientation[0] == '-') {
				m_nZDir = atoi(&szOrientation[1]);
			} else
				m_strOrientation = ArgToVal(szOrientation);
		} 
		// else just leave m_strOrientation empty for unknown
	}
	return TRUE;
}
   
void CVector::GetTypeName(CString& strType)
{
	switch (m_nVectorType) 
	{
	case VECTOR_FORCE: strType = "Force"; break;
	case VECTOR_VELOCITY: strType = "Velocity"; break;
	case VECTOR_ACCELERATION: strType = "Acceleration"; break;
	case VECTOR_COMPONENT: strType = "Component"; break;
	case VECTOR_DISPLACEMENT: strType = "Displacement"; break;
	case VECTOR_MOMENTUM: strType = "Momentum"; break;
	case VECTOR_IMPULSE: strType = "Impulse"; break;
	case VECTOR_POSITION: strType = "Position"; break;
	case VECTOR_RELVEL: strType = "Relative-Vel"; break;
	case VECTOR_TORQUE: strType = "Torque"; break;
	case VECTOR_EFIELD: strType = "E-field"; break;
	case VECTOR_BFIELD: strType = "B-field"; break;
	case VECTOR_UNITVECTOR: strType = "Unit-Vector"; break;
	default: strType = "Vector"; break;
	}
	if (m_bAngular)
		strType = "Ang-" + strType;
}

CString CVector::GetLabelPrefix()
{
	switch (m_nVectorType) 
	{
	case VECTOR_FORCE: return  "F"; 
	case VECTOR_VELOCITY: return m_bAngular ? "$w" :"v"; 
	case VECTOR_ACCELERATION: return m_bAngular ? "$a" :"a"; 
	case VECTOR_COMPONENT: return ""; 
	case VECTOR_DISPLACEMENT: return m_bAngular ? "$q" :"d";
	case VECTOR_MOMENTUM: return m_bAngular ? "L" : "p";
	case VECTOR_IMPULSE: return "J";
	case VECTOR_POSITION: return "r";
	case VECTOR_RELVEL: return "v";
	case VECTOR_TORQUE: return "$t";
	case VECTOR_EFIELD: return "E";
	case VECTOR_BFIELD: return "B";
	case VECTOR_UNITVECTOR: return "n";
	default: return "v"; 
	}
}

// Test if given drawobj is different object with same definition
BOOL CVector::HasSameDef(CDrawObj* pObj)
{
	if (!pObj->IsKindOf(RUNTIME_CLASS(CVector)))
		return FALSE;

	CVector* pVec = (CVector*)pObj;
	if (strcmp(m_strId, pVec->m_strId)==0)
		return FALSE;//we are editing this object

	if (m_nVectorType == pVec->m_nVectorType &&
		m_bAngular == pVec->m_bAngular)		//check if same vector type
	{
		// for components, check def 
		if ( (m_nVectorType == VECTOR_COMPONENT)
			&& (_stricmp(m_strCompOf, pVec->m_strCompOf) == 0)//case insensitive
				&& (strcmp(m_strCompDir, pVec->m_strCompDir) == 0) )
			return TRUE;

		// check subtype where used (as forcetype or avg/inst)
		// (though avg/inst is redundant with time).
		if ((m_nVectorType == VECTOR_FORCE)||
			(m_nVectorType == VECTOR_ACCELERATION)||
			(m_nVectorType == VECTOR_VELOCITY)) {
			if (strcmp(m_strForceType, pVec->m_strForceType) != 0)
				return FALSE;
		}
		
		// check agent where used
		if ((m_nVectorType == VECTOR_FORCE || 
			m_nVectorType == VECTOR_TORQUE ||
			m_nVectorType == VECTOR_POSITION ||
			m_nVectorType == VECTOR_RELVEL ||
			m_nVectorType == VECTOR_EFIELD ||
			m_nVectorType == VECTOR_BFIELD)
			&& (strcmp(m_strAgent, pVec->m_strAgent) != 0) )
			return FALSE;
		
		// for rest check body and time
		if (m_nVectorType != VECTOR_COMPONENT) {
			if ( (strcmp(m_strBody, pVec->m_strBody) == 0)
					&& theApp.GetDocument()->IsMatchingTime(m_strTime, pVec->m_strTime) )
				return TRUE;
		}
	}
	return FALSE;
}

//Check if given variable has the same definition
BOOL CVector::HasSameDef(CVariable* pVar)
{
	// verify variable type matches this vector's type
	BOOL bMatchType = FALSE;
	switch (pVar->m_nType)
	{
	case ID_VARIABLE_ADDACCELERATION:
		bMatchType = (!m_bAngular && m_nVectorType == VECTOR_ACCELERATION); break; 
	case ID_VARIABLE_ADDVELOCITY:  
		bMatchType = (!m_bAngular && m_nVectorType == VECTOR_VELOCITY); break; 
	case ID_VARIABLE_ADDDISPLACEMENT:	
		bMatchType = (!m_bAngular && m_nVectorType == VECTOR_DISPLACEMENT); break; 
	case ID_VARIABLE_ADDFORCE: 
		bMatchType = (!m_bAngular && m_nVectorType == VECTOR_FORCE); break; 	 
	case ID_VARIABLE_ADDMOMENTUM:	
		bMatchType = (!m_bAngular && m_nVectorType == VECTOR_MOMENTUM) ; break; 
	case ID_VARIABLE_ADDANGACCELERATION:
		bMatchType = (m_bAngular && m_nVectorType == VECTOR_ACCELERATION); break; 
	case ID_VARIABLE_ADDANGVELOCITY:
		bMatchType = (m_bAngular && m_nVectorType == VECTOR_VELOCITY); break; 
	case ID_VARIABLE_ADDANGDISPLACEMENT: 
		bMatchType = (m_bAngular &&	m_nVectorType == VECTOR_DISPLACEMENT); break; 
	case ID_VARIABLE_ADDANGMOMENTUM:
		bMatchType = (m_bAngular && m_nVectorType == VECTOR_MOMENTUM); break;
	// if no angular flag, just match corresponding types
	case ID_VARIABLE_ADDIMPULSE:
		bMatchType = (m_nVectorType == VECTOR_IMPULSE); break;
	case ID_VARIABLE_ADDTORQUE:
		bMatchType = (m_nVectorType == VECTOR_TORQUE); break;
	case ID_VARIABLE_ADDUNITVECTOR:
		bMatchType = (m_nVectorType == VECTOR_UNITVECTOR); break;
	// etc. 
	}
	if (!bMatchType) return FALSE;

	// if vector a force, check for match on force type and agent
	if ( (m_nVectorType == VECTOR_FORCE) 
		  && ( (m_strForceType != pVar->m_strForceType)
			    || (m_strAgent != pVar->m_strAgent) ) )
				return FALSE;

	// check for match on body & time. (time determines avg/inst type).
	return ( (m_strBody == pVar->m_strObject)
		  && theApp.GetDocument()->IsMatchingTime(m_strTime, pVar->m_strTime) );
}

BOOL  CVector::HasSameDir(CDrawObj* pObj)
{
	ASSERT_KINDOF(CVector, pObj);
	CVector* pVec = (CVector*) pObj;

	return (pVec->m_nZDir == m_nZDir && 
		(IsZAxisVector() || pVec->m_strOrientation == m_strOrientation) );
}

void CVector::UpdateVarNames(CString strOldName)
{
	if (strOldName.IsEmpty())
		strOldName = m_strName;

	RemoveVarNames(strOldName);
	if (m_nVectorType != VECTOR_COMPONENT)
	{
		if (m_pDocument && m_pDocument->IsAxesDrawn())
		{
			CString strNewXVar = m_strName + "_x";
			CString strNewYVar = m_strName + "_y";
			((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(strNewXVar);
			((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(strNewYVar);
			if (m_pDocument->UseZAxis()) {
				CString strNewZVar = m_strName + "_z";
				((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(strNewZVar);
			}
		}
		// Add theta/phi var if it is unknown, so it shows on solve-for list
		CString strDirVar; 
		if (IsZAxisVector() && m_nZDir == ZDIR_UNKNOWN) {
			strDirVar = "$j" + m_strName;
		} else if (! IsZeroMag() && UnknownDir()) {
			strDirVar = "$q" + m_strName;
		}
		if (!strDirVar.IsEmpty())
			((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(strDirVar);		
	}
	((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(m_strName);
}

void CVector::RemoveVarNames(CString strOldName)
{
	RemoveVarName(strOldName);
	if (m_nVectorType != VECTOR_COMPONENT)
	{
		CString strOldXVar = strOldName + "_x";
		CString strOldYVar = strOldName + "_y";
		RemoveVarName(strOldXVar);
		RemoveVarName(strOldYVar);
		if (m_pDocument->UseZAxis()) {
			CString strOldZVar = strOldName + "_z";
			RemoveVarName(strOldZVar);
		}

		// Remove theta/phi in case it was added because unknown. Should be safe if
		// not already defined because was known or was zero-mag, say (except for
		// pathological case where student defined thetaV as something else!)
		CString strDirVar; 
		if (IsZAxisVector()) {
			strDirVar = "$j" + strOldName;
		} else {
			strDirVar = "$q" + strOldName;
		}
		RemoveVarName(strDirVar);
	}
}

// If vec's "body" is a defined system, get list of underlying bodies in it.
CString CVector::GetBodies() 
{
	CString str;
	CFBDDoc* pDoc = ((CFBDDoc*)theApp.GetDocument());
	POSITION pos = pDoc->m_objects.GetTailPosition();
	while (pos!=NULL)
	{
		CDrawObj* pObj=pDoc->m_objects.GetPrev(pos);
		if (pObj->m_flag == TEACHER_OBJECT)//want to skip our teacher drawn objects
			continue;
		if (pObj->IsKindOf(RUNTIME_CLASS(CSystem))){
			CSystem* pSys = (CSystem*)pObj;
			if (_stricmp(pSys->m_strName, m_strBody) == 0){//case insensitive
				str = pSys->m_strBodies;		
				str.TrimRight();
			}

		}
	}
	return str;
}

    
void CVector::CheckObject()
{
	if (! theApp.CheckDiagram()) return;

	LPCTSTR pszResult;
	if (m_nVectorType == VECTOR_FORCE) 
		pszResult = CheckForceVector();
	else if (m_nVectorType == VECTOR_COMPONENT)  
		pszResult = CheckCompVector();
	else if (m_nVectorType == VECTOR_TORQUE)
		pszResult = CheckTorqueVector();
#if 0 // handled as MoveVector:
	else if (m_nVectorType == VECTOR_POSITION)
		pszResult = CheckPositionVector();
#endif 0
	else if (m_nVectorType == VECTOR_UNITVECTOR)
		pszResult = CheckUnitVector();
	else
		pszResult = CheckMoveVector();	// all others only differ in vector type

	ApplyStatus(pszResult);
}

CString CVector::DirArg()	// returns direction arg as string for helpsys calls
{
	// special case for ZDIR vectors: code as negative numbers
	if (IsZAxisVector()) {
		if (m_nZDir == ZDIR_UNKNOWN) 
			return "-3";	// 
		return m_nZDir == ZDIR_OUTOF ? "-1" : "-2";
	}

	// else non ZDIR vector:
	if (UnknownDir())				// use NIL for unspecified orientation
		return "NIL";
	return m_strOrientation;	// should be non-empty number if known
}

CString CVector::MagArg()	// returns magnitude arg for helpsys calls (as string!)
{
	if (IsZAxisVector())
		return "NIL";
	
	CString strMag;
	strMag.Format("%d", GetDrawnMagnitude());
	return strMag;
}

LPCTSTR CVector::CheckForceVector()
{
	// Translate our ForceType strings to Lisp's
	CString strForceType;
	if (m_strForceType == "Weight")
		strForceType = "grav";
	else 
		strForceType = m_strForceType;
	
	// For Andes2: bracket body name to preserve case if compound body label.
	// (lookup-force label type object agent direction mag time id)
	return HelpSystemExecf( "(lookup-force \"%s\" |%s| |%s| %s %s %s |%s| %s)",
			STR2ARG(m_strName),					// label
			STR2ARG(strForceType),				// force type
			STR2ARG(m_strBody),					// object
			STR2ARG(m_strAgent),				// agent
			DirArg(),							// direction
			MagArg(),							// mag
			STR2ARG(m_strTime),					// time  
			m_strId								// id
		);
}
    
LPCTSTR CVector::CheckCompVector()
{
	// Note shouldn't be possible to create Z-Axis component vector (?)
	// !!! Is it possible to have unknown drawn component dirs? Seems yes,
	// if base vector is unknown. (Axis direction is always known, although
	// may not know sign of component). But m_strOrientation may not be set
	// correctly for components, since component dialog lacks direction slot
	// -- should be fixed!. For now send drawn direction.

	// (lookup-component label compo-of axis mag id dir)
	return HelpSystemExecf( "(lookup-component \"%s\" %s %s %d %d %s)",
		STR2ARG(m_strName),						// label
		STR2ARG(m_strCompOf),					// compo-of
		(m_strCompDir == "X axis") ? "X" : "Y", // axis
		GetDrawnMagnitude(),					// magnitude
		GetDirection(),							// direction
		m_strId									// id
		);
}
	
LPCTSTR CVector::CheckMoveVector()
{
	CString strType;
	GetTypeName(strType);
	
	// Hack since the API has no second body argument for position vectors for
	// origin stored in agent slot: send it in the subtype field used for
	// avg vs. instantaneous. (Actually there is no need to send the avg/inst
	// argument at all, assuming we enforce consistency on our side. Andes2 helpsys
	// ignores it. So we could use it for optional second body in all cases.
	// Possibly logfile processing code might use it somewhere, though.)
	CString strSubTypeArg;
	if (m_nVectorType == VECTOR_POSITION ||
		m_nVectorType == VECTOR_RELVEL ||
		m_nVectorType == VECTOR_EFIELD ||
		m_nVectorType == VECTOR_BFIELD ||
		m_nVectorType == VECTOR_IMPULSE)
		strSubTypeArg = m_strAgent;
	else
		strSubTypeArg = m_strForceType;

	// if net field, send special subtype of 'NIL
	if ((m_nVectorType == VECTOR_EFIELD || m_nVectorType == VECTOR_BFIELD) && 		
		m_strAgent == c_szAllSources)
			strSubTypeArg = "NIL";

	// For Andes2: bracket body name to preserve case if compound body label.
	//(lookup-vector label type object direction mag time)
	return HelpSystemExecf( "(lookup-vector \"%s\" %s %s |%s| %s %s |%s| %s)",
			STR2ARG(m_strName),				// label
			STR2ARG(strSubTypeArg),			// instantaneous or average
			strType,						// vector type
			STR2ARG(m_strBody),				// object
			DirArg(),						// direction
			MagArg(),						// mag
			STR2ARG(m_strTime),				// time
			m_strId							// id
		);
}
 
LPCTSTR CVector::CheckTorqueVector()
{
	// For Andes2: bracket body name to preserve case if compound body label.
	// (lookup-torque label type object axis direction mag time id)
	return HelpSystemExecf( "(lookup-torque \"%s\" |%s| |%s| %s %s %s |%s| %s)",
			STR2ARG(m_strName),					// label
			STR2ARG(m_strForceType),			// Net or empty
			STR2ARG(m_strBody),					// object
			STR2ARG(m_strAgent),				// axis
			DirArg(),							// direction
			MagArg(),							// mag
			STR2ARG(m_strTime),					// time  
			m_strId								// id
		);
}

LPCTSTR CVector::CheckUnitVector()
{
	// Urgh, lookup vector does not have enough arguments for both a subtype and a second body.
	// So we code subtype in the main type as unit-normal, unit-towards, unit-away-from
	// assume helpsys can deal with these pretend vector types.
	CString strSubType = m_strForceType;
	strSubType.Replace(" ", "-");  
	CString strType = "unit-" + strSubType;
	//(lookup-vector label type object direction mag time)
	return HelpSystemExecf( "(lookup-vector \"%s\" %s |%s| |%s| %s %s |%s| %s)",
			STR2ARG(m_strName),				// label
			STR2ARG(m_strAgent),			// instantaneous or average
			strType,						// vector type
			STR2ARG(m_strBody),				// object
			DirArg(),						// direction
			MagArg(),						// mag
			STR2ARG(m_strTime),				// time
			m_strId							// id
		);
}
 
BOOL CVector::CanDuplicate()
{
   	return TRUE;
}
    
CDrawObj* CVector::Clone()
{
	// create duplicate w/same properties 
   	CVector* pClone = new CVector(m_position);

	pClone->m_strId = m_strId;	// Initially, use same id, if added to document, 
								// different id will be generated
	pClone->m_strName = m_strName;
   	pClone->m_status = m_status;
    pClone->m_nVectorType = m_nVectorType;
	pClone->m_bAngular = m_bAngular;
	pClone->m_strForceType = m_strForceType;
   	pClone->m_strBody = m_strBody;
   	pClone->m_strAgent = m_strAgent;
   	pClone->m_strCompOf = m_strCompOf;
   	pClone->m_strCompDir = m_strCompDir;
	pClone->m_strTime = m_strTime;
	pClone->m_strOrientation = m_strOrientation;
	pClone->m_nZDir = m_nZDir;
	
    //Needed to add because checking temporary cloned object refers to its
	//m_pDocument during EARTHCHECK
   	pClone->m_pDocument = m_pDocument;
    
   	return (pClone);
}

void CVector::Delete()
{
	if (!m_Angles.IsEmpty())
	{
		if (m_pDocument)
			m_pDocument->UpdateAllViews(NULL,  HINT_DELETE_SELECTION , &m_Angles);

		POSITION pos = m_Angles.GetHeadPosition();
		while (pos != NULL) {//if you are deleting or moving an angle side
			CDrawObj* pAng = m_Angles.GetNext(pos);
			m_pDocument->Remove(pAng);//remove the associated angles from the list of objs
			((CCheckedObj*)pAng)->NotifyDelete();
			pAng->Delete();//delete all its associated angles
			// Notify all *other* views of list of objects that are going away.

		}
	}
	while (!m_Comps.IsEmpty())
	{
		CDrawObj* pObj = m_Comps.RemoveHead();
		pObj->m_status = statusUnknown;
	}

	CDrawObj::Delete();

}

   
CPoint CVector::GetBtnPos(CRect btnPos)
{
	CPoint ptNew(m_position.right, m_position.top);
	CRect posLabel;
	if (GetLabelRect(posLabel)) 	// labelled object
 	{
		// place "outward": set y above or below label 
		if (m_position.Height() < 0) // vec points upward
			ptNew.y = posLabel.top - btnPos.Height(); // place above
		else // vec points downward or is horizontal
 			ptNew.y = posLabel.bottom; // place below
 		// !!! button might extend off bottom of screen
 
 		// set xPos aligned left or right as direction.
 		if (m_position.Width() >= 0)	{// vec rightward or vertical:
 			ptNew.x = posLabel.left; // align left edges
 		} else {	// vec is leftward: 
 			// align right edges; but don't move beyond screen left
 			int xLeft = posLabel.right - btnPos.Width();
 			ptNew.x = (xLeft < 0 ? 0 : xLeft); 
 		}
 	}
 	return ptNew;
}

// Transfer all property values from those of given object. 
// Used to apply changes on close of dialog.
void CVector::UpdateObj(CDrawObj* pObj)
{
	// transfer dlg props into object
	CVector* pTempVec = (CVector*)pObj;
	
	// vector type must be the same for this to make sense
	ASSERT(pTempVec->m_nVectorType == m_nVectorType);
	// but angular bit *can* be changed in dialog
	m_bAngular = pTempVec->m_bAngular;

	m_strName = pTempVec->m_strName;
	m_strOrientation = pTempVec->m_strOrientation;

	// If orientation/ZDir changed, use methods to update drawing to reflect.
	// Note possible direction m_strOrientation not a number if dir is unset.
	SetZDir(pTempVec->m_nZDir);
	int nDeg;
	if (! IsZAxisVector() && sscanf(pTempVec->m_strOrientation,"%d", &nDeg) == 1)
		SetDirection(nDeg);
	
	m_strBody = pTempVec->m_strBody;
	m_strTime = pTempVec->m_strTime;
	m_status = pTempVec->m_status;
	// Following not valid for all subtypes, but should be harmless to 
	// transfer the values in all cases anyway (garbage values should remain unused.)
	m_strForceType = pTempVec->m_strForceType;	// force type or avg vs. instantaneous
	m_strAgent = pTempVec->m_strAgent;

	if (m_nVectorType == VECTOR_COMPONENT)
	{
		m_strCompOf = pTempVec->m_strCompOf;
		m_strCompDir = pTempVec->m_strCompDir;
		
		if (!m_pDocument) return;

		POSITION pos = m_pDocument->m_objects.GetTailPosition();
		while (pos != NULL)
		{
			CDrawObj* pObj = m_pDocument->m_objects.GetPrev(pos);
			if (  pObj->IsKindOf(RUNTIME_CLASS(CVector)) &&
				!( ((CVector*)pObj)->m_nVectorType == VECTOR_COMPONENT ) &&
				! pObj->m_strName.IsEmpty()   )
			{
				CVector* pVec = (CVector*)pObj;
				if (m_strCompOf == pVec->m_strName)
				{
					if (!pVec->m_Comps.Find(this))
					{
						pVec->m_Comps.AddTail(this);
						break;
					}
				}
			}
		}	

		// Log the new properties on change for trace info
		LogEventf(EV_PROPS_COMPO, "%s name |%s| of |%s| dir |%s|", 
					m_strId, (LPCTSTR) m_strName, m_strCompOf, (LPCTSTR) m_strCompDir);
	}
	else if (m_nVectorType == VECTOR_FORCE) {
			// Log the new properties on change for trace info
			LogEventf(EV_PROPS_FORCE, "%s name |%s| type |%s| body |%s| agent |%s| dir %s time |%s|", 
					 m_strId,  m_strName, m_strForceType,  m_strBody,  m_strAgent,  m_strOrientation, m_strTime);
	} else{ 
			// Log the new properties on change for trace info. 
			// 6/27/00 Changed format to write kind by name, & include forcetype in all cases.
			CString strKind;
			GetTypeName(strKind);
			LogEventf(EV_PROPS_VECTOR, "%s name |%s| kind %s type |%s| body |%s| body2 |%s| dir %s time |%s|", 
					 m_strId,  m_strName, strKind, m_strForceType,  m_strBody, m_strAgent, m_strOrientation, m_strTime);
	}
}

//////////////////////////////////////////////////////////////////////////// 
// Coordinate Axes: 
//
// m_position.TopLeft() is origin
// m_position.BottomRight() is endpoint of positive X axis.
//
// As with vectors, m_position not necessarily normalized and 
// not a true bounding box.
////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CAxes, CCheckedObj, VERSIONABLE_SCHEMA | 4);
CAxes::CAxes()					// used from serialization only
{ 
//	m_pAngle = NULL;
	m_nIndex = 0;
}
CAxes::CAxes(const CRect& position)
	:CCheckedObj(position)
{	
	m_nDirection = 0;	// initial default
//	m_pAngle = NULL;
	m_nAxis = 2;
	m_nLastHit = 2;
	m_nIndex = 0;
	// string-valued props initialized to empty for unset.
}
    
#ifdef _DEBUG
void CAxes::Dump(CDumpContext& dc) const
{
	CDrawObj::Dump(dc);
	// dc << "Name: " << m_strName << "\n";
}
#endif // _DEBUG
    
COLORREF CAxes::StatusColor()
{	
	if (m_pDocument) // allow for drawing objects dangling outside any document
   	{
   		// hack to use special highlight color if selected in example view.
   		CDocTemplate* pDocTemplate = (m_pDocument)->GetDocTemplate();
   		if ((m_pDocument->m_nProblemType == PROB_EXAMPLE)
   			&&(pDocTemplate!=theApp.m_ptmplExEdit))
   		{
   			CEXView* pView = theApp.GetEXView();
   			if (this == pView->m_pExpSel)
 				return (RGB(128, 0, 128));	//purple
  		}
   	}
	// We color axes lighter colors, so as not to obscure vectors drawn
	// on top of them
	switch (m_status) {
	default: TRACE("CAxes::StatusColor: Unknown status!\n");
	case statusUnknown: return(RGB(0, 0, 0));	// Dark Grey not Black 
	case statusCorrect: return(RGB(0, 255, 0));		// Green not Dark Green
	case statusError:	return(RGB(255, 0, 0));		// Same old Red
	}
}
    
void CAxes::Draw(CDC* pDC)
{	
	// Position rect defines positive x-axis from origin in top-left to endpoint in
 	// bottom right. Calc axes endpoints, w/End1 = negative, End2 = positive 
    CPoint ptOrigin = m_position.TopLeft();
    CPoint pt_xEnd2 = m_position.BottomRight();
    	
	int dx = pt_xEnd2.x - ptOrigin.x;	// components of positive x-axis vector
   	int dy = pt_xEnd2.y - ptOrigin.y;
   	CPoint pt_xEnd1(ptOrigin.x - dx, ptOrigin.y - dy);
   	CPoint pt_yEnd1(ptOrigin.x - dy, ptOrigin.y + dx); 
   	CPoint pt_yEnd2(ptOrigin.x + dy, ptOrigin.y - dx);
	m_negX = pt_xEnd1;
	m_posX = pt_xEnd2;
	m_negY = pt_yEnd1;
	m_posY = pt_yEnd2;
  	// Draw axes lines
   	int lineWidth = nAxesWidth;
    		
   	CPen penAxes;
   	if (! penAxes.CreatePen(PS_INSIDEFRAME, lineWidth, StatusColor()))
			return ;
	CPen* pOldPen = pDC->SelectObject( &penAxes );
	//Draw Axis (path)
	pDC->MoveTo(pt_xEnd1);
	pDC->LineTo(pt_xEnd2);
	pDC->MoveTo(pt_yEnd1);
	pDC->LineTo(pt_yEnd2);
	// Compute axis line coordinates.  We are creating a rectangle shaped
	// polygon region ((linewidth+2) X (length of line)) for hittesting
	CRgn axisXRgn, axisYRgn, wholeRgn;
	double angle= atan2((double)dy,(double)dx);
	int xShift=(int)((lineWidth+2)*sin(angle));
	int yShift=(int)((lineWidth+2)*cos(angle));
	// construct actual polygon region to represent line of X-axis
	CPoint xLinePts[4];
	xLinePts[0]=CPoint(pt_xEnd1.x-xShift, pt_xEnd1.y+(1*yShift));
	xLinePts[1]=CPoint(pt_xEnd1.x+xShift, pt_xEnd1.y-(1*yShift));
	xLinePts[2]=CPoint(pt_xEnd2.x+xShift, pt_xEnd2.y-(1*yShift));
	xLinePts[3]=CPoint(pt_xEnd2.x-xShift, pt_xEnd2.y+(1*yShift));
	axisXRgn.CreatePolygonRgn(xLinePts, 4, ALTERNATE);
	// construct actual polygon region to represent line of Y-axis
	CPoint yLinePts[4];
	yLinePts[0]=CPoint(pt_yEnd1.x-yShift, pt_yEnd1.y-(1*xShift));
	yLinePts[1]=CPoint(pt_yEnd1.x+yShift, pt_yEnd1.y+(1*xShift));
	yLinePts[2]=CPoint(pt_yEnd2.x+yShift, pt_yEnd2.y+(1*xShift));
	yLinePts[3]=CPoint(pt_yEnd2.x-yShift, pt_yEnd2.y-(1*xShift));
	axisYRgn.CreatePolygonRgn(yLinePts, 4, ALTERNATE);
	//Combine the line regions to create one region for entire
	//coordinate axes (in axisXRgn)
	axisXRgn.CombineRgn(&axisXRgn, &axisYRgn, RGN_OR);
    	
	axisYRgn.DeleteObject();		// finished with this
	//Any existing GDIObject is deleted from axis region
	m_axisRgn.DeleteObject();
	//Detach returns handle to windows GDI Object which is then attached
	//to the axis region
	m_axisRgn.Attach(axisXRgn.Detach());
	pDC->SelectObject(pOldPen);
    //*******************************************
	m_negXAxisRgn.DeleteObject();
	m_posXAxisRgn.DeleteObject();
	m_negYAxisRgn.DeleteObject();
	m_posYAxisRgn.DeleteObject();

	CPoint negxLinePts[4];
	negxLinePts[0]=CPoint(pt_xEnd1.x-xShift, pt_xEnd1.y+(1*yShift));
	negxLinePts[1]=CPoint(pt_xEnd1.x+xShift, pt_xEnd1.y-(1*yShift));
	negxLinePts[2]=CPoint(ptOrigin.x+xShift, ptOrigin.y-(1*yShift));
	negxLinePts[3]=CPoint(ptOrigin.x-xShift, ptOrigin.y+(1*yShift));
	m_negXAxisRgn.CreatePolygonRgn(negxLinePts, 4, ALTERNATE);
	// construct actual polygon region to represent line of Y-axis
	CPoint negyLinePts[4];
	negyLinePts[0]=CPoint(pt_yEnd1.x-yShift, pt_yEnd1.y-(1*xShift));
	negyLinePts[1]=CPoint(pt_yEnd1.x+yShift, pt_yEnd1.y+(1*xShift));
	negyLinePts[2]=CPoint(ptOrigin.x+yShift, ptOrigin.y+(1*xShift));
	negyLinePts[3]=CPoint(ptOrigin.x-yShift, ptOrigin.y-(1*xShift));
	m_negYAxisRgn.CreatePolygonRgn(negyLinePts, 4, ALTERNATE);
	//******************************
	CPoint posxLinePts[4];
	posxLinePts[0]=CPoint(pt_xEnd2.x-xShift, pt_xEnd2.y+(1*yShift));
	posxLinePts[1]=CPoint(pt_xEnd2.x+xShift, pt_xEnd2.y-(1*yShift));
	posxLinePts[2]=CPoint(ptOrigin.x+xShift, ptOrigin.y-(1*yShift));
	posxLinePts[3]=CPoint(ptOrigin.x-xShift, ptOrigin.y+(1*yShift));
	m_posXAxisRgn.CreatePolygonRgn(posxLinePts, 4, ALTERNATE);
	// construct actual polygon region to represent line of Y-axis
	CPoint posyLinePts[4];
	posyLinePts[0]=CPoint(pt_yEnd2.x-yShift, pt_yEnd2.y-(1*xShift));
	posyLinePts[1]=CPoint(pt_yEnd2.x+yShift, pt_yEnd2.y+(1*xShift));
	posyLinePts[2]=CPoint(ptOrigin.x+yShift, ptOrigin.y+(1*xShift));
	posyLinePts[3]=CPoint(ptOrigin.x-yShift, ptOrigin.y-(1*xShift));
	m_posYAxisRgn.CreatePolygonRgn(posyLinePts, 4, ALTERNATE);
	//***********************************************
	// Calc positions of axes label bounding boxes.
	CString strXLabel("+X");
	CString strYLabel("+Y");
	// add subscript to label if other than the first axes system
	if (m_nIndex > 0) {
		strXLabel.Format("+X%d", m_nIndex);
		strYLabel.Format("+Y%d", m_nIndex);
	}
    
   	CSize ext = pDC->GetTextExtent(strXLabel);	// label box extent = width, height
   	// CRect rcX = CRect(CPoint(pt_xEnd2.x + 5, pt_xEnd2.y + 5), ext);
	CPoint ptX = CalcLabelPos(pt_xEnd2, dx, dy, ext);

	// We save position of "x" label for use in hit testing
	// NB this member var only valid after obj is drawn.
	m_posLabel = CRect(ptX, ext);

	// Now Y axis label.  Components of perpendicular pos Y-axis = (dy, -dx)
	ext = pDC->GetTextExtent(strYLabel);
	CPoint ptY = CalcLabelPos(pt_yEnd2, dy, -dx, ext);
		
	// Draw the labels, transparent text, black so visible against colored objects.
	COLORREF oldColor = pDC->SetTextColor(/* StatusColor()*/ RGB(0,0,0));
	int oldBkMode = pDC->SetBkMode(TRANSPARENT);
	pDC->TextOut(ptX.x,  ptX.y, strXLabel);
	pDC->TextOut(ptY.x, ptY.y, strYLabel);
	pDC->SetBkMode(oldBkMode);
	pDC->SetTextColor(oldColor);
}
    
// Calculate top-left point for label text block placed outside directed line endpoint,
// IN: line endpoint, line slope components, and text block extent (= width, height)
CPoint CAxes::CalcLabelPos(CPoint ptEnd, int dx, int dy, CSize size)
{
	int w = size.cx;	// width of block
	int h = size.cy;	// height of block
	const space = 2;	// size of slight offset from endpoint, used in x direction only 
   						// for y, rely on internal text leading to provide small space.
    
	if (dy == 0) 	// horizontal line: center height at left or right of axis end
		return (dx > 0) ? CPoint(ptEnd.x + space, ptEnd.y - h/2)
						: CPoint(ptEnd.x - (w + space), ptEnd.y - h/2);
	else if (dx == 0)  // vertical line: center width above or below axis end; 
		return (dy < 0) ? CPoint(ptEnd.x - w/2, ptEnd.y - h)
						: CPoint(ptEnd.x - w/2, ptEnd.y);
	// else set appropriate corner of text block near line endpoint
	else if (dx > 0 && dy < 0)		// (I) line points up-right  
		return CPoint(ptEnd.x /*+ space*/, ptEnd.y - h);	// lower left corner by end 
	else if (dx <0 && dy < 0)		// (II) line points up-left 
		return CPoint(ptEnd.x - (w /*+ space*/), ptEnd.y - h);// lower right corner by end
	else if (dx < 0 && dy > 0)		// (III) line points down-left
		return CPoint(ptEnd.x - (w /*+ space*/), ptEnd.y);	// top right corner by end
	else if (dx > 0 && dy > 0)		// (IV) line points down-right
		return CPoint(ptEnd.x /*+ space*/, ptEnd.y);			// top left corner by end
	
	ASSERT(FALSE);		// shouldn't ever get here
	return ptEnd;
}
    
    
int CAxes::GetHandleCount()
{
	return (2);
}
CPoint CAxes::GetHandle(int nHandle)
{
	if (nHandle == 2){
		if (m_nAxis == POSX)
			return CDrawObj::GetHandle( 5 );
		else if (m_nAxis == POSY)
			return m_posY;
		else if (m_nAxis == NEGX)
			return m_negX;
		else if (m_nAxis == NEGY)
			return m_negY;
	}
	return CDrawObj::GetHandle(nHandle);
}

HCURSOR CAxes::GetHandleCursor(int nHandle)
{	
	return CDrawObj::GetHandleCursor(nHandle == 2 ? 5 : nHandle);
}

void CAxes::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{

	CRect position = m_position;
	int dx = point.x - position.left;
	int dy = point.y - position.top;
	if ((m_nAxis != POSX)&&(nHandle == 2)){
		if (m_nAxis == POSY){
			position.bottom = position.top + (dx);
			position.right = position.left - (dy);
		
		}
		else if (m_nAxis == NEGX){
			position.bottom = position.top - (dy);
			position.right = position.left - (dx);
	
		}
		else if (m_nAxis == NEGY){
			position.bottom = position.top - (dx);
			position.right = position.left + (dy);

		}
		MoveTo(position, pView);
	}
	else
		CDrawObj::MoveHandleTo(nHandle ==2 ? 5 : nHandle, point, pView);

	if (nHandle == 1)  
	{
		theApp.GetDocument()->DeleteObjList(&m_Angles);
	}
	else
		UpdateAngles();
}
    
void CAxes::MoveTo(const CRect& position, CFBDView* pView)
{
	// New in Andes2 -- prohibit move out of first quadrant.
	int nNewDir = GetDirection(position.TopLeft(), position.BottomRight()) ;
	if (!(nNewDir >= 0 && nNewDir < 90))
		return;		// silent no-op. Could beep user to warn.

	CDrawObj::MoveTo(position, pView);

	// update angle status pane
	CMainFrame* pFrame = (CMainFrame*) AfxGetApp()->m_pMainWnd;
	CString str;
	str.Format("%d degrees", GetDirection());
	pFrame->SetAngleText(str);
}
    
// This func was used by select tool for hit testing; called by ObjectAt
// now we are using HitTest instead.
BOOL CAxes::Intersects(const CRect& rect)
// rect in logical coords.
{
	if (CDrawObj::Intersects(rect))
		return (TRUE);
	
	// else check if hit on X axis label
	CRect fixed = m_posLabel;
	fixed.NormalizeRect();
	CRect rectT = rect;
	rectT.NormalizeRect();
	return !(rectT & fixed).IsRectEmpty();
}
    
int CAxes::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
	if ((bSelected)||(m_axisRgn.m_hObject == NULL)) // testing for resize handle grab: 
		return CDrawObj::HitTest(point, pView, bSelected);//handle it in base class
	// Also ensuring that region exists and axis hasn't bee resized to a point
	
	//return Axis Part hit
	if (m_posXAxisRgn.PtInRegion(point)){
		m_nLastHit = POSX;
		return m_nLastHit;
	}
	if (m_posYAxisRgn.PtInRegion(point)){
		m_nLastHit = POSY;
		return m_nLastHit;
	}
	if (m_negXAxisRgn.PtInRegion(point)){
		m_nLastHit = NEGX;
		return m_nLastHit;
	}
	if (m_negYAxisRgn.PtInRegion(point)){
		m_nLastHit = NEGY;
		return m_nLastHit;
	}

	
	// else check if hit on X-axis label for non-selected case. Currently X only
	if (!bSelected) {
		CRect posLabel = m_posLabel;
		posLabel.NormalizeRect();
		if (posLabel.PtInRect(point))
			return 2;
	}
	return 0;
}

void CAxes::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
    	
	// document versions >= 1 added serialization of base class descriptor
	if (nDocVersion >= 1) {
		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
	}
	CDrawObj::Serialize(ar);
	if (ar.IsStoring()){        // store object specific data
		// version 0 had nothing
		// added in version 1:
		ar << (WORD) m_status;
		// added version 2:
		ar << (WORD) m_nDirection;
		ar << m_strSystem;
		// added version 3: 
		m_Angles.Serialize(ar);
		// added version 4:
		ar << m_nIndex;
	} else {							// load object specific data
		
		WORD wTemp;
		if (nClassVersion >= 1) {		// get v1-added members
			ar >> wTemp;
			m_status = (Status) wTemp; // !!! ensure valid
			// changed order of enumeration in doc version 4
   			if (nClassVersion == 1 && nDocVersion <= 3) {
				if(m_status == statusError)
					m_status = statusUnknown;
				else if (m_status == statusUnknown)
					m_status = statusCorrect;
				else //m_status == statusCorrect
					m_status = statusError;
			}
		} 
		if (nClassVersion >= 2){		// get v2-added members or init defaults
			ar >> wTemp;
			m_nDirection = (int) wTemp;
			ar >> m_strSystem;
		} else	{
			m_nDirection = GetDirection();
			// leave m_strSystem as empty string
		}
		if (nClassVersion >= 3){		// get v3-added members
			m_Angles.Serialize(ar);
		} // else leave as empty list
		if (nClassVersion >= 4) {		// get v4-added members
			ar >> m_nIndex;
		} // else leave as zero from constructor
		// earlier versions had no data
	}
}

void CAxes::LogEntry()
{
	CString strDrawObj = GetDrawObjLogPart();
	CString strSystem = ValToArg(m_strSystem);
	LogEventf(EV_FBD_ENTRY, "%s %d %s", strDrawObj, m_nDirection, strSystem);
}

BOOL CAxes::SetFromLogStr(LPCTSTR pszStr)
{
	LPCSTR pszRest = ParseDrawObjLogPart(pszStr);
	if (pszRest == NULL) return FALSE;
	char szSystem[80];
	if (sscanf(pszRest, "%d %s", &m_nDirection, szSystem) != 2) return FALSE;
	m_strSystem = ArgToVal(szSystem);
	return TRUE;
}
    
CDialog* CAxes::GetPropertyDlg()
{
	return new CAxesDlg(this);
}
 
void CAxes::CheckObject()
{
	if (! theApp.CheckDiagram()) return;

    // add subscript to label if other than the first axes system
	CString strXLabel = "x";
	CString strYLabel = "y";
	CString strZLabel = "z";
	if (m_nIndex > 0) {
		strXLabel.Format("x%d", m_nIndex);
		strYLabel.Format("y%d", m_nIndex);
		strZLabel.Format("z%d", m_nIndex);
	}

	// (assert-x-axis body dir &opt id (x-label "x") (y-label "y") (z-label "z"))
	LPCTSTR pszResult = HelpSystemExecf("(assert-x-axis NIL %d %s \"%s\" \"%s\" \"%s\")", 
    		              m_nDirection, m_strId, strXLabel, strYLabel, strZLabel); 
	ApplyStatus(pszResult);
}

CPoint CAxes::GetAxisPoint()
{
	CPoint point = m_position.BottomRight();
	if (m_nAxis == POSX){
		return m_posX;	
	}
	if (m_nAxis == POSY){
		
		return m_posY;
	}
	if (m_nAxis == NEGX){
			
		return m_negX;	
	}
	if (m_nAxis == NEGY){
			
		return m_negY;	
	}
	return point;
}

CPoint CAxes::GetAxisPoint(int nAxis)
{
	CPoint point = m_position.BottomRight();
	if (nAxis == POSX){
		return m_posX;	
	}
	if (nAxis == POSY){
		
		return m_posY;
	}
	if (nAxis == NEGX){
			
		return m_negX;	
	}
	if (nAxis == NEGY){
			
		return m_negY;	
	}
	return point;
}

CPoint CAxes::GetBtnPos(CRect btnPos)
{
	CPoint ptNew(m_position.right, m_position.bottom);
	CRect posLabel;
 	if (GetLabelRect(posLabel)) 	// labelled object
 	{
 		// place beside label (top aligned), and outward (to left or right)
 		ptNew.y = posLabel.top;
 		if (m_position.Width() >= 0)	{// axis rightward or vertical:
 			ptNew.x = posLabel.right; // place right of label
 		} else {	// axis points leftward: 
 			// try to place left of. but change to right of if no room.
 			int xLeft = posLabel.left - btnPos.Width();
 			ptNew.x = (xLeft < 0 ? posLabel.right : xLeft); 
		}
	}
 	
 	return ptNew;
}

void CAxes::SetSelectedPart(int hit)
{
	m_nAxis = hit;
	return;
}

int CAxes::GetSelectedPart()
{
	if (m_nAxis != -1)
		return m_nAxis;
	return 2;
}


int CAxes::GetHit()
{
	return m_nLastHit;
}

void CAxes::Delete()
{
	if (!m_Angles.IsEmpty()){
		if (m_pDocument)
			m_pDocument->UpdateAllViews(NULL,  HINT_DELETE_SELECTION , &m_Angles);
		POSITION pos = m_Angles.GetHeadPosition();
		while (pos != NULL) {//if you are deleting or moving an angle side
			CDrawObj* pAng = m_Angles.GetNext(pos);
			m_pDocument->Remove(pAng);//remove the associated angles from the list of objs
			((CCheckedObj*)pAng)->NotifyDelete();
			pAng->Delete();//delete all its associated angles
		}
	}
	CDrawObj::Delete();
}

//need to update objects with components, pre-defined components for vector
//and vector variables now exist
void CAxes::UpdateVarNames(CString strOldName)
{
	POSITION pos = ((CFBDDoc*)theApp.GetDocument())->GetObjects()->GetTailPosition();
	while (pos != NULL){
		CDrawObj* pObj = ((CFBDDoc*)theApp.GetDocument())->GetObjects()->GetPrev(pos);
		if (!pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)))
			continue;
		if (pObj->m_flag == TEACHER_OBJECT)
			continue;
		CCheckedObj* pChkObj = (CCheckedObj*)pObj;
		if (pChkObj->HasComponents())
			pChkObj->UpdateVarNames();
	}
	pos = ((CFBDDoc*)theApp.GetDocument())->m_Variables.GetTailPosition();
	while (pos != NULL){
		CDrawObj* pObj = ((CFBDDoc*)theApp.GetDocument())->m_Variables.GetPrev(pos);
		if (!pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)))
			continue;
		if (pObj->m_flag == TEACHER_OBJECT)
			continue;
		CCheckedObj* pChkObj = (CCheckedObj*)pObj;
		if (pChkObj->HasComponents())
			pChkObj->UpdateVarNames();
	}

}

void CAxes::UpdateObj(CDrawObj* pObj)
{
	CAxes* pTempAxes = (CAxes*)pObj;
	m_nDirection = pTempAxes->m_nDirection;
	SetDirection(pTempAxes->m_nDirection);	
	m_strSystem = pTempAxes->m_strSystem;
	m_status = pTempAxes->m_status;

	LogEventf(EV_PROPS_AXES, "%s dir %d", m_strId, m_nDirection);

}

CDrawObj* CAxes::Clone()
{
   	// create duplicate w/same properties 
   	CAxes* pClone = new CAxes(m_position);
	pClone->m_strId = m_strId;
  	pClone->m_strName = m_strName;
   	pClone->m_status = m_status;
	pClone->m_nDirection = m_nDirection; 
	pClone->m_strSystem = m_strSystem;
	pClone->m_nIndex = m_nIndex;
  
   	return (pClone);

}

BOOL  CAxes::HasSameDir(CDrawObj* pObj)
{
	return (((CAxes*)pObj)->m_nDirection == m_nDirection);
}

//need to update objects with components, pre-defined components for vector
//and vector variables no longer exist 
//also setting any drawn component to unknown status (maybe should be somewhere else)
void CAxes::RemoveVarNames(CString strName)
{
	POSITION pos = ((CFBDDoc*)theApp.GetDocument())->GetObjects()->GetTailPosition();
	while (pos != NULL){
		CDrawObj* pObj = ((CFBDDoc*)theApp.GetDocument())->GetObjects()->GetPrev(pos);
		if (pObj->m_flag == TEACHER_OBJECT)
			continue;
		if (!pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)))
			continue;
		else{
			CCheckedObj* pChkObj = (CCheckedObj*)pObj;
			if (pChkObj->HasComponents())
				pChkObj->UpdateVarNames();
			else if (pChkObj->IsComponent())
				pChkObj->m_status = statusUnknown;
		}
	}
	pos = ((CFBDDoc*)theApp.GetDocument())->m_Variables.GetTailPosition();
	while (pos != NULL)
	{
		CDrawObj* pObj = ((CFBDDoc*)theApp.GetDocument())->m_Variables.GetPrev(pos);
		if (pObj->m_flag == TEACHER_OBJECT)
			continue;
		if (!pObj->IsKindOf(RUNTIME_CLASS(CCheckedObj)))
			continue;
		else{
			CCheckedObj* pChkObj = (CCheckedObj*)pObj;
			if (pChkObj->HasComponents())
				pChkObj->UpdateVarNames();
		}
	}

}
    
//////////////////////////////////////////////////////////////////////////
// System: 
// Represents a body or bodies at a time. Represented by a filled 
// circle sized by the user.
// 
// Might want to change this to enforce a standard body dot.   
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CSystem, CCheckedObj, VERSIONABLE_SCHEMA | 2);
CSystem::CSystem(){}				// called from serialization only
    
void CSystem::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	// document versions >= 1 added serialization of base class descriptor
	if (nDocVersion >= 1) {
		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
	}
	CDrawObj::Serialize(ar);
	if (ar.IsStoring()){        // store object specific data
		// version 0 had nothing
		// added in version 1:
		ar << (WORD) m_status;
		// added in version 2:
		ar << (WORD) m_nSystemType;
		ar << m_strBodies;
		ar << m_strTime;
	} else {							// load object specific data
	
		WORD wTemp;
		if (nClassVersion == 1) {
			ar >> wTemp;
			m_status = (Status) wTemp;     // !!! ensure valid
			if (nDocVersion <= 3)//changed order of enumeration in doc version 4
			{
				if(m_status == statusError)
					m_status = statusUnknown;
				else if (m_status == statusUnknown)
					m_status = statusCorrect;
				else //m_status == statusCorrect
					m_status = statusError;
			}
			// other members not saved. Init to defaults:
			m_nSystemType = SYSTEM_SINGLE_BODY;
		} 
		else if (nClassVersion == 2){
			ar >> wTemp;
			m_status = (Status) wTemp;  
			ar >> wTemp;
			m_nSystemType = (int)wTemp;
			ar >> m_strBodies; 
			ar >> m_strTime;
		}
		// else earlier version, no data
	}
}
    
    
CSystem::CSystem(const CRect& position)
	:CCheckedObj(position)
{
	m_nSystemType = SYSTEM_SINGLE_BODY;  // initial default
	// String-valued props initialized to empty strings
}

void CSystem::LogEntry()
{
	CString strDrawObj = GetDrawObjLogPart();
	CString strBodies = ValToArg(m_strBodies);
	CString strTime = ValToArg(m_strTime);
	LogEventf(EV_FBD_ENTRY, "%s %d %s %s", strDrawObj, m_nSystemType, strBodies, strTime);
}

BOOL CSystem::SetFromLogStr(LPCTSTR pszStr)
{
	LPCSTR pszRest = ParseDrawObjLogPart(pszStr);
	if (pszRest == NULL) return FALSE;

	char szTime[80]; char szBodies[255];
	if (sscanf(pszRest, "%d %s %s", &m_nSystemType, szBodies, szTime) != 3) return FALSE;
	m_strBodies = ArgToVal(szBodies);
	m_strTime = ArgToVal(szTime);

	return TRUE;
}
   
#ifdef _DEBUG
void CSystem::Dump(CDumpContext& dc) const
{
	CDrawObj::Dump(dc);
	// dc << "Name: " << m_strName << "\n";
}
#endif // _DEBUG
    
void CSystem::Draw(CDC* pDC)
{
	// Draw the oval that represents the body
	CBrush brushSystem(StatusColor());
	CBrush* pOldObj = pDC->SelectObject(&brushSystem);

	pDC->Ellipse(m_position);
	pDC->SelectObject(pOldObj);
	
	//Create the circular region
	//Any existing GDIObject is deleted from circle region
	m_circleRgn.DeleteObject();
	m_circleRgn.CreateEllipticRgnIndirect(&m_position);

	// draw the label
	if (! m_strName.IsEmpty()) {
		// simple label placement, saving into member for hit-testing
		m_posLabel = CRect(CPoint(m_position.right, m_position.bottom),
			       pDC->GetTextExtent(m_strName));
    
		COLORREF oldColor = pDC->SetTextColor(/*StatusColor()*/ RGB(0,0,0));
		int oldBkMode = pDC->SetBkMode(TRANSPARENT);
		pDC->TextOut(m_posLabel.left, m_posLabel.top, m_strName);
		pDC->SetTextColor(oldColor);
		pDC->SetBkMode(oldBkMode);
	}
}

int CSystem::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
	if ((bSelected)||(m_circleRgn.m_hObject==NULL)) // testing for resize handle grab: 
		return CDrawObj::HitTest(point, pView, bSelected);//handle it in base class
	// Also ensuring region exists and system hasn't been resized to a point

	// else testing if hit system
	
	// first see if hit system region
	if (m_circleRgn.PtInRegion(point))
		return 1;
	// else check if hit on system label for non-selected case
	if (!m_strName.IsEmpty()) {
		CRect posLabel = m_posLabel;
		posLabel.NormalizeRect();
		if (posLabel.PtInRect(point))
			return 1;
	}
	return 0;
    
}

CRect CSystem::GetBoundingBox()
{
	// base class returns fixed-up normalized position rect.
	return CDrawObj::GetBoundingBox() | m_posLabel;		// union of the rects
}
  
CDialog* CSystem::GetPropertyDlg()
{
	return new CSystemDlg(this);
}
    

void CSystem::CheckObject()
{
	if (! theApp.CheckDiagram()) return;
	LPCTSTR pszResult;

	// Extract time point number out of time string 
//	CString strTimeId = GetTimeId(m_strTime);
	
	// (assert-object label object-name)
	if (m_nSystemType == SYSTEM_SINGLE_BODY) {
		pszResult = HelpSystemExecf( "(assert-object \"%s\" %s |%s| %s)", 
						STR2ARG(m_strName), 
						STR2ARG(m_strBodies),
						STR2ARG(m_strTime), 
						m_strId);
	} else {// compound body
		// dialog loads body list into m_strBodies.
		//!!! need to iterate to do EARTHCHECK on each body
		pszResult = HelpSystemExecf( "(assert-compound-object \"%s\" (%s) |%s| %s)",
						STR2ARG(m_strName), 
						STR2ARG(m_strBodies),
						STR2ARG(m_strTime),
						m_strId);
	}
	ApplyStatus(pszResult);
}
    
CString CSystem::GetPrintDef()
{
	CString strTimePart; // default empty
	if (!m_strTime.IsEmpty())
		strTimePart.Format("at time %s", m_strTime);
	CString strDef;
	strDef.Format("%s %s %s", 
		m_nSystemType == SYSTEM_SINGLE_BODY ? "Body" : "Bodies",
		m_strBodies, strTimePart);
	return strDef;
    
}
    
CString CSystem::GetDef()
{
	CString strDef;
	m_strBodies.TrimRight();
	strDef.Format("body %s", m_strBodies);
	return strDef;
    
}

void CSystem::GetTypeName(CString & strType)
{
	strType = "Body";	
}

BOOL CSystem::IsValid()
{
	CString str;
	CFBDDoc* pDoc = (CFBDDoc*)theApp.GetDocument();
	CDrawObj* pObj = pDoc->GetMatchingObj(this, FALSE);//bMatchDef
	if (pObj != NULL)
	{
		str.Format(IDS_SAMEDEF_OBJ, pObj->GetDef(), pObj->m_strName);
		theApp.DoInfoMessage(str);
		return FALSE;
	}
	pObj = pDoc->GetMatchingObj(this, TRUE);//bMatchName
	if (pObj != NULL){
		CString def = pObj->GetDef();
		/*if ( (pObj->IsKindOf(RUNTIME_CLASS(CSystem))) && (pObj->m_strName[0] != 'm') )
			def = "body" + def.Mid(7); */
		str.Format(IDS_LABEL_NOTUNIQUE, pObj->m_strName, def);
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
			str.Format(IDS_LABEL_NOTUNIQUE, pVar->m_strName, pVar->m_strDef);
			theApp.DoInfoMessage(str);
			return FALSE;
		}
	}
	//If a mass variable already exists, do not add pre-defined mass variable to list
	//"m" + label of body
	//And so do not show this error message
/*	pVar = pDoc->GetMatchingVar(this, FALSE);//bMatchDef
	if (pVar != NULL)
	{
		str.Format(IDS_REDEFINE_OBJ, pVar->m_strName, pVar->m_strDef);
		theApp.DoInfoMessage(str);
		return FALSE;
	}*/
	return TRUE;

}

BOOL CSystem::HasSameName(CDrawObj* pObj)
{
	//Check object list to see if we already have drawn an object with the
	//same label
	CString strName = pObj->m_strName;
//	CString strMassName = "m" + m_strName;

	if (strcmp(strName, m_strName) == 0){//AW: change to be case sensitive
		//if same name, check if same id
		if (strcmp(m_strId, pObj->m_strId)!=0)
			return TRUE;
	}
/*
	if (_stricmp(strName, strMassName) == 0){//case insensitive
		//if same name, check if same id
		if (strcmp(m_strId, pObj->m_strId)!=0)
			return TRUE;
	}

	if (pObj->IsKindOf(RUNTIME_CLASS(CSystem))){
		strName = "m" + strName;
		if (_stricmp(strName, m_strName) == 0){//case insensitive
			//if same name, check if same id
			if (strcmp(m_strId, pObj->m_strId)!=0)
				return TRUE;
		}
		if (_stricmp(strName, strMassName) == 0){//case insensitive
			//if same name, check if same id
			if (strcmp(m_strId, pObj->m_strId)!=0)
				return TRUE;
			}
	}
*/
	return FALSE;

}

BOOL CSystem::HasSameDef(CDrawObj* pObj)
{
	//Check object list to see if we already have drawn a system with the
	//same definition
	if (!pObj->IsKindOf(RUNTIME_CLASS(CSystem)))
		return FALSE;
	//if system, check if same body & time
	CSystem* pSys = (CSystem*)pObj;
	if (strcmp(m_strId, pSys->m_strId)==0)
		return FALSE;//we are editing this object
//	m_strBodies.TrimRight();
	if ((strcmp(m_strBodies, pSys->m_strBodies) == 0)
		&& theApp.GetDocument()->IsMatchingTime(m_strTime, pSys->m_strTime) )
		return TRUE;
	
	return FALSE;
}
 
BOOL CSystem::HasSameDef(CVariable* pVar)
{
/*
	//Check object list to see if we already have declared a variable 
	//with the same definition
	m_strBodies.TrimRight();
	if (_stricmp(pVar->m_strQuantName, "mass")==0)//case insensitive
	{
		//if system, check if same body & time
		if (strcmp(m_strBodies, pVar->m_strObject) == 0)				
			return TRUE;			
	}
*/
	return FALSE;
}

void CSystem::UpdateVarNames(CString strOldName)
{
/*
	if (strOldName.IsEmpty())
		strOldName = m_strName;

	RemoveVarNames(strOldName);
	
	CString strNewMassVar = "m" + m_strName;
	((CFBDDoc*)theApp.GetDocument())->m_strVarNames.AddTail(strNewMassVar);
*/
}

void CSystem::RemoveVarNames(CString strOldName)
{
/*
	if (strOldName.IsEmpty())
		strOldName = m_strName;

	CString strOldMassVar = "m" + strOldName;
	RemoveVarName(strOldMassVar);
*/	
}


CDrawObj* CSystem::Clone()
{
	// create duplicate w/same properties 
	CSystem* pClone = new CSystem(m_position);
	pClone->m_strId = m_strId;	// Initially, use same id, if added to document, 
								// different id will be generated
	pClone->m_strName = m_strName;
	pClone->m_status = m_status;

	pClone->m_nSystemType = m_nSystemType;
	pClone->m_strBodies = m_strBodies;
	pClone->m_strTime = m_strTime;
    //Needed to add because checking temporary cloned object which refers to its
	//m_pDocument during EARTHCHECK
   	pClone->m_pDocument = m_pDocument;
    	
  
	return (pClone);
}
    
BOOL CSystem::CanDuplicate()
{
	return TRUE;
}
   
CPoint CSystem::GetBtnPos(CRect btnPos)
{
 	CPoint ptNew(m_position.right, m_position.top);
 	CRect posLabel;
 	if (GetLabelRect(posLabel)) 	// labelled object
 	{
 		// try above label (always).
		ptNew = CPoint(posLabel.left, posLabel.top - btnPos.Height());
 	}
 	return ptNew;
}
 
void CSystem::UpdateObj(CDrawObj* pObj)	// transfer new props into object
{
	// transfer dlg props into object
	CSystem* pTempSys = (CSystem*)pObj;
	
	m_strName = pTempSys->m_strName;
	m_strTime = pTempSys->m_strTime;
	m_nSystemType = pTempSys->m_nSystemType; // set in OnOK
	m_strBodies = pTempSys->m_strBodies;	// set in OnOK
	m_status = pTempSys->m_status;	// set in OnOK

	// Log the new properties
	LogEventf(EV_PROPS_SYSTEM, "%s name |%s| type %d time |%s| bodies |%s|", m_strId, 
			(LPCTSTR) m_strName, m_nSystemType, (LPCTSTR) m_strTime, 
			(LPCTSTR) m_strBodies);
}

 
////////////////////////////////////////////////////////////////////////////
// 
// Angle arcs w/labels
//
////////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CAngle, CCheckedObj, VERSIONABLE_SCHEMA | 3);
CAngle::CAngle()	// called from serialization
{
	m_nAxis = -1;
	m_degAng = -1;  // out of bounds value => unknown
}					
    
CAngle::CAngle(const CRect& position)
    	:CCheckedObj(position)
{
	m_nAxis = -1;
	m_degAng = -1;	// out of bounds value => unknown
}
    
void CAngle::Serialize(CArchive& ar)
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
    	if (ar.IsStoring())		// store object specific data
    	{
    		// version 1 info:
    		ar << m_intercept; 
    		ar << m_pAngSide1;
    		ar << m_pAngSide2;
    		ar << (WORD)m_radius;
    		ar << (WORD)m_maxRadius;
			// version 2 info
			ar << m_nAxis;
			// version 3, if don't serialize, checkInitEntries won't work properly
			// this value figured out when drawn, CheckInitEntries called before figured out
			ar << m_degAng;
    	} 
    	else // load object specific data
    	{
    		WORD wTemp;
    		if (nVersion >= 1) {
    			ar >> m_intercept; 
    			ar >> m_pAngSide1;
    			ar >> m_pAngSide2;
    			ar >> wTemp; m_radius = (int)wTemp;
    			ar >> wTemp; m_maxRadius = (int)wTemp;
    		 
    		}
			if (nVersion >= 2)
				ar >> m_nAxis;
			if (nVersion >= 3)
				ar >> m_degAng;
    	}
}
    
    
void CAngle::Draw(CDC* pDC)
{
    	// Draw the oval that represents the body
    	CPen penVector;
    	if (! penVector.CreatePen(PS_INSIDEFRAME, 3, StatusColor()))
    			return ;
    	CPen* pOldPen = pDC->SelectObject( &penVector );
    
    	CFBDView* pView = theApp.GetFBDView();

		m_arcRect = GetAngleRect(m_intercept, m_radius);
    	pDC->Arc(m_arcRect, m_position.TopLeft(), m_position.BottomRight());
    	
		SetAngleMag();

		//draw the label
		DrawLabel(pDC);	
		//finished drawing
    	pDC->SelectObject(pOldPen);
    	//Create the angle region
		if (!CreateAngleRegion(pView))
			TRACE("Failed to create angle region");
	
  	
}


void CAngle::DrawLabel(CDC * pDC)
{
	COLORREF oldColor = pDC->SetTextColor(StatusColor());
	int oldMode = pDC->SetBkMode(TRANSPARENT);
	CString angstr = "?";
	if (m_degAng != -1)
		angstr.Format("%d", m_degAng);
    CString str = m_strName + " = " + angstr;
    CSize size = pDC->GetTextExtent(str);
	CPoint point = GetLabelPosition(size);
	CPoint ptLabel = point;
	CSize szLabel = size;
	int pos = str.Find('$');
	CFont hpfont;
    hpfont.CreateFont(20,0,0,0,FW_DONTCARE,0,0,0,ANSI_CHARSET,0,0,0,
    		VARIABLE_PITCH|FF_DONTCARE,"MS Sans Serif");
    CFont* pOldFont = pDC->SelectObject(&hpfont);
	CString grchar;
	while (pos != -1){
		if (pos > 0){
			CString prevStr = str.Mid(0, pos);
			pDC->TextOut(point.x, point.y, prevStr);
			CSize size = pDC->GetTextExtent(prevStr);
			point.x = point.x + size.cx;
			szLabel.cx = szLabel.cx + size.cx;
		}

		grchar = str.Mid(pos + 1, 1);
		CFont hfont;
    	hfont.CreateFont(20,0,0,0,FW_DONTCARE,0,0,0,SYMBOL_CHARSET,0,0,0,
    		VARIABLE_PITCH|FF_DONTCARE,"Symbol");
    	CFont* phpFont = pDC->SelectObject(&hfont);
		pDC->TextOut(point.x, point.y, grchar);
		CSize size = pDC->GetTextExtent(grchar);
		point.x = point.x + size.cx;
		szLabel.cx = szLabel.cx + size.cx;
		str = str.Mid(pos + 2);
		pos = str.Find('$');
		pDC->SelectObject(phpFont);
	}
		
    	
    //	CPoint point = GetLabelPosition();
    m_posLabel = CRect(ptLabel, szLabel);
    pDC->TextOut(point.x, point.y, str);
	pDC->SelectObject(pOldFont);
	pDC->SetBkMode(oldMode);
	pDC->SetTextColor(oldColor);
		
}
/*
    
BOOL CAngle::OnEditProperties()
{
    	
    	CString strOldName = m_strName;
    	CAngleDlg dlg(this);
    	// Pop up the property dialogue
    	if (LogModalDlg(dlg)!= IDOK)
    		return FALSE;								// dialog was cancelled
    
    
    	// set modified flag in document
    	if (m_pDocument != NULL) 
    		m_pDocument->SetModifiedFlag();
    	// invalidate object in case change is visible (e.g. label)
    	Invalidate();
    	return TRUE;
}
    
 */   	
    
CRect CAngle::GetAngleRect(CPoint intercept, int radius)
{
    	int x1 = intercept.x - radius;
    	int y1 = intercept.y - radius;
    	int x2 = intercept.x + radius;
    	int y2 = intercept.y + radius;
    
    	CRect arcRect;
    	arcRect.left = x1;
    	arcRect.top = y1;
    	arcRect.right = x2;
    	arcRect.bottom = y2;
    
    	return arcRect;
}

// updates both m_degAng and m_degDrawn to reflect current state   
void CAngle::SetAngleMag()
{
	// Update drawn direction from drawn directions of sides.
   	int angle1 = m_pAngSide1->GetDirection();
	if (m_pAngSide1->IsKindOf(RUNTIME_CLASS(CAxes))){
		angle1 = (angle1 + ((m_nAxis-POSX) * 90)) % 360;
	}
   	int angle2 = m_pAngSide2->GetDirection();
	if (m_pAngSide2->IsKindOf(RUNTIME_CLASS(CAxes))){
		angle2 = (angle2 + ((m_nAxis-POSX) * 90)) % 360;
	}
	
	if (angle1>angle2)
   		m_degDrawn = 360 - abs(angle1 - angle2);
  	else
   		m_degDrawn = abs(angle1 - angle2);

	// Update m_degAng.
	// check against drawings representing vectors of unknown orientation
	if ( (m_pAngSide1->IsKindOf(RUNTIME_CLASS(CVector)) &&
		 ((CVector*)m_pAngSide1)->UnknownDir()) 
		|| (m_pAngSide2->IsKindOf(RUNTIME_CLASS(CVector)) &&
		   ((CVector*)m_pAngSide2)->UnknownDir()) ) {
		 m_degAng = -1;
	} else // both sides represent known directions
		m_degAng = m_degDrawn;
}

    
int CAngle::GetHandleCount()
{
    	return (3);
}
    
CPoint CAngle::GetHandle(int nHandle)
{
    	CFBDView* pView = theApp.GetFBDView();
    	if (nHandle == 2)
    	{
    		double angFromZero;
    		double const pi = 3.1415926535;
    		angFromZero = (((double)(m_pAngSide1->GetDirection())) +
    								((double)m_degDrawn/2.0));
			if (m_pAngSide1->IsKindOf(RUNTIME_CLASS(CAxes))){
				if (m_nAxis == -1)
					angFromZero = angFromZero + ((((CAxes*)m_pAngSide1)->m_nAxis-POSX) * 90);
				else
					angFromZero = angFromZero + ((m_nAxis-POSX) * 90);
			}
    		double radsFromZero = angFromZero*((2.0*pi)/360.0);
    		double realx = m_radius*(cos(radsFromZero));
    		double realy = -m_radius*(sin(radsFromZero));
    		int x = (int)realx;
    		int y = (int)realy;
    		x = x + m_intercept.x;
    		y = y + m_intercept.y;
    		return CPoint(x, y);
    	}
    	else 
    		return CDrawObj::GetHandle(nHandle == 3 ? 5 : nHandle);
    
}
    
HCURSOR CAngle::GetHandleCursor(int nHandle)
{
	ASSERT_VALID(this);
   
   	LPCTSTR id;
	if (nHandle == 2){
		int dir = GetDirection();
   		if ( ((dir>=23)&&(dir<68)) || ((dir>=203)&&(dir<248)) )
			id = IDC_SIZENESW;
		else if ( ((dir>=68)&&(dir<113)) || ((dir>=261)&&(dir<306)) )
   			id = IDC_SIZENS;
   		else if ( ((dir>=113)&&(dir<158)) || ((dir>=248)&&(dir<261)) )
   			id = IDC_SIZENWSE;
		else if ( ((dir>=158)&&(dir<203)) || (dir>=306) || (dir<23) )
   			id = IDC_SIZEWE;
	}
	else
		id = IDC_ARROW;
   

	return AfxGetApp()->LoadStandardCursor(id);
}
 
CString CAngle::GetDef()
{
	CString strDef;
	CString str1, str2, strAxis;
	str1 = m_pAngSide1->m_strName;
	str2 = m_pAngSide2->m_strName;

	if (m_nAxis != -1)
		strAxis = axes[m_nAxis-2].strDef;
	if (str1.IsEmpty())
		str1 = strAxis;
	else if (str2.IsEmpty())
		str2 = strAxis;
	strDef.Format("Angle between %s and %s", str1, str2);
	return strDef;
    
}

CString CAngle::GetPrintDef()
{
	
	return GetDef();
    
} 
  
void CAngle::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{
    	double const pi = 3.1415926535;
//	if (nHandle == 1)
//	{
//		double bound = pow((point.x-m_intercept.x), 2)+pow((point.y-m_intercept.y), 2);
//		if ( (bound > (pow((m_radius+5),2))) || (bound < (pow((m_radius-5),2))) )
//			return;
//		CPoint ptTo = m_pAngSide1->m_position.BottomRight();
//		CPoint ptFrom = m_pAngSide1->m_position.TopLeft();
//		int dy = point.y - m_position.top;
//		int dx = point.x - m_position.left;
//		double d = sqrt((dx*dx) +(dy*dy));
//		double rad = 2.0*(sinh(d/(m_radius*2.0)));
//		int dir = pView->CCW(m_intercept, ptTo, point);
//		double radCCW = dir*rad ;
    	//	/don't forget that if the intercept is not the top left that the line 
    		//must be pivoted around the intercept
//		CRect newPos = RotateLine(m_intercept, ptFrom, ptTo, radCCW);
//		CPoint endPt= m_position.BottomRight();	
//		m_pAngSide1->MoveTo(newPos, pView);
//		CPoint startPt= pView->GetPointOnLine(m_pAngSide1, m_intercept, m_radius);
//		CRect position = CRect(startPt, endPt);
//		MoveTo(position, pView);
//	}
//	else*/ 
    	if (nHandle == 2){
    		CRect position = m_position;
    		int radius = pView->GetRadius(m_intercept, point);
    		if ((radius > m_maxRadius)||(radius < 20))
    			return;
    		CFBDView* pView = theApp.GetFBDView();
    		CPoint startPt= pView->GetPointOnLine(m_pAngSide1, m_intercept, radius, m_nAxis);
    		CPoint endPt= pView->GetPointOnLine(m_pAngSide2, m_intercept, radius, m_nAxis);
    		m_radius = radius;
    		position = CRect(startPt, endPt);
    		MoveTo(position, pView);
    	}
    	/*
    	else{
    		double bound = pow((point.x-m_intercept.x), 2)+pow((point.y-m_intercept.y), 2);
    		if ( (bound > (pow((m_radius+5),2))) || (bound < (pow((m_radius-5),2))) )
    			return;
    		CPoint ptTo = m_pAngSide2->m_position.BottomRight();
    		CPoint ptFrom = m_pAngSide2->m_position.TopLeft();
    		int dy = point.y - m_position.bottom;
    		int dx = point.x - m_position.right;
    		double d = sqrt((dx*dx) +(dy*dy));
    		double rad = 2.0*(sinh(d/(m_radius*2.0)));
    		int dir = pView->CCW(m_intercept, ptTo, point);
    		double radCCW = dir*rad;
    	//	/don't forget that if the intercept is not the top left that the line 
    		//must be pivoted around the intercept
    		CRect newPos = RotateLine(m_intercept, ptFrom, ptTo, radCCW);
    		CPoint startPt= m_position.TopLeft();
    		m_pAngSide2->MoveTo(newPos, pView);
    		CPoint endPt= pView->GetPointOnLine(m_pAngSide2, m_intercept, m_radius);
    		
    		CRect position = CRect(startPt, endPt);
    		MoveTo(position, pView);
    		
    		
    	}*/
    
    	
}
    
void CAngle::MoveTo(const CRect& position, CFBDView* pView)
{
    	CDrawObj::MoveTo(position, pView);
}
    
    
    
CRect CAngle::RotateLine(CPoint intercept, CPoint startPt, CPoint endPt, double ang)
{
    	CPoint newStartPt;
    	CPoint newEndPt;
    	double dCos = cos(ang);
    	double dSin = sin(ang);
    
    	newStartPt.x = (int)( ((double)startPt.x*dCos) - ((double)startPt.y*(-dSin)) + 
    		((double)intercept.x *(1.0-dCos)) + ((double)intercept.y *(-dSin)) );
    	newStartPt.y = (int)( ((double)startPt.x*(-dSin)) + ((double)startPt.y*dCos) +
    		((double)intercept.y *(1.0-dCos)) - ((double)intercept.x * (-dSin)) );
    	newEndPt.x = (int)( ((double)endPt.x*dCos) - ((double)endPt.y*(-dSin)) + 
    		((double)intercept.x *(1.0-dCos)) + ((double)intercept.y *(-dSin)) );
    	newEndPt.y = (int)( ((double)endPt.x*(-dSin)) + ((double)endPt.y*dCos) +
    		((double)intercept.y *(1.0-dCos)) - ((double)intercept.x * (-dSin)) );
    	return CRect(newStartPt, newEndPt);
    
}
    
    
void CAngle::Delete()
{
    	
	// Find and remove from document
	if (!m_pAngSide1->m_Angles.IsEmpty()){
		POSITION pos1 = m_pAngSide1->m_Angles.Find(this);
		if (pos1 != NULL)
			m_pAngSide1->m_Angles.RemoveAt(pos1);
	}
	if (!m_pAngSide2->m_Angles.IsEmpty()){
		POSITION pos2 = m_pAngSide2->m_Angles.Find(this);
		if (pos2 != NULL)
			m_pAngSide2->m_Angles.RemoveAt(pos2);
	}
   	CDrawObj::Delete();
    
}
    
CPoint CAngle::GetLabelPosition(CSize size)
{
    	CPoint pos;
    	CPoint point = GetHandle(2);
    	if ((point.x >m_intercept.x) && (point.y >m_intercept.y)){
    		pos.x=point.x+10;
    		pos.y=point.y+10;
    	}
    	if ((point.x >m_intercept.x) && (point.y <m_intercept.y)){
    		pos.x=point.x+10;
    		pos.y=point.y-10;
    	}
    	if ((point.x <m_intercept.x) && (point.y >m_intercept.y)){
    		pos.x=point.x-10 - size.cx;
    		pos.y=point.y+10;
    	}
    	if ((point.x <m_intercept.x) && (point.y <m_intercept.y)){
    		pos.x=point.x-10 - size.cx;
    		pos.y=point.y-10;
    	}
    	return pos;
}
    

 
CDialog* CAngle::GetPropertyDlg()
{
	return new CAngleDlg(this);
}



int CAngle::HitTest(CPoint point, CFBDView * pView, BOOL bSelected)
{
	if ((bSelected)||(m_angleRgn.m_hObject == NULL)) // testing for resize handle grab: 
		return CDrawObj::HitTest(point, pView, bSelected);//handle it in base class
	// Also ensuring that region exists and angle hasn't bee resized to a point
	
	//return Angle Part hit
// first see if hit system region
	if (m_angleRgn.PtInRegion(point))
		return 1;
	// else check if hit on system label for non-selected case
	if (!m_strName.IsEmpty()) {
		CRect posLabel = m_posLabel;
		posLabel.NormalizeRect();
		if (posLabel.PtInRect(point))
			return 1;
	}
	return 0;

}

void CAngle::CheckObject()
{
	if (! theApp.CheckDiagram()) return;

	LPCTSTR pszResult;
	CString axis;

	if (m_nAxis == -1)
		axis = "NIL";
	else 
		axis = axes[m_nAxis-2].strHelp;

	// use string so can send NIL for unknown magnitude angle label
	CString strDegreeArg = "NIL";
	if  (! UnknownDir())
		strDegreeArg.Format("%d", m_degAng);

	pszResult = HelpSystemExecf( "(label-angle \"%s\" %s %s %s %s %s)", 
						STR2ARG(m_strName), 
						strDegreeArg,
						STR2ARG(m_pAngSide1->m_strId),
						STR2ARG(m_pAngSide2->m_strId), 
						m_strId,
						axis);
	ApplyStatus(pszResult);
}


BOOL CAngle::CreateAngleRegion(CFBDView* pView)
{
	CRgn iCrcRgn;//inner circle region
	CRect iRect = GetAngleRect(m_intercept, m_radius + 2);
	if (!iCrcRgn.CreateEllipticRgnIndirect(&iRect))
		return FALSE;

	CRgn oCrcRgn;//outer circle region
	CRect oRect = GetAngleRect(m_intercept, m_radius - 2);
	if (!oCrcRgn.CreateEllipticRgnIndirect(&oRect))
		return FALSE;

	CRgn rgn;////doughnut region about 4 pixels in diameter (includes angle path)
	rgn.CreateRectRgn(1,1,2,2);//Combine region needs valid m_hObject
	if (rgn.CombineRgn(&oCrcRgn, &iCrcRgn, RGN_XOR) == ERROR)
		return FALSE;

	CRgn tempRgn;//triangular region (pie slice of angle)
	CPoint points[3];
	points[0] = pView->ExtendEndLine(m_pAngSide1, -1);
	points[1] = m_intercept;
	points[2] = pView->ExtendEndLine(m_pAngSide2, -1);
	if (!tempRgn.CreatePolygonRgn(points, 3, ALTERNATE))
		return FALSE;
	//Any existing GDIObject is deleted from angle region
	m_angleRgn.DeleteObject();
	m_angleRgn.CreateRectRgn(1, 1, 2, 2);//Combine region needs valid m_hObject
	//final combination gets angle path
	if (m_angleRgn.CombineRgn(&rgn, &tempRgn, RGN_AND) == ERROR)
		return FALSE;
	return TRUE;
}

int CAngle::GetDirection()
{
	CPoint ptFrom = m_intercept;
	CPoint ptTo = GetHandle(2);
	return CDrawObj::GetDirection(ptFrom, ptTo);

}


BOOL CAngle::HasSameDef(CDrawObj* pObj)
{
	//Check object list to see if we already have drawn a system with the
	//same definition
	if (!pObj->IsKindOf(RUNTIME_CLASS(CAngle)))
		return FALSE;
	//if system, check if same body & time
	CAngle* pAng = (CAngle*)pObj;
	if (strcmp(m_strId, pAng->m_strId)==0)
		return FALSE;//we are editing this object
	if ( (m_pAngSide1 == pAng->m_pAngSide1)
		&& (m_pAngSide2 == pAng->m_pAngSide2) && (m_nAxis == pAng->m_nAxis) )
	{
		return TRUE;
	}
	
	return FALSE;
}
 
BOOL CAngle::HasSameDef(CVariable* pVar)
{
	//Check object list to see if we already have declared a variable 
	//with the same definition
	if (_stricmp(pVar->m_strQuantName, "Angle") != 0)//case insensitive
		return FALSE;
	else
	{
		//if angle, check sides
		if (m_pAngSide1->IsKindOf(RUNTIME_CLASS(CVector)))
		{
			if (strcmp(m_pAngSide1->m_strName, pVar->m_strObject) != 0)	
				return FALSE;
		}
		else
		{
			if (strcmp(axes[m_nAxis-2].strDef, pVar->m_strObject) != 0)
				return FALSE;

		}
		if (m_pAngSide2->IsKindOf(RUNTIME_CLASS(CVector)))
		{
			if ( strcmp(m_pAngSide2->m_strName, pVar->m_strAgent) != 0 )
				return FALSE;	
		}
		else
		{
			if (strcmp(axes[m_nAxis-2].strDef, pVar->m_strAgent) != 0)
				return FALSE;
		}
	}

	return TRUE;
}

CDrawObj* CAngle::Clone()
{
	// create duplicate w/same properties 
	CAngle* pClone = new CAngle(m_position);

	pClone->m_strId = m_strId;	// Initially, use same id, if added to document, 
								// different id will be generated

	pClone->m_strName = m_strName;

	pClone->m_degAng = m_degAng;
	pClone->m_nAxis = m_nAxis;
	pClone->m_pAngSide1 = m_pAngSide1;
	pClone->m_pAngSide2 = m_pAngSide2;
    //Needed to add because checking temporary cloned object which may refer to its
	//m_pDocument
   	pClone->m_pDocument = m_pDocument;
    
	return (pClone);
}

void CAngle::UpdateObj(CDrawObj* pObj)
{
	// transfer dlg props into object
	CAngle* pTempAng = (CAngle*)pObj;
	m_strName = pTempAng->m_strName;
	m_degAng = pTempAng->m_degAng;
	m_nAxis = pTempAng->m_nAxis;
	m_pAngSide1 = pTempAng->m_pAngSide1;
	m_pAngSide2 = pTempAng->m_pAngSide2;
	m_status = pTempAng->m_status;

	//need to log these properties

}

void CAngle::UpdatePosition()
{
	CFBDView* pView = theApp.GetFBDView();

	CPoint startPt = pView->GetPointOnLine(m_pAngSide1, m_intercept, m_radius, m_nAxis);
	CPoint endPt = pView->GetPointOnLine(m_pAngSide2, m_intercept, m_radius, m_nAxis);
	m_position = CRect(startPt, endPt);
}

void CAngle::LogEntry()
{
	CString strDrawObj = GetDrawObjLogPart();

	// save a lot of cruft that can be recomputed, cause don't understand it	
	LogEventf(EV_FBD_ENTRY, "%s %s %s %d %d %d %d %d %d", strDrawObj, 
		m_pAngSide1->m_strId, m_pAngSide2->m_strId, m_nAxis, m_degAng,
		m_intercept.x, m_intercept.y, m_radius, m_maxRadius);
}

BOOL CAngle::SetFromLogStr(LPCTSTR pszStr)
{
	LPCSTR pszRest = ParseDrawObjLogPart(pszStr);
	if (pszRest == NULL) return FALSE;
	char szSide1[80]; char szSide2[80];
	if (sscanf(pszRest, "%s %s %d %d %d %d %d %d", 
		szSide1, szSide2, &m_nAxis, &m_degAng,
		&m_intercept.x, &m_intercept.y, &m_radius, &m_maxRadius) != 8) return FALSE;
	// have to restore links to objects -- but possible they are not created yet,
	// if list out of order. Would have to patch up after all objects created.
	m_pAngSide1 = theApp.GetDocument()->Lookup(szSide1);
	m_pAngSide2 = theApp.GetDocument()->Lookup(szSide2);
	// must restore back pointers in side objects.
	if (m_pAngSide1) m_pAngSide1->m_Angles.AddTail(this);
	if (m_pAngSide2) m_pAngSide2->m_Angles.AddTail(this);
	return TRUE;
}   

//////////////////////////////////////////////////////////////////////////
// Radius:
// Represents a radius of a circular path to be resized by the user.
// 
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CRadius, CCheckedObj, VERSIONABLE_SCHEMA | 1);
CRadius::CRadius(){}				// called from serialization only
    
#ifdef _DEBUG
void CRadius::Dump(CDumpContext& dc) const
{
	CDrawObj::Dump(dc);
	// dc << "Name: " << m_strName << "\n";
}
#endif // _DEBUG
    
void CRadius::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	// document versions >= 1 added serialization of base class descriptor
	if (nDocVersion >= 1) {
		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
	}
	CDrawObj::Serialize(ar);
	if (ar.IsStoring()){        // store object specific data
		// version 0 had nothing
		ar << m_strBodies;
	} else {							// load object specific data
		ar >> m_strBodies; 
		// else earlier version, no data
	}
}

void CRadius::LogEntry()
{
	CString strDrawObj = GetDrawObjLogPart();
	CString strBodies = ValToArg(m_strBodies);
	
	LogEventf(EV_FBD_ENTRY, "%s %s", strDrawObj, strBodies);
}

BOOL CRadius::SetFromLogStr(LPCTSTR pszStr)
{
	LPCSTR pszRest = ParseDrawObjLogPart(pszStr);
	if (pszRest == NULL) return FALSE;
	char szBodies[80];
	if (sscanf(pszRest, "%s", szBodies) != 1) return FALSE;
	m_strBodies = ArgToVal(szBodies);
	return TRUE;
}

CRadius::CRadius(const CRect& position)
	:CCheckedObj(position)
{
}

void CRadius::Draw(CDC* pDC)
{
	// Draw the oval that represents the circular path
	CPen pen;
	if (! pen.CreatePen(PS_INSIDEFRAME, 2, StatusColor()))
			return ;
	CBrush brNull;
	brNull.CreateStockObject(NULL_BRUSH);
	CPen* pOldPen = pDC->SelectObject( &pen );
	CBrush* pOldBrush = pDC->SelectObject(&brNull);

	pDC->Ellipse(m_position);
	// draw the line that identifies the radius
	CRect normPos = m_position;		// position
   	/* normPos.NormalizeRect();	*/	
    	
   	// this gets the center regardless of left/right and top/bottom ordering
   	int xCenter = normPos.left + normPos.Width() / 2;
   	int yCenter = normPos.top + normPos.Height() / 2;	
	int xRight  = normPos.left + normPos.Width();
	pDC->MoveTo(xCenter, yCenter);
	pDC->LineTo(GetHandle(4));


	pDC->SelectObject(pOldPen);
	pDC->SelectObject(pOldBrush);
	
	//Create the circular region
	//Any existing GDIObject is deleted from circle region
	m_circleRgn.DeleteObject();
	m_circleRgn.CreateEllipticRgnIndirect(&m_position);

	// draw the label
	if (! m_strName.IsEmpty()) {
			int xsign = (normPos.Width() >= 0) ? 1 : -1;
			CSize extText = pDC->GetTextExtent(m_strName);
			const int delta = 8;
			
		if (xsign == 1) {
				m_posLabel = CRect( CPoint(xRight + delta,  
										   yCenter), extText);
		} 
		else if (xsign == -1) {
				m_posLabel = CRect( CPoint(xRight - (delta + extText.cx),  
    									  yCenter), extText);
		} 


		int oldBkMode = pDC->SetBkMode(TRANSPARENT);
		pDC->TextOut(m_posLabel.left, m_posLabel.top, m_strName);
		pDC->SetBkMode(oldBkMode);
	}
}

// MoveHandleTo -- adjust position to move point specified by the given resize 
//                 handle index to the given point. 
//
// View arg used as in MoveTo, which does the work. 
// Point arg must be in logical coords.
void CRadius::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{
   	ASSERT_VALID(this);
   
   	CRect position = m_position;
	int deltalx = position.left - point.x;
	int deltaty = position.top - point.y;
 	int deltarx = position.right - point.x;
	int deltaby = position.bottom - point.y;
  	switch (nHandle)
   	{
   	default:
   		ASSERT(FALSE);
   
   	case 1:
   		position.left = point.x;
   		position.top = point.y;
		position.bottom += deltaty;
		position.right += deltalx;
   		break;
   
   	case 2:
   		position.top = point.y;
		position.bottom += deltaty;
   		break;
   
   	case 3:
   		position.right = point.x;
   		position.top = point.y;
		position.bottom += deltaty;
		position.left += deltarx;
   		break;
    
   	case 4:
   		position.right = point.x;
		position.left += deltarx;
   		break;
    
   	case 5:
   		position.right = point.x;
   		position.bottom = point.y;
		position.top += deltaby;
		position.left += deltarx;
   		break;
    
   	case 6:
   		position.bottom = point.y;
		position.top += deltaby;
   		break;
   
   	case 7:
   		position.left = point.x;
   		position.bottom = point.y;
 		position.top += deltaby;
		position.right += deltalx;
  		break;
    
   	case 8:
   		position.left = point.x;
		position.right += deltalx;
  		break;
   	}
   
   	MoveTo(position, pView);
}

int CRadius::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
	if ((bSelected)||(m_circleRgn.m_hObject==NULL)) // testing for resize handle grab: 
		return CDrawObj::HitTest(point, pView, bSelected);//handle it in base class
	// Also ensuring region exists and system hasn't been resized to a point

	// else testing if hit system
	
	// first see if hit system region
	if (m_circleRgn.PtInRegion(point))
		return 1;
	// else check if hit on system label for non-selected case
	if (!m_strName.IsEmpty()) {
		CRect posLabel = m_posLabel;
		posLabel.NormalizeRect();
		if (posLabel.PtInRect(point))
			return 1;
	}
	return 0;
    
}

void CRadius::CheckObject()
{
	if (! theApp.CheckDiagram()) return;

	LPCTSTR pszResult;
	pszResult = HelpSystemExecf( "(label-radius \"%s\" %s %s)", 
						STR2ARG(m_strName), 
						m_strId,
						STR2ARG(m_strBodies));
	ApplyStatus(pszResult);
}


CRect CRadius::GetBoundingBox()
{
	// base class returns fixed-up normalized position rect
	return CDrawObj::GetBoundingBox() | m_posLabel;		// union of the rects
}
  
CDialog* CRadius::GetPropertyDlg()
{
	return new CRadiusDlg(this);
}
    

CString CRadius::GetPrintDef()
{
	CString strDef;
	strDef.Format("%s %s", "Radius", m_strBodies);
	return strDef;
    
}
    
CString CRadius::GetDef()
{
	CString strDef;
	m_strBodies.TrimRight();
	strDef.Format("Radius of Circular Motion of the %s", m_strBodies);
	return strDef;
    
}

CString CRadius::GetLabelPrefix()
{
	return "r";
}

BOOL CRadius::HasSameDef(CVariable* pVar)
{
	//Check object list to see if we already have declared a variable 
	//with the same definition
	CString strBodies = m_strBodies;
	strBodies.TrimRight();
	if (_stricmp(pVar->m_strQuantName, "radius")==0 ||
		_stricmp(pVar->m_strQuantName, "revolution-radius") == 0){//case insensitive
		//if system, check if same body & time
		if (strcmp(strBodies, pVar->m_strObject) == 0)				
			return TRUE;			
	}
	return FALSE;

}

BOOL CRadius::HasSameDef(CDrawObj* pObj)
{
	//Check object list to see if we already have drawn an system with the
	//same definition
	if (!pObj->IsKindOf(RUNTIME_CLASS(CRadius)))
		return FALSE;

	//if system, check if same body & time
	CRadius* pRad = (CRadius*)pObj;
	if (strcmp(m_strId, pRad->m_strId)==0)
		return FALSE;//we are editing this object
	if (strcmp(m_strBodies, pRad->m_strBodies) == 0)
		return TRUE;			
	
	return FALSE;
}

    
CDrawObj* CRadius::Clone()
{
	// create duplicate w/same properties 
	CRadius* pClone = new CRadius(m_position);

	pClone->m_strId = m_strId;	// Initially, use same id, if added to document, 
								// different id will be generated

	pClone->m_strName = m_strName;

	pClone->m_strBodies = m_strBodies;
    //Needed to add because checking temporary cloned object which may refer to its
	//m_pDocument
   	pClone->m_pDocument = m_pDocument;
   
	return (pClone);
}
    
BOOL CRadius::CanDuplicate()
{
	return TRUE;
}
 
void CRadius::UpdateObj(CDrawObj* pObj)	
{
	// transfer dlg props into object
	CRadius* pTempRad = (CRadius*)pObj;
	m_strName = pTempRad->m_strName;
	m_strBodies = pTempRad->m_strBodies;
	m_status = pTempRad->m_status;
	
	// Log the new properties
	LogEventf(EV_PROPS_RADIUS, "%s name %s body", (LPCTSTR) m_strName, m_strBodies);
}












