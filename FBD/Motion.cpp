///////////////////////////////////////////////////////////////////////////
//
// Motion.cpp -- Implementation of motion diagram drawing objects
//
///////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include <math.h>

#include "FBD.h"
#include "history.h"
#include "HelpIfc.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "Motion.h"
#include "FBDView.h"

// Property dialogs
#include "MotDlg.h"

//
// Motion diagrams -- graduated position axis ("ruler") on which to plot displacement 
// of a body at successive times at equal intervals and indicate velocity and acceleration. 
//
// Motion diagrams function as containers which include a set of child objects.
// However, since we have not implemented a general notion of containment in our
// DrawObj class, currently all contained objects are added to the document's main 
// object list. The diagram maintains its own lists referencing these document objects.
// 
// m_entries is the list of entered bodies (also called time points), represented by 
// CMotionBody objects and  look like systems Each body has a back pointer to its containing 
// diagram parent. Therefore each must update the other  in some way on deletions, to avoid 
// dangling references. 
//
// The diagram also contains a list of vectors. This may include the velocities associated 
// with time points, and one or more acceleration representations. Currently all our
// problems use constant acceleration, which therefore need not be associated with
// any time point, although the representation allows for them to be.
//
// The association between vectors and time points is represented by the body name
// property of the vector, which contains the time point name on the body. Several
// helper functions exist to extract attributes from this representation (which is
// probably suboptimal, given the use we need to make of it in constraining the drawing).
//
// The diagram also references a DocArea object delimiting its bounding area;
// this affects the interpretation of drawing operations in the view. This may be a 
// fixed-size area pre-drawn on the page by the problem author (e.g "Draw Motion diagram 
// in this space:") or one created as a result of the student's inserting a motion diagram 
// ruler which changes size with changes in the ruler. 
// m_bOwnArea is true in the latter case (the ruler "owns" the area.).
//
// Note that we adhere to that convention that a time *number* variable (eg. "nTimeNum") is a 
// 1-based (ordinal) time point number. This is the primary representation of time points
// used throughout the MD code, because the help system originally required 1-based indices, 
// although that has since changed. Unfortunately, a couple of methods use a zero-based 
// time point *index* (e.g. "i") as noted. !! Should change everything to use zero based.
//
IMPLEMENT_SERIAL(CMotionDiagram, CCheckedObj, VERSIONABLE_SCHEMA | 0);
CMotionDiagram::CMotionDiagram() 				// used by serialization
{
	m_pArea = NULL;
	m_bOwnArea = FALSE;
	m_p2DParent = NULL;
}
CMotionDiagram::CMotionDiagram(const CRect& position)
	:CCheckedObj(position)
{
	m_nIntervalType = Default;
	m_bOwnArea = FALSE;
	m_pArea = NULL;
	m_p2DParent = NULL;
}

// Clean up on Motion diagram destruction:
CMotionDiagram::~CMotionDiagram()	
{
	// All our contained children just became orphans: destroy them.
	while (!m_entries.IsEmpty()) {
		// Unlink from diagram's list, snapping back pointer. 
		CMotionBody* pBody = m_entries.RemoveHead();
		pBody->m_pDiagram = NULL;	
		// Unlink from document's object list
		m_pDocument->Remove(pBody);
		// destroy object
		pBody->Delete();
	}
	
	while (!m_vectors.IsEmpty()) {
		CMDVector* pVec = m_vectors.RemoveHead();
		pVec->m_pDiagram = NULL;
		m_pDocument->Remove(pVec);
		pVec->Delete();
	}
	
	// Remove diagram area and free if we created it.
	// (Area can be deleted before this, e.g. if freeing everything on document 
	// close. In that case it's destructor should have nulled our link.
	if (m_pArea && m_bOwnArea) {
		m_pDocument->Remove(m_pArea);
		m_pArea->Delete();
	}

	// If "parented" to a dependent 2D diagram, must update it when we go away.
	if (m_p2DParent) {
		// To add:  m_p2DParent->OnDestroyComponent(this);
		// For now, just destroy it. 2D destructor will unlink this and other 1D diagram.
		m_pDocument->Remove(m_p2DParent);
		delete m_p2DParent;
	}
}

void CMotionDiagram::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	// document versions >= 1 added serialization of base class descriptor
	if (nDocVersion >= 1) {
		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
	}
	CDrawObj::Serialize(ar);

	if (ar.IsStoring())        // store object specific data
	{
		ar << m_strBody;
		ar << (WORD) m_nIntervalType;
		ar << (WORD) m_nInterval;
		ar << m_strUnits;
		ar << (WORD) m_bOwnArea;
		ar << m_pArea;
		ar << m_p2DParent;
		m_entries.Serialize(ar);
		m_vectors.Serialize(ar);
	}
	else // load object specific data
	{
		WORD wTemp; 
		ar >> m_strBody;
		ar >> wTemp;  m_nIntervalType = (IntervalType) wTemp;
		ar >> wTemp; m_nInterval = wTemp;
		ar >> m_strUnits;
		ar >> wTemp; m_bOwnArea = wTemp;
		ar >> m_pArea;
		ar >> m_p2DParent;
		m_entries.Serialize(ar);
		m_vectors.Serialize(ar);
		// must set back pointers to parent the contained vectors to diagram, since
		// this is missing from generic CVector serialization. (Didn't add it there because
		// likely to change, so don't want to make a new version)
		POSITION pos = m_vectors.GetHeadPosition();
		while (pos) {
			CMDVector* pVec = m_vectors.GetNext(pos);
			pVec->m_pDiagram = this;
		}
	}
	
}

// If Ruler not drawn on author's predefined Diagram area, 
// we create a DocArea around ruler and add it to doc 
// 
void CMotionDiagram::CalcDiagramArea(CRect& rcArea)
{
// For now, defining area in simple way. Could be changed to be polygon for
// slanted rulers, say.
#define MOTION_BORDER_X 72	// offset around ruler that constitutes diagram box
#define MOTION_BORDER_Y 72

	CRect rcRuler = m_position;
	rcRuler.NormalizeRect();
	rcArea = rcRuler;
	rcArea.InflateRect(MOTION_BORDER_X, MOTION_BORDER_Y);
}

void CMotionDiagram::CreateArea()
{
	ASSERT(m_pArea == NULL);

	CRect rcArea;
	CalcDiagramArea(rcArea);

	m_pArea = new CDocArea(rcArea);
	m_bOwnArea = TRUE;
	m_pArea->m_strName = CDocArea::c_szMotion; 
	m_pArea->m_bBorder = FALSE;		  // don't add solid border; will draw grey dotted border
	m_pArea->m_flag = TEACHER_OBJECT; // so student can't select and move it.

	m_pDocument->Add(m_pArea);
	// Move it to back of document so it's behind ruler
	m_pDocument->MoveToBack(m_pArea); // Invalidates
}


void CMotionDiagram::Draw(CDC* pDC)
{
	CRect rect = m_position;

	CPen penGuide(PS_SOLID, 3, StatusColor());
	CPen* pOldPen = pDC->SelectObject( &penGuide );

	// Draw major ruler line from origin in top left to endpoint in bottom right
	pDC->MoveTo(rect.TopLeft());
	pDC->LineTo(rect.BottomRight());

	// Calculate sizes for gradation hash markings:
	int cxStep; 			// count of LUs in step between hash markings
	int cyStep;				// !!! no real use for different x & y steps
	int nHeight;			// height minor interval hash mark extends above ruler line
	// Minor interval cxStep is set to 4 [invisible] grid steps = 2 Visible grid lines
	// With current grid that gives ruler 1/4 inch steps every 24 LUs. 
	CFBDView* pView = theApp.GetFBDView();
	if (pView != NULL) {
		cxStep= pView->m_cxGrid * 4;	
		cyStep = pView->m_cyGrid * 4;
		nHeight = pView->m_cxGrid;
	} else {	// Couldn't find FBD View?! Use hardcoded defaults
		cxStep = cyStep = 24;
		nHeight = 6;
	}
	CPen penHash(PS_SOLID, 1, StatusColor());
	pDC->SelectObject(& penHash);

	if (rect.top == rect.bottom) // Horizontal Ruler
	{
		int xmin = min(rect.left, rect.right);
		int xmax = max(rect.left, rect.right);
		for (int x = xmin, n = 0; x <= xmax; x += cxStep, ++n)
		{
			int nFactor = (n % 4 == 0) ? 2 : 1; // major divisions twice as high
			pDC->MoveTo(x, rect.top - nFactor * nHeight);
			pDC->LineTo(x, rect.top + nFactor * nHeight);
		}
	}
	else if (rect.right == rect.left) // Vertical Ruler
	{
		int ymin = min(rect.top, rect.bottom);
		int ymax = max(rect.top, rect.bottom);
		for (int y = ymin, n = 0; y <= ymax; y += cyStep, ++n)
		{
			int nFactor = (n % 4 == 0) ? 2 : 1; // major divisions twice as high
			pDC->MoveTo(rect.left - nFactor * nHeight, y);
			pDC->LineTo(rect.left + nFactor * nHeight, y);
			
		}
	}
	else // Tilted ruler
	{
		int x0 = rect.left; int y0 = rect.top;
		int x1 = rect.right; int y1 = rect.bottom;
		int dx = x1 - x0;
		int dy = y1 - y0;
		double theta = atan2(dy, dx);	// angle of ruler line (counterclockwise)
		// Keep step sizes as floats to avoid loss of precision before 
		// using in computations of drawing coords. [Ex: step of 3.3 LUs
		// comes at 0, 3, 7, 10 -- desirable?]. But round offsets to
		// integral amounts so all hash marks exactly parallel.
		double fxStep = (cxStep * cos(theta));	// x and y components of displacement
		double fyStep = (cyStep * sin(theta));	// between marked intervals (signed)
		// hash marks angled at 90 - theta, so just switch sin and cos to 
		// project magnitudes of offsets. Signs handled below.
		int xDelta = Round(nHeight * sin(theta));// offset to endpoint of hash mark, x
		int yDelta = Round(nHeight * cos(theta));// offset to endpoint of hash mark, y
		int nMarks = abs(int(dx/fxStep));			// no. of hash marks to draw
		int n = 0;
		for (double fx = x0, fy = y0; n <= nMarks; fx += fxStep, fy += fyStep, ++n)
		{
			int nFactor = (n % 4 == 0) ? 2 : 1; // major divisions bigger
			pDC->MoveTo(Round(fx) - nFactor * xDelta, Round(fy) + nFactor * yDelta);
			pDC->LineTo(Round(fx) + nFactor * xDelta, Round(fy) - nFactor * yDelta);
		}
	}
	
	pDC->SelectObject(pOldPen);
}

int CMotionDiagram::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
	if (bSelected)
		return CDrawObj::HitTest(point, pView, bSelected);

	CRect rect = m_position;
	rect.NormalizeRect();
	if (rect.top == rect.bottom) // Horizontal Ruler
	{
		return (point.x >= rect.left && point.x < rect.right
			    && abs(point.y - rect.top) <= 6);

	}
	else if (rect.right == rect.left)	// Vertical Ruler
	{
		return (point.y >= rect.top && point.y < rect.bottom
			    && abs(point.x - rect.left) <= 6);
	}
	else
		return CDrawObj::HitTest(point, pView, bSelected);

}

// stock code for two-handled objects:
int CMotionDiagram::GetHandleCount()
{
	return (2);
}

CPoint CMotionDiagram::GetHandle(int nHandle)
{	
	return CDrawObj::GetHandle(nHandle == 2 ? 5 : nHandle);
}

HCURSOR CMotionDiagram::GetHandleCursor(int nHandle)
{
	return CDrawObj::GetHandleCursor(nHandle == 2 ? 5 : nHandle);
}

// Handles resizing of diagram by grabbing ruler handles. 
// !!! what about moving other contained objects when diagram resizes. 
void CMotionDiagram::MoveHandleTo(int nHandle, CPoint point, CFBDView* pView)
{
	CDrawObj::MoveHandleTo(nHandle == 2 ? 5 : nHandle, point, pView);
	
	// move associated diagram area with it if we have created it.
	// !!! if not, need to ensure ruler is still within it.
	if (! m_bOwnArea )
		return;

	// recalc area position
	CRect rcArea;
	CalcDiagramArea(rcArea);
	// and move it too.
	m_pArea->MoveTo(rcArea, pView);
}

void CMotionDiagram::MoveTo(const CRect& position, CFBDView* pView)
{
	CPoint delta = (CPoint) (position.TopLeft() - m_position.TopLeft());

	// Invalidating whole surrounding area before and after move should suffice.
	// But ruler may not have an area during initial drawing, urgh.
	if (m_pArea)
		m_pArea->Invalidate();
	else
		Invalidate();

	// Move the ruler
	m_position = position;

	// Move the surrounding area if we created it
	if (m_bOwnArea) {
		CRect rcArea;
		CalcDiagramArea(rcArea);
		m_pArea->m_position = rcArea;
	}

	// !!! if had all objects on a single list, could move them as group.
	// !!! if we didn't create area (its fixed size) this could move out of it

	// move all the bodies contained in diagram
	POSITION pos = m_entries.GetHeadPosition();
	while (pos != NULL) 
	{
		CDrawObj* pObj = m_entries.GetNext(pos);
		ASSERT(pObj != NULL);
		CRect position = pObj->m_position + delta;
		pObj->MoveTo(position, pView);
	}

	// move all the vectors contained in diagram
	pos = m_vectors.GetHeadPosition();
	while (pos != NULL) 
	{
		CDrawObj* pObj = m_vectors.GetNext(pos);
		ASSERT(pObj != NULL);
		CRect position = pObj->m_position + delta;
		pObj->MoveTo(position, pView);
	}

	// Update the new area
	if (m_pArea)
		m_pArea->Invalidate();
	else
		Invalidate();

	if (m_pDocument) m_pDocument->SetModifiedFlag();
}

BOOL CMotionDiagram::OnEditProperties()
{
	CString strOldName = m_strName;
	CMotionDlg dlg;
	dlg.m_strName = m_strName;
	dlg.m_strBody = m_strBody;
	dlg.m_nIntervalType = (int) m_nIntervalType;
	if (m_nIntervalType == Measured) {
		dlg.m_nInterval = m_nInterval;
		dlg.m_strUnits = m_strUnits;
	}
	
	if (dlg.DoModal() != IDOK)
		return FALSE;

	m_strName= dlg.m_strName;
	m_strBody = dlg.m_strBody;
	m_nIntervalType = (IntervalType) dlg.m_nIntervalType;
	if (m_nIntervalType == Measured) {
		m_nInterval = dlg.m_nInterval;
		m_strUnits = dlg.m_strUnits;
	}
	// Could have been a change in units, so update everything
	UpdateBodies();
	// UpdateVectors();
	
	// changing props makes doc dirty:
	if (m_pDocument) m_pDocument->SetModifiedFlag();

	// delete old object from database. Checking will add new one
	NotifyChange(strOldName);
	
	// invalidate object in case status changed
	Invalidate();

	return TRUE;
}

void CMotionDiagram::CheckObject()
{	
	// (lookup-md object time dir deltat id
	LPCTSTR pszResult = 
		HelpSystemExecf("(lookup-md %s %s %d %d %s)",
				STR2ARG(m_strBody),
				STR2ARG(m_pDocument->m_strProblemId),	 // time = problem name
				GetDirection(),
				m_nIntervalType == Default ? 0 : m_nInterval,
				STR2ARG(m_strId)  );
	ApplyStatus(pszResult);

}

void CMotionDiagram::NotifyDelete(CString& strOldName)
{
	(void) HelpSystemSendf("(delete-md-object %s %s)", STR2ARG(strOldName), STR2ARG(m_strId));
}

// Returns label for Nth time point. nIndex is *zero-based* index
void CMotionDiagram::GetNthTimeLabel(int nIndex, CString& strLabel)
{
	if (m_nIntervalType == Default)
		strLabel.Format("T%d", nIndex);
	else 
		strLabel.Format("%d %s", nIndex * m_nInterval, m_strUnits);
}
// get next unused time point label
void CMotionDiagram::GetNextTimeLabel(CString& strLabel) // Time to be used next
{
	// count of existing entries always 1 more than max zero-based index in use
	GetNthTimeLabel(m_entries.GetCount(), strLabel);
}

// Special choice string for whole period
const char* CMotionDiagram::szWholePeriod = "Whole Period";

// fill combo box with list of available time points, used for vector properties
// we use drawn time point names, plus a distinguished value to represent value
// constant over whole interval (!!! currently to be used only for acceleration)
// This set is like systems in freebody diagrams; value associates vector with body.
void CMotionDiagram::BodiesToCbo(CComboBox& cbo, int nVectorType /*=ACCEL*/)
{
	// find first time point lacking vector of given type
	for (int i = 0; i < m_entries.GetCount(); i++)	
	{
		if (GetVectorAt(i+1, nVectorType) == NULL) {
			CString strName;
			GetNthTimeLabel(i, strName);
			cbo.AddString(strName);
			break;
		}
	}
	// for accelerations, add choice for whole interval
	if (nVectorType == VECTOR_ACCELERATION)
		cbo.AddString(szWholePeriod);
}

void CMotionDiagram::AddBody(CMotionBody* pBody)
{
	// Generate label if needed
	if (pBody->m_strName.IsEmpty())
		GetNextTimeLabel(pBody->m_strName);

	// Save ordinal number of body.
	// NB: body *Number* is 1-based count, but labels are generated from zero-based *index*
	pBody->m_nNumber = m_entries.GetCount() + 1;

	// Add a reference into diagram's list and set back pointer.
	m_entries.AddTail(pBody);
	pBody->m_pDiagram = this;
	
	// doc now modified
	if (m_pDocument) m_pDocument->SetModifiedFlag();
}

// Find body by zero based array index, returns NULL if not in diagram. 
CMotionBody* CMotionDiagram::GetBody(int i)
{
	POSITION pos = m_entries.FindIndex(i);
	if (pos != NULL)
		return m_entries.GetAt(pos);
	else
		return NULL;
}

// Find body by time point name, returning NULL if not found.
CMotionBody* CMotionDiagram::FindBody(CString& strName)
{
	POSITION pos = m_entries.GetHeadPosition();
	while (pos != NULL) {
		CMotionBody* pBody =  m_entries.GetNext(pos);
		if (pBody->m_strName == strName)
			return pBody;
	}
	return NULL;
}


void CMotionDiagram::UpdateBodies()
{
	// re-label all child objects after some change
	// Now run through entries list and recalc labels ?? Desirable ??
	POSITION pos = m_entries.GetHeadPosition();
	int nCount = 0;
	while (pos != NULL)
	{
		CDrawObj* pObj = m_entries.GetNext(pos);
		ASSERT( pObj->IsKindOf(RUNTIME_CLASS(CMotionBody)) );
		CMotionBody* pBody = (CMotionBody*) pObj;
		GetNthTimeLabel(nCount++, pBody->m_strName);
		Invalidate();
	}
	// !!! what about vectors hanging off them?
}

// UI constraint: test if can delete this body from diagram
BOOL CMotionDiagram::CanDelete(CMotionBody* pBody)
{
	if (pBody->m_pDiagram == NULL) //orphaned, ok to delete
		return TRUE;
	ASSERT(pBody->m_pDiagram == this); // better be in this diagram
	// OK iff last body in list
	return (pBody->m_nNumber == m_entries.GetCount());
}

// removes reference to body from our list, updating back ptr in body.
// (Like RemoveFromSel. Doesn't unlink body from doc or delete it)
void CMotionDiagram::RemoveBody(CMotionBody* pBody)
{
	// first delete all vectors on this body!
	POSITION pos = m_vectors.GetHeadPosition();
	while (pos) {
		CMDVector* pVec = m_vectors.GetNext(pos);
		if (GetVectorBody(pVec) == pBody) {
			// !!! hairy -- modifies the list we are traversing. Should be OK
			// because POSITION advanced past it. 
			// Note following may also propagate an update to 2D parent diagram. 
			RemoveVector(pVec);
			// must also unlink from whole doc list
			m_pDocument->Remove(pVec);
			pVec->Delete();
		}
	}

	// Unlink from list and clear back pointer
	pos = m_entries.Find(pBody);
	if (pos != NULL) {
		m_entries.RemoveAt(pos);
		pBody->m_pDiagram = NULL;
	}

	// Recalc all other body labels (now unnecessary since only delete at end)
	/* UpdateBodies(); */

	// doc now modified
	if (m_pDocument) m_pDocument->SetModifiedFlag();
}

// Add a new vector associated with the given time pt obj to the diagram. 
// Generates name and sets strBody attribute on vector.
// If pBody is NULL, assumes vector not associated with time point
void CMotionDiagram::AddVector(CMDVector* pVector, CMotionBody* pBody)
{
	// Name prefix (V or A) comes from vector type. followed by number of body
	CString strType;
	pVector->GetTypeName(strType);

	if (pBody)	// It is being added to a time point entry
	{
		// Generate a label if necessary
		if (pVector->m_strName.IsEmpty())
		{
			int nUnits = (m_nIntervalType == Default) ?  1 : m_nInterval;
			pVector->m_strName.Format("%c%d", strType[0], nUnits * (pBody->m_nNumber - 1));
		}
		// Set vector's body field to this Time-point's name.
		pVector->m_strBody = pBody->m_strName;
	}
	else	// not attached to any body.  
	{
		// Give it a default name, so label is drawn.
		if (pVector->m_strName.IsEmpty())
			pVector->m_strName.Format("%c", strType[0]);
		/* // Leave body empty, so not associated with any. Will appear to be
		// whole period in some tests, but dialog can see it is unset.
		// Give it the special time point name.
		pVector->m_strBody = szWholePeriod;
		*/
	}
	// Following set readonly prefix to use in dialogs.
	// Change to use same as vector's
	// pVector->SetLabelPrefix(pVector->m_strName);

	// Link into our list and set back pointer
	m_vectors.AddTail(pVector);
	pVector->m_pDiagram = this;

	// doc now modified
	if (m_pDocument) m_pDocument->SetModifiedFlag();
}

/// find vector of given type for 1-based time point number, NULL if none
CMDVector* CMotionDiagram::GetVectorAt(int nTimePoint, int nType)
{
	POSITION pos = m_vectors.GetHeadPosition();
	while (pos != NULL) {
		CMDVector* pVec = m_vectors.GetNext(pos);
		if (pVec->m_nVectorType != nType) // skip if not of right type
			continue;
		CMotionBody* pBody = GetVectorBody(pVec);
		if (pBody && pBody->m_nNumber == nTimePoint)
			return pVec;
	}
	return NULL;
}

// UI constraint tests if vec can be added. Body name should be specified in NewVector
// !!NB: vec may already be in diagram's list -- added provisionally by view, pending
// this check. (Ugh)
BOOL CMotionDiagram::CanAddVector(CMDVector* pNewVector)
{
	CMotionBody* pBody = GetVectorBody(pNewVector);
	if (pBody == NULL) // candiate not on a body
		return TRUE;
	int nNewVector = pBody->m_nNumber;	// 1-based num of candidate's time point
	if (nNewVector > m_entries.GetCount())		// sanity check, shouldn't happen
		return FALSE;
	
	// verify all *earlier* time points have vectors of this type drawn
	// (so shouldn't hit this vector even though its in vector list
	for (int n = 1;  n < nNewVector; n++) {
		CMDVector* pDrawnVec = GetVectorAt(n, pNewVector->m_nVectorType);
		if (pDrawnVec == NULL)
			return FALSE;
	} 
	return TRUE;
}

// UI constraint tests if this vector can be deleted <-> last numbered vector of its kind
BOOL CMotionDiagram::CanDelete(CMDVector* pVec)
{
	if (pVec->m_strBody == szWholePeriod)	// if vec applies over all, fine
		return TRUE;

	// search vecs in reverse till hit last vector of this type in series
	POSITION pos = m_vectors.GetTailPosition();
	while (pos != NULL) {
		CMDVector* pLastVec =  m_vectors.GetPrev(pos);
		if (pLastVec->m_nVectorType == pVec->m_nVectorType) // hit last of this type
			return (pVec == pLastVec);	
	}
	// !!! not found on our list?
	return TRUE;
}

void CMotionDiagram::RemoveVector(CMDVector* pVector)
{
	// First, propagate notification to 2D diagram so it can update before
	// this is destroyed (it may need m_pDiagram in vector).
	if (m_p2DParent)
		m_p2DParent->OnRemoveComponent(pVector);

	// Unlink from our list and clear back pointer
	POSITION pos = m_vectors.Find(pVector);
	if (pos != NULL) {
		m_vectors.RemoveAt(pos);
		pVector->m_pDiagram = NULL;
	}

	// doc now modified
	if (m_pDocument) m_pDocument->SetModifiedFlag();

	
}

// return the body that a vector is associated with, NULL if none
CMotionBody* CMotionDiagram::GetVectorBody(CMDVector* pVector)
{
	// search bodies for match by name to body name set for vector.
	return FindBody(pVector->m_strBody);
}

// return the 1-based time point number of a diagram vector; 0 for whole interval.
// or unknown.
int CMotionDiagram::GetVectorTimeNum(CMDVector * pVec)
{
	if (pVec->m_strBody == szWholePeriod)
		return 0;

	CMotionBody* pBody = GetVectorBody(pVec);
	if (pBody)
		return pBody->m_nNumber;

	// else something's wrong, assume whole interval	
	TRACE("MD::GetVectorTimeNum: no time point!");
	return 0;
}


int CMotionDiagram::GetAccelNum(CMDVector* pVector)
{
	// search vectors, counting accelerations
	int n = 0;
	POSITION pos = m_vectors.GetHeadPosition();
	while (pos != NULL) {
		CMDVector* pVector = (CMDVector*) m_vectors.GetNext(pos);
		if (pVector->m_nVectorType == VECTOR_ACCELERATION) {
			++n;
		}
	}
	return n;
}

// add a vector obj created as a duplicate of an existing one
void CMotionDiagram::AddClone(CMDVector* pClone)
{
	// Link clone into diagram's list and set back pointer
	m_vectors.AddTail(pClone);
	pClone->m_pDiagram = this;

	// A service[?]: when adding duplicates of vectors linked to time pts,
	// we update label to refer to next unused time point. 
	// !!! Next time point object need not have been drawn yet;
	// also repeated clones get same id; also next unused time point might
	// not be next unusued velocity slot. Might be better to disallow or only
	// generate cloning or only generate if next time pt drawn.
	if (pClone->m_strBody != szWholePeriod)	// not linked by name to a body
	{
		// Update clone's label to next unused time pt index
		int nNextTimeIndex = m_entries.GetCount();
		// Name prefix comes from vector type, followed by number of body
		CString strType;
		pClone->GetTypeName(strType);	// "Acceleration" or "Velocity"
		if (m_nIntervalType == Default)
			pClone->m_strName.Format("%c%d", strType[0], nNextTimeIndex);
		else // Have real time numbers
			pClone->m_strName.Format("%c%d", strType[0], m_nInterval * nNextTimeIndex);
		
		// Also update body attribute in vector. 
		GetNextTimeLabel(pClone->m_strBody);
	}
	// doc now modified
	if (m_pDocument) m_pDocument->SetModifiedFlag();
}

// Test if Motion diagram is complete
BOOL CMotionDiagram::IsComplete()
{
	// Ensure all time points exist and have velocities
	if (m_entries.GetCount() != MAX_TIMES)
		return FALSE; 
	for (int n = 1; n <= MAX_TIMES; n++) {
		if (GetVectorAt(n, VECTOR_VELOCITY) == NULL) // nth velocity doesn't exist.
			return FALSE;
	}
	return TRUE;
}

//
// "MotionBody" -- entries for a body at a time in a motion diagram
// 
// Drawn like system dots. They are different in that they contain back ptr
// linked to the parent diagram and inherit the system props from that, so
// other system attributes not used. Also, since we fill in the attributes
// for the student as they are created, they don't have editable properties.
// 
IMPLEMENT_SERIAL(CMotionBody, CCheckedObj, VERSIONABLE_SCHEMA | 0);
CMotionBody::CMotionBody()	// called from serialization only
{	
	m_pDiagram = NULL;
}

CMotionBody::CMotionBody(const CRect& position)
	:CCheckedObj(position)
{
	m_pDiagram = NULL;
}

CMotionBody::~CMotionBody()
{
	// Notify parent we have gone away so it doesn't contain dangling ref.
	if (m_pDiagram != NULL)
		m_pDiagram->RemoveBody(this);
}

void CMotionBody::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	// document versions >= 1 added serialization of base class descriptor
	if (nDocVersion >= 1) {
		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
	}
	CDrawObj::Serialize(ar);

	if (ar.IsStoring()){        // store object specific data
		ar << (WORD) m_nNumber;
		ar << m_pDiagram;
	} 
	else  // load object specific data
	{
		WORD wTemp;
		ar >> wTemp; m_nNumber = wTemp;
		ar >> m_pDiagram;
	}
}




#ifdef _DEBUG
void CMotionBody::Dump(CDumpContext& dc) const
{
	CDrawObj::Dump(dc);
	// dc << "Name: " << m_strName << "\n";
}
#endif // _DEBUG

// Copied from CSystem. Only difference is label placement
void CMotionBody::Draw(CDC* pDC)
{
	// Draw the oval that represents the body
	CBrush brushSystem(StatusColor());
	CBrush* pOldObj = pDC->SelectObject(&brushSystem);

	pDC->Ellipse(m_position);
	pDC->SelectObject(pOldObj);
	
	// draw the label
	if (! m_strName.IsEmpty()) 
	{
		// Currently, label goes right under body.
		COLORREF oldColor = pDC->SetTextColor(StatusColor());
		int oldBkMode = pDC->SetBkMode(TRANSPARENT);
		pDC->TextOut(m_position.left + m_position.Width()/2, 
			         m_position.bottom + 3, m_strName);
		pDC->SetTextColor(oldColor);
		pDC->SetBkMode(oldBkMode);
	}
}


int CMotionBody::HitTest(CPoint point, CFBDView* pView, BOOL bSelected)
{
	if (bSelected) // testing for resize handle grab: 
		return CDrawObj::HitTest(point, pView, bSelected);//handle it in base class
	
	// else testing if hit system. Test with GDI region object
	// !! could filter w/cheap bounding box test before creating region.
	CRgn rgnCircle;
	if (rgnCircle.CreateEllipticRgnIndirect(&m_position) // Create succeeded
		&& rgnCircle.PtInRegion(point) )
		return TRUE;

// else check if hit on system label for non-selected case
//	if (!m_strName.IsEmpty()) 
//	{
//		CRect posLabel = m_posLabel;
//		posLabel.NormalizeRect();
//		if (posLabel.PtInRect(point))
//			return 1;
	
	return FALSE;
}

CDrawObj* CMotionBody::Clone()
{
	// create duplicate w/mostly same properties 
	CMotionBody* pClone = new CMotionBody(m_position);
	pClone->m_status = m_status;

	// Adding to diagram should update time label
	if (m_pDiagram != NULL) {
		m_pDiagram->AddBody(pClone);
	}
	

	return (pClone);
}

// Get offset of body along ruler line. Returns "canonical" direction
// required by help system: from screen left to right or from top to
// bottom if vertical (note this is same as device and our logical coordinates). 
int CMotionBody::GetRulerPos()
{
	ASSERT(m_pDiagram != NULL);

	// identify body loc with midpoint of drawing circle
	CPoint ptBody(m_position.left + m_position.Width()/2,
	              m_position.top + m_position.Height()/2);
	
	// Ruler was drawn from pt in TopLeft to pt in BottomRight. Help sys wants
	// "canonical" dir from screen left to right; or top to bottom if vertical.
	CRect rcRuler = m_pDiagram->m_position;
	CPoint ptRulerOrg, ptRulerEnd;
	if ( rcRuler.left < rcRuler.right
		 || (rcRuler.left == rcRuler.right	/* vertical */
		     && rcRuler.bottom >= rcRuler.top) ) // was drawn our way. 
	{			
		ptRulerOrg = rcRuler.TopLeft();
		ptRulerEnd = rcRuler.BottomRight();
	}
	else {									// was drawn inverted; reverse
		ptRulerOrg = rcRuler.BottomRight();
		ptRulerEnd = rcRuler.TopLeft();
	}
	
	if (rcRuler.left == rcRuler.right)		// vertical ruler: org is top, lower in coords
		return ptBody.y - ptRulerOrg.y;
	else if (rcRuler.top == rcRuler.bottom) // horizontal ruler
		return ptBody.x - ptRulerOrg.x;
	else // Tilted ruler
	{
		// Calc projection of a body position vector (from ruler
		// origin to body loc) onto the ruler line. 
		CSize vecPos = ptBody - ptRulerOrg;		// component form rep in cx, cy
		CSize vecRuler = ptRulerEnd - ptRulerOrg; // ditto (only slope matters)
		double dirRuler = atan2(vecRuler.cy, vecRuler.cx);
		double dirPos = atan2(vecPos.cy, vecPos.cx);
		double dirDiff = dirPos - dirRuler;
		double magPos = sqrt(vecPos.cx * vecPos.cx + vecPos.cy * vecPos.cy);
		
		return Round(magPos * cos(dirDiff));
	}
}

void CMotionBody::CheckObject()
{
	ASSERT(m_pDiagram != NULL);
	int nPos = GetRulerPos();

	// (lookup-md-position id-md time position id)
	// changed to take zero-based time number
	LPCTSTR pszResult = 
		HelpSystemExecf("(lookup-md-position %s %d %d %s)",
		     m_pDiagram->m_strId, m_nNumber - 1, nPos, m_strId);
	ApplyStatus(pszResult);
}

// Must use a different helpsys API for motion diagrams.
// Can't have a common base class for these with single inheritance if want to
// inherit from CVector for vector entries.
void CMotionBody::NotifyDelete(CString& strOldName)
{
	// Default API is delete-object, must override for md objects
	// Note body label has spaces if interval specified (e.g. "3 secs"); 
	// (Label no longer used by helpsys, just id, but must pass legal lisp call.
	(void) HelpSystemSendf("(delete-md-object |%s| %s)", STR2ARG(strOldName), STR2ARG(m_strId));
}


//
// MDVector -- type for vector contained in a motion diagram
// used both for 1D and 2D diagram vector elements
// 
IMPLEMENT_SERIAL(CMDVector, CVector, VERSIONABLE_SCHEMA | 1);
CMDVector::CMDVector()			// default ctor used only by serialization on loading
	: CVector()
{ 
	// m_nVectorType = VECTOR_VELOCITY;
	
	// ptrs not currently initted from serialize:
	m_pDiagram = NULL;
	m_p2DDiagram = NULL;
}					

CMDVector::CMDVector(const CRect& position)
	:CVector(position)
{	
	// m_nVectorType = VECTOR_VELOCITY; // initial default
	
	m_pDiagram = NULL;
	m_p2DDiagram = NULL;
}

CMDVector::~CMDVector()
{
	// if contained in motion diagram, notify parent we have gone away so 
	// it doesn't hold dangling ref.
	if (m_pDiagram != NULL)
		m_pDiagram->RemoveVector(this);
	if (m_p2DDiagram != NULL)
		m_p2DDiagram->RemoveVector(this);
}

void CMDVector::Serialize(CArchive& ar)
{
	// If loading: save archived class version over possible call to SerializeClass for base
	UINT nVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0 /* unused */;

	// load/store the base class data prepending class info w/its version no.
	// (Don't need to check if Docversion >= 1 since base is not CDrawObj).
	ar.SerializeClass(RUNTIME_CLASS(CVector));
	CVector::Serialize(ar);

	if (ar.IsStoring())		// store object specific data
	{
		// add back pointers for vecs contained in motion diagrams
		ar << m_pDiagram;
		ar << m_p2DDiagram;
	} 
	else // load object specific data
	{
		ar >> m_pDiagram;
		ar >> m_p2DDiagram;
	}
}

void CMDVector::CheckObject()
{
	if (! theApp.CheckDiagram()) return;

	LPCTSTR pszResult;
	if (m_nVectorType == VECTOR_VELOCITY ||
		m_nVectorType == VECTOR_ACCELERATION) 
		pszResult = CheckMoveVector();
	else {
		TRACE("MDVector::CheckObject -- bad vector type (%d)\n", m_nVectorType);
		return;
	}
	ApplyStatus(pszResult);
}

void CMDVector::NotifyDelete(CString & strOldName)
{
	if (m_pDiagram) {	// contained in 1D motion diagram, needs special call
		(void) HelpSystemSendf("(delete-md-object %s %s)", 
		                          STR2ARG(strOldName), STR2ARG(m_strId));
	} 
}

LPCTSTR CMDVector::CheckMoveVector() // do the check with the help system
{
	// use md-* calls for vecs in 1D motion diagram
	if (m_pDiagram && m_nVectorType == VECTOR_VELOCITY)	
	{
		//(lookup-md-velocity md-id label timept mag dir id)
		return HelpSystemExecf("(lookup-md-velocity %s %s %d %d %d %s)",
			m_pDiagram->m_strId,				// diagram id
			STR2ARG(m_strName),					// vector label
			m_pDiagram->GetVectorTimeNum(this) - 1,	// timept, adjusted to zero-based
			GetMagnitude(),						// mag
			GetDirection(),						// dir
			m_strId								// id
		);
	}
	else if (m_pDiagram && m_nVectorType == VECTOR_ACCELERATION)
	{
		// lookup-md-accel md-id label timept mag dir id
		return HelpSystemExecf("(lookup-md-accel %s %s %d %d %d %s)",
			m_pDiagram->m_strId,				// diagram id
			STR2ARG(m_strName),					// vector label
			/*	was GetAccelNum; now changed to 0-based time point, -1 for whole */
			m_pDiagram->GetVectorTimeNum(this) - 1, //timept, adjusted to zero-based
			GetMagnitude(),						// mag
			GetDirection(),						// dir
			m_strId   );						// id
	}
	else if (m_p2DDiagram && m_nVectorType == VECTOR_VELOCITY) // drawn on 2D diagram
	{
		return Check2DEntry();
	}
	return NULL;
}

CDrawObj* CMDVector::Clone()
{
	// create duplicate w/same properties. Unattached to any document
   	CMDVector* pClone = new CMDVector(m_position);
	
	// Copy checked drawobj props:
	pClone->m_strId = m_strId;	// Initially, use same id, if added to document, 
								// different id will be generated
	pClone->m_strName = m_strName;
   	pClone->m_status = m_status;

	// Copy all vector props to be safe. Not all used for MD vectors
   	pClone->m_nVectorType = m_nVectorType;
   	pClone->m_strForceType = m_strForceType;
   	pClone->m_strBody = m_strBody;
   	pClone->m_strAgent = m_strAgent;
   	pClone->m_strCompOf = m_strCompOf;
   	pClone->m_strCompDir = m_strCompDir;
	pClone->m_strOrientation = m_strOrientation;
	pClone->m_strTime = m_strTime;

	// Copy MD-specific props:
	pClone->m_pDiagram = m_pDiagram;
	pClone->m_p2DDiagram = m_p2DDiagram;
    
   	return (pClone);
}

CString CMDVector::GetLabelPrefix()
{
	if (m_nVectorType == VECTOR_ACCELERATION)
		return "A";
	else if (m_nVectorType == VECTOR_VELOCITY)
		return "V";
	return "";
}


/////////////////////////////////////////////////////////////////////////////////
//
// 2DMotion -- merged diagrams showing result of two existing 1D plots
//
/////////////////////////////////////////////////////////////////////////////////

// 2D motion diagram is a box into which the system copies bodies and component
// vectors from component diagrams (horizontal and vertical). These become an unselectable
// "background". The box itself starts
// out parallel to the component diagrams but may be moved anywhere; thus positions
// must be computed relative to box origin. The motion diagram therefore depends on
// its component diagrams, and changes to the components must be propagated to the
// resultant. To ensure that, we always fetch our info about components out of
// component diagrams. We "parent" the component diagrams to the 2D diagram so that they
// will call the appropriate methods on changes, especially deletions.
//
// The student's job is only to fill in resultant vectors correctly, which we must check.
IMPLEMENT_SERIAL(C2DMotion, CCheckedObj, VERSIONABLE_SCHEMA | 1);
C2DMotion::C2DMotion() 				// used by serialization
	:CCheckedObj()
{
	m_pmdX = NULL;
	m_pmdY = NULL;
}

// Clean up on destroy
C2DMotion::~C2DMotion()	
{
	// all our contained children now orphaned: destroy them 
	
	// Delete bodies in our cached list. (these not part of document list)
	while (!m_entries.IsEmpty())
		m_entries.RemoveHead()->Delete();

	// delete the student drawn resultant vectors:
	while (! m_resultants.IsEmpty()) {
		CMDVector* pVec = m_resultants.RemoveHead();
		pVec->m_p2DDiagram = NULL;
		m_pDocument->Remove(pVec);
		pVec->Delete();
	}
	
	// Update x and y diagrams since we are no longer dependent on them.
	if (m_pmdX)
		m_pmdX->m_p2DParent = NULL;
	if (m_pmdY)
		m_pmdY->m_p2DParent = NULL;
}

void C2DMotion::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	
	// document versions >= 1 added serialization of base class descriptor
	if (nDocVersion >= 1) {
		ar.SerializeClass(RUNTIME_CLASS(CDrawObj));
	}
	CDrawObj::Serialize(ar);

	if (ar.IsStoring()) {	// store object-specific members
		ar << m_pmdX;
		ar << m_pmdY;
	}
	else // load object specific members
	{
		ar >> m_pmdX;
		ar >> m_pmdY;
	}
	// load/store embedded object members: element lists
	m_entries.Serialize(ar);
	m_resultants.Serialize(ar);
}

// Construct combined diagram given two existing diagrams
C2DMotion::C2DMotion(CMotionDiagram* pmdX, CMotionDiagram* pmdY)
	:CCheckedObj()
{
	m_pmdX = pmdX;
	m_pmdY = pmdY;
	// update component diagrams to indicate they have been combined
	pmdX->m_p2DParent = this;
	pmdY->m_p2DParent = this;
	// deleting component diagrams will destroy us, so we should never
	// exist with NULL component diagrams. 

	// Layout is a problem here. We would like the combined diagram's X and Y
	// to match those of the components. But we also want its border outside existing 
	// diagrams' respective areas. However, there is no requirement that the
	// existing diagrams are layed out in any special way at all with respect to 
	// each other. For now, just use existing diagram's position as endpoints. 
	// We can allow student to move the combined diagram too, treating x and y
	// position within diagram as relative to boundary.

	CRect rc2D(pmdX->m_position.left, pmdY->m_position.top,
			    pmdX->m_position.right, pmdY->m_position.bottom);
	rc2D.NormalizeRect();
	// Add a small border to allow for bodies at the ends. (what about vectors?)
	rc2D.InflateRect(CSize(MARGIN2D, MARGIN2D)); 
	// Note must take border into acount when positioning elements!
	m_position = rc2D; 

	// could create bunch of drawing objects from component info and add them into
	// this diagram -- might make some tests easier. Or could just treat the component
	// diagrams as our representation and derive whatever info we need from them. That
	// way, don't have to worry about updating on changes to components. 
	// For now we create the time points since they are handy, but derive the components 
	UpdateBodies();	
}

// Get position of center of ith body in diagram, i = zero-based index
// returns TRUE for success, FALSE if none.
BOOL C2DMotion::GetBodyPos(int i, CPoint& ptBody)
{
	// ASSERT(i >= 0 && i < CMotionDiagram::MAX_TIMES);

	CMotionBody* pBodyX = m_pmdX->GetBody(i);
	CMotionBody* pBodyY = m_pmdY->GetBody(i);
	if (! pBodyX || ! pBodyY) 
		return FALSE;

	int xPos = pBodyX->GetRulerPos();	// from left of horizontal ruler
	int yPos = pBodyY->GetRulerPos();	// from top of vertical ruler
	ptBody = CPoint(m_position.left + xPos, m_position.top + yPos); // center of body
	ptBody += CSize(MARGIN2D, MARGIN2D);	// allow for margin we added to 2D box
	return TRUE;
}

// Generate current list of bodies from component diagrams, caching result into m_entries;
void C2DMotion::UpdateBodies()
{
	// empty list of existing entries, since we are updating it
	while (!m_entries.IsEmpty())
		delete m_entries.RemoveHead();

	for (int i = 0; i < CMotionDiagram::MAX_TIMES; ++i) 
	{
		// Create Motion body at given position. Note this obj dangles outside of
		// document's list. !!!risky, since we have not been consistent in dealing with
		// dangling drawobjs. Some drawobj code doesn't test m_pDocument before using 
		// (hack in StatusColor, e.g); also, not clear if all default constructors properly 
		// use base class to init it to NULL).
		CPoint ptBody;
		if (! GetBodyPos(i, ptBody)) 
			continue;						// components might not exist
		
		const int r = CMotionBody::RADIUS; // shorthand
		CMotionBody* pBody = new CMotionBody(CRect(ptBody.x-r, ptBody.y-r, ptBody.x+r, ptBody.y+r));
		
		// setting TEACHER_OBJECT flag means generated objects can't be selected/changed
		pBody->m_flag = TEACHER_OBJECT;

		// can just take body label from one of the component diagrams
		pBody->m_strName = m_pmdX->GetBody(i)->m_strName;
		
		// Save ordinal (1-based) number of body. (not used, but for completeness)
		pBody->m_nNumber = m_entries.GetCount() + 1;

		// And add into diagram's list 
		m_entries.AddTail(pBody);
		// pBody->m_pDiagram = (CMotionDiagram*) this; // urgh, need common base class to do
	}
}

void C2DMotion::Draw(CDC* pDC)
{
	// draw border around the area
	pDC->SelectStockObject(NULL_BRUSH);
	pDC->Rectangle(m_position);		// assumes default 1-pixel solid pen

	// draw the generated component objects as "background"
	// First, the Time points:
	UpdateBodies();				// ensures we are in synch with changes to component diagrams
	POSITION pos = m_entries.GetHeadPosition();
	while (pos) {
		CMotionBody* pBody = m_entries.GetNext(pos);
		pBody->Draw(pDC);
	}
	
	// Then the component vectors. Derive from component diagrams
	CVector vec(CRect(0, 0, 0, 0));		// temp vector object used for drawing
	vec.m_nVectorType = VECTOR_VELOCITY;
	vec.m_pDocument = NULL;
	for (int i = 0; i < CMotionDiagram::MAX_TIMES; ++i) 
	{
		CPoint ptBody;
		if (! GetBodyPos(i, ptBody)) 
			continue;						// components might not exist

		// position vec as X component on diagram
		CMDVector* pCompX = m_pmdX->GetVectorAt(i + 1, VECTOR_VELOCITY); // !urgh, need ordinal
		if (pCompX) {
			vec.m_position = CRect(ptBody.x, ptBody.y, ptBody.x + pCompX->m_position.Width(), ptBody.y);
			// vec.m_strName = pCompX->m_strName + "_x";
			vec.Draw(pDC);
		}

		// position vec as Y component on diagram
		CMDVector* pCompY = m_pmdY->GetVectorAt(i + 1, VECTOR_VELOCITY);
		if (pCompY) {
			vec.m_position = CRect(ptBody.x, ptBody.y, ptBody.x, ptBody.y + pCompY->m_position.Height());
			// vec.m_strName = pCompY->m_strName + "_y";
			vec.Draw(pDC);
		}
	}


	// Finally any student-added resultant vectors
	pos = m_resultants.GetHeadPosition();
	while (pos) {
		CMDVector* pVector = m_resultants.GetNext(pos);
		pVector->Draw(pDC);
	}
}

void C2DMotion::DrawSelectState(CDC* pDC, TrackerState state)
{
	// Override to give some feedback on selection, since we have no resize handles
	// !!! put check for CanResize() or no resize handles into default???
	CPen penBorder(PS_INSIDEFRAME, 3, RGB(0, 0, 0));
	CPen* pOldPen = pDC->SelectObject(&penBorder);
	pDC->SelectStockObject(NULL_BRUSH);
	pDC->Rectangle(m_position);	
	pDC->SelectObject(pOldPen);

}

// return body at a given position in the diagram
CMotionBody* C2DMotion::BodyAt(CPoint local)
{
	// UpdateBodies();			// ensure we are in sync /* not needed since update on redraw */
	POSITION pos = m_entries.GetTailPosition();
	while (pos) {
		CMotionBody* pBody = m_entries.GetPrev(pos);
		if (pBody->m_position.PtInRect(local))		// cheap test on bounding rect
			return pBody;
	}
	return NULL;
}

// add vector to the diagram on body. Should be a resultant velocity drawn on a body
// (UI should ensure this)
void C2DMotion::AddVector(CMDVector* pVector, CMotionBody* pBody)
{
	// add and set back pointer in vector
	m_resultants.AddTail(pVector);
	pVector->m_p2DDiagram = this;
	// associate with body by setting m_strBody field to time point name
	pVector->m_strBody = pBody->m_strName;

	// Generate a label for it from time point number
	// taking interval size from X subdiagram. 
	// !!! ought to check that both subdiagrams are consistent in interval type
	ASSERT(m_pmdX != NULL);
	int nUnits = (m_pmdX->m_nIntervalType == CMotionDiagram::Default) ? 
					1 : m_pmdX->m_nInterval;
	pVector->m_strName.Format("V%d", nUnits * (pBody->m_nNumber - 1));
	
	if (m_pDocument) m_pDocument->SetModifiedFlag();	//  doc now changed
}

// remove a vector from the diagram (should be student-drawn resultant)
void C2DMotion::RemoveVector(CMDVector* pVector)
{
	// Unlink from our list and clear back pointer
	POSITION pos = m_resultants.Find(pVector);
	if (pos != NULL) {
		m_resultants.RemoveAt(pos);
		pVector->m_p2DDiagram = NULL;
	}
	
	if (m_pDocument) m_pDocument->SetModifiedFlag();	// doc now changed
}

// Get a body entry for a given drawn resultant vector. Note return value is a kind of 
// temporary -- it comes from the cached latest result in m_entries -- so pointer should 
// not be stored. Really only used to get the time point number on the vector out of the body. 
CMotionBody* C2DMotion::GetVectorBody(CMDVector * pVector)
{
	// search body list for match of name to pVector->m_strBody
	POSITION pos = m_entries.GetHeadPosition();
	while (pos) {
		CMotionBody* pBody = m_entries.GetNext(pos);
		if (pBody->m_strName == pVector->m_strBody)
			return pBody;
	}
	return NULL;
}

// Fetch resultant drawn at given time point number (1-based), NULL if none
// Note it need not be nth in our list.
CMDVector* C2DMotion::GetResultantAt(int nTime)
{
	POSITION pos = m_resultants.GetHeadPosition();
	while (pos) {
		CMDVector* pVec = m_resultants.GetNext(pos);
		CMotionBody* pBody = GetVectorBody(pVec);
		if (pBody && pBody->m_nNumber == nTime)
			return pVec;
	}
	return NULL;
}

// Update sent after deletion of a vector from one of our component diagrams.
void C2DMotion::OnRemoveComponent(CMDVector* pComp)
{
	// We only care about velocities
	if (pComp->m_nVectorType != VECTOR_VELOCITY || 
		! pComp->m_pDiagram /* not in motion diagram */ )
		return;
	
	// delete any student-added resultants that depended on this component diagram vector
	// first need the component's time point number from the vector's MD
	CMotionBody* pBody = pComp->m_pDiagram->GetVectorBody(pComp);
	if (! pBody) 
		return;
	int nTime = pBody->m_nNumber;
	ASSERT(1 <= nTime && nTime <= CMotionDiagram::MAX_TIMES);

	// see if we've got a resultant drawn at that number. 
	CMDVector* pResultant = GetResultantAt(nTime);
	if (! pResultant)
		return;

	// remove it from diagram and delete it from document
	RemoveVector(pResultant);
	m_pDocument->Remove(pResultant);
	pResultant->Delete();
}

void C2DMotion::MoveTo(const CRect& position, CFBDView* pView)
{
	if (position == m_position)
		return;

	// calc offset of move
	CPoint delta = (CPoint) (position.TopLeft() - m_position.TopLeft());

	if (pView == NULL)
		Invalidate();
	else
		pView->InvalObjInView(this);
		
	// Move the diagram box
	m_position = position;

	// move all the bodies contained in diagram
	POSITION pos = m_entries.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = m_entries.GetNext(pos);
		ASSERT(pObj != NULL);
		CRect position = pObj->m_position + delta;
		pObj->MoveTo(position, pView);
	}

	// move all the vectors contained in diagram
	pos = m_resultants.GetHeadPosition();
	while (pos != NULL) {
		CDrawObj* pObj = m_resultants.GetNext(pos);
		ASSERT(pObj != NULL);
		CRect position = pObj->m_position + delta;
		pObj->MoveTo(position, pView);
	}

	// Update the new area
	if (pView == NULL)
		Invalidate();
	else
		pView->InvalObjInView(this);

	if (m_pDocument) m_pDocument->SetModifiedFlag();
}


// CMDVector method to check resultant vector drawn on 2D motion diagram.
// This is anomalous in that the Workbench does the check itself, without using
// the help system. We return "T" or "NIL" just like help system.
static const char szNIL[] = "NIL";
static const char szT[] = "T";

LPCTSTR CMDVector::Check2DEntry()
{
	C2DMotion* p2D = m_p2DDiagram;
	if (! p2D )					// shouldn't happen, mark as error
		return szNIL;

	// Need to get resultant's 1-based body number from body its drawn on
	CMotionBody* pBody = p2D->GetVectorBody(this);
	if (! pBody)
		return szNIL;
	int nTimeNum = pBody->m_nNumber;	// !! Num means 1-based ordinal number
	
	// Possible problem in calculating is that vector tail might be drawn anywhere on body 
	// dot (which has a significant radius). So we first check that its tail is inside the dot
	// and then check that offsets from endpoints to dot center matches those of components 
	// within a certain tolerance.
	// !!! might be good to anchor vector endpoint to dot center
	// !!! or else must build this into GetMagnitude function for motion diagram vectors,
	// since it affects what's sent to help system.
	// !! calculation actually takes components projection onto ruler, assumes not drawn
	// at wrong angle

	//  Require resultant's tail to be drawn on body
	if (! pBody->m_position.PtInRect(m_position.TopLeft()) ) // cheap test on rect, not dot
		return szNIL;

	// Check resultant's endpoints against those of drawn components, 
	// allowing a small threshhold. 

	// need to get 2D body's center. Note 2D diagram may have moved on screen, so
	// not aligned with component diagrams.
	CPoint ptBody;
	if (! p2D->GetBodyPos(nTimeNum - 1 /* takes index! */, ptBody) )
		return szNIL;
	
	// fetch the components from their respective diagrams
	CMDVector* pCompX = p2D->m_pmdX->GetVectorAt(nTimeNum, VECTOR_VELOCITY);
	CMDVector* pCompY = p2D->m_pmdY->GetVectorAt(nTimeNum, VECTOR_VELOCITY);
	if (! pCompX || ! pCompY ) // shouldn't happen, mark as error
		return szNIL;
	
	int dxRes = m_position.right - ptBody.x;	// xComp of drawn resultant
	int dxComp = pCompX->m_position.Width();	// length of X component !! use body center
	int dyRes = m_position.bottom - ptBody.y;	// yComp of drawn resultant
	int dyComp = pCompY->m_position.Height();	// lenght of Y component !! use body center 
	const int epsilon = 12; // visible grid line = 2 * (nLUsPerInch/nGridPerInch)) 
	// !!! might want to ensure direction is correct too
	if (abs(dxRes - dxComp) >= epsilon || abs(dyRes - dyComp) >= epsilon)
		return szNIL;
	// !!!  should record error to handle possible help what's wrong request !

	return szT;		// reached here => no error
}











