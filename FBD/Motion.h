///////////////////////////////////////////////////////////////////////////////////////
// 
// Motion Diagram Drawing Object definitions
//
///////////////////////////////////////////////////////////////////////////////////////
#ifndef MOTION_INCLUDED
#define MOTION_INCLUDED 1	// protect against multiple inclusion

#include "FBDObj.h"			// CVector required for template lists

// forward class declarations: 
class CMotionDiagram;	// a 1D motion diagram line	
class C2DMotion;		// Merged two-D diagram if we become part of it.

//
// MotionBody -- system-like entry representing position of body at time in a motion diagram
//
class CMotionBody: public CCheckedObj
{
protected:
	DECLARE_SERIAL(CMotionBody);
	CMotionBody();
	~CMotionBody();
public:
	CMotionBody(const CRect& position);

// Attributes:
	CMotionDiagram* m_pDiagram;	// back pointer to containing diagram
	int m_nNumber;				// Time point's 1-based ordinal number, for easy access
	int GetRulerPos();			// calc pos on ruler from left or top if vert

	enum { RADIUS = 10 } ;		// radius of body dot in logical units
// Operations:
	void Serialize(CArchive& ar);
	virtual void Draw(CDC* pDC);
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
	virtual BOOL CanEditProperties() { return FALSE; }
	virtual BOOL CanDuplicate() 
		{ return FALSE; /* m_pDiagram && m_pDiagram->m_entries.GetCount() < 4; */ }
	virtual CDrawObj* Clone();	// copies to next time point
	virtual void NotifyDelete(CString& strOldName);
	virtual void CheckObject();
// Implementation:
#ifdef _DEBUG
public:
	
	/* virtual void AssertValid() const; */
	virtual void Dump(CDumpContext& dc) const;
#endif

};

// MDVector: subclass of Vector for use in MotionDiagrams
class CMDVector: public CVector
{
// Constructors:
protected:
	DECLARE_SERIAL(CMDVector);
	CMDVector();
	~CMDVector();
public:
	CMDVector(const CRect& position);
	void Serialize(CArchive& ar);

// Attributes:
	CMotionDiagram* m_pDiagram;	// back pointer to containing 1D diagram
	C2DMotion* m_p2DDiagram;	// back pointer to containing 2D diagram

	// Operations:
	virtual CDrawObj* Clone(); 
	virtual void CheckObject();
	virtual void NotifyDelete(CString& strOldName);
	virtual BOOL CanDelete() { return FALSE; };

	// Furnish read-only label prefix to dialog base class
	virtual CString GetLabelPrefix();

#ifdef _DEBUG
public:
	
	/* virtual void AssertValid() const; */
	/* virtual void Dump(CDumpContext& dc) const; */
#endif
protected:
	LPCTSTR CheckMoveVector();
	LPCTSTR Check2DEntry();
};


//
// Motion Diagrams (aka "Rulers") -- grid line along which motion is sketched
//
// A motion diagram is, in effect, a container for several other objects. But we
// have not implemented a general notion of container objects (although we ought to).
// Until then, Motion Diagrams contain lists referencing objects that also exist in 
// the document's main object list:
//	  The DocArea object that surrounds the drawn ruler,
//	  A list of MotionBody objects representing the individual time point entries.
//	  A list of Vectors entered on the diagram.
// When added to a motion diagram's list, the entered objects will have back pointers
// set to the referencing diagram. They thus need to update the diagram if they are deleted.
//
class CDocArea;			// defines page area associated with this diagram

class CMotionDiagram: public CCheckedObj
{
protected:
	DECLARE_SERIAL(CMotionDiagram);
	CMotionDiagram();
public:
	CMotionDiagram(const CRect& position);
	~CMotionDiagram();
	virtual void Serialize(CArchive& ar);

	CDocArea* m_pArea;		// document area marker for this diagram
	void CreateArea();		// creates an area around drawn ruler
	BOOL m_bOwnArea;		// T = We created area (i.e. not pre-defined by teacher)
	void CalcDiagramArea(CRect& rcArea);

	CString m_strBody;		// Problem object whose motion is being plotted
	enum IntervalType {		// How our time points should be labelled
		Default,			//	unspecifed equal intervals = T0, T1, T2
		Measured			//	Specified number of units.
	} m_nIntervalType;		//		In case of measured intervals:	
	UINT m_nInterval;		// Number of units per interval
	CString m_strUnits;		// Units to use

	void GetNthTimeLabel(int nCount, CString& strLabel);
	void GetNextTimeLabel(CString& strLabel);
	
	CTypedPtrList<CObList, CMotionBody*> m_entries;	// List of time pts = MotionBody's entered on diagram.
	CTypedPtrList<CObList, CMDVector*> m_vectors;	// List of Vectors drawn in diagram.
	enum { MAX_TIMES = 4 };	// Maximum number of entries allowed

	C2DMotion* m_p2DParent;  // Possible merged diagram of which we have become part.

	void AddBody(CMotionBody* pBody);
	void RemoveBody(CMotionBody* pBody);
	BOOL CanDelete(CMotionBody* pBody);
	CMotionBody* FindBody(CString& strName);	// find body by name
	CMotionBody* GetBody(int nTimeNum);			// find body by 1-based time point num 
	void UpdateBodies();
	// fill combo box w legal time pt choices for given vector type: 
	void BodiesToCbo(CComboBox& cbo, int nVectorType = VECTOR_ACCELERATION);	
	static const char* szWholePeriod;	// string used for whole period choice

	void AddVector(CMDVector* pVector, CMotionBody* pBody);
	void RemoveVector(CMDVector* pVector);
	void AddClone(CMDVector* pClone);
	BOOL CanDelete(CMDVector* pVec);
	BOOL CanAddVector(CMDVector* pVec);
	CMDVector* GetVectorAt(int nTimePoint, int nType);
	CMotionBody* GetVectorBody(CMDVector* pVector);	
	int GetVectorTimeNum(CMDVector* pVec);	// get 1-based time pt num of vector
	int GetAccelNum(CMDVector* pVector);
	
	BOOL IsComplete();
	
	virtual void Draw(CDC* pDC);
	virtual BOOL CanEditProperties() { return TRUE; }
	virtual BOOL OnEditProperties();
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
	virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView);
	virtual void MoveTo(const CRect& position, CFBDView* pView);
	virtual HCURSOR GetHandleCursor(int nHandle);
	virtual CPoint GetHandle(int nHandle);
	virtual int GetHandleCount();
	virtual void NotifyDelete(CString& strOldName);
	virtual void CheckObject();
};

//
// C2DMotion -- Two dimensional motion diagram showing resultants obtained by
//              merging two existing one-dimensional motion plots.
//
// Time points and vectors are generated from original diagrams by system,
// student task is to sketch resultant velocities only.
// 
// ???should this be subclass of MotionDiagram? Looks like not much commonality
// ???Should we create objects for derived elements or just draw from original diagrams?
class C2DMotion : public CCheckedObj
{
// Construction
protected:
	DECLARE_SERIAL(C2DMotion);
	C2DMotion();
public:

	C2DMotion(CMotionDiagram* pmdX, CMotionDiagram* pmdY);
	~C2DMotion();
	virtual void Serialize(CArchive& ar);

// Attributes
	CMotionDiagram* m_pmdX;	// existing diagram for horizontal (X) component
	CMotionDiagram* m_pmdY;	// existing diagram for vertical (Y) component

	CTypedPtrList<CObList, CMotionBody*> m_entries;	// Generated list of time points
	CTypedPtrList<CObList, CMDVector*> m_resultants;	// List of student-added resultant velocities

	enum { MARGIN2D = 24} ;	// width of border region, LUs

	BOOL GetBodyPos(int i, CPoint& ptBody);
	void UpdateBodies();
	CMotionBody* BodyAt(CPoint local);
	void AddVector(CMDVector* pVector, CMotionBody* pBody);
	void RemoveVector(CMDVector* pVector);
	CMotionBody* GetVectorBody(CMDVector* pVector);
	CMDVector* GetResultantAt(int nTime);
	void OnRemoveComponent(CMDVector* pVector);

// Operations:	
	virtual void Draw(CDC* pDC);
	virtual void DrawSelectState(CDC* pDC, TrackerState state);
	virtual int GetHandleCount() { return 0; } // can't resize this.
	virtual void MoveTo(const CRect& position, CFBDView* pView);
};


#endif MOTION_INCLUDED