///////////////////////////////////////////////////////////////////////////////////
// 
// Free body diagram objects:  System, Vector and Axes
//
///////////////////////////////////////////////////////////////////////////////////
#ifndef FBDOBJ_INCLUDED
#define FBDOBJ_INCLUDED 1   // prevent multiple inclusion

//
// Vectors:
//
class CMotionDiagram;		// forward ref for contained vector field
class C2DMotion;

class CVector: public CCheckedObj
{
// Constructors:
protected:
	DECLARE_SERIAL(CVector);
	CVector();
	~CVector();
public:
	CVector(const CRect& position);
	void Serialize(CArchive& ar);

// Attributes:
	//
	// Vector properties: set from dialog box. Empty string means unset.
	//
	int		m_nVectorType;		// type of vector:
	
	// NB: order may have to parallel ID_DRAWVECTOR_* command IDs and/or 
	// CFBDView vector drawing tool enums -- should be centralized.
#define VECTYPE_FIRST		0
#define VECTOR_FORCE		(ID_DRAWVECTOR_FORCE - ID_DRAWVECTOR_FIRST)
#define VECTOR_VELOCITY		(ID_DRAWVECTOR_VELOCITY - ID_DRAWVECTOR_FIRST)
#define VECTOR_ACCELERATION (ID_DRAWVECTOR_ACCELERATION - ID_DRAWVECTOR_FIRST)
#define VECTOR_COMPONENT    (ID_DRAWVECTOR_COMPONENT - ID_DRAWVECTOR_FIRST)
#define VECTOR_DISPLACEMENT (ID_DRAWVECTOR_DISPLACEMENT - ID_DRAWVECTOR_FIRST)
#define VECTOR_MOMENTUM		(ID_DRAWVECTOR_MOMENTUM - ID_DRAWVECTOR_FIRST)
#define VECTOR_POSITION		(ID_DRAWVECTOR_RELPOS - ID_DRAWVECTOR_FIRST)
#define VECTOR_TORQUE		(ID_DRAWVECTOR_TORQUE - ID_DRAWVECTOR_FIRST)
#define VECTOR_RELVEL       (ID_DRAWVECTOR_RELVEL - ID_DRAWVECTOR_FIRST)
#define VECTOR_EFIELD       (ID_DRAWVECTOR_EFIELD - ID_DRAWVECTOR_FIRST)
#define VECTOR_BFIELD       (ID_DRAWVECTOR_BFIELD - ID_DRAWVECTOR_FIRST)
#define VECTOR_IMPULSE      (ID_DRAWVECTOR_IMPULSE - ID_DRAWVECTOR_FIRST)
#define VECTOR_UNITVECTOR   (ID_DRAWVECTOR_UNITVECTOR - ID_DRAWVECTOR_FIRST)
#define VECTOR_MAGDIPOLE    (ID_DRAWVECTOR_MAGDIPOLE - ID_DRAWVECTOR_FIRST)
#define VECTOR_ELECDIPOLE   (ID_DRAWVECTOR_ELECDIPOLE - ID_DRAWVECTOR_FIRST) 
#define VECTYPE_LAST		(ID_DRAWVECTOR_LAST - ID_DRAWVECTOR_FIRST)
	BOOL	m_bAngular;			// means type is angular version of specified vec

	CString m_strForceType;		// FORCE: type string
	CString m_strAgent;			// FORCE: agent of force
	CString m_strBody;			// FORCE|MOTION: what vector is on/of
	CString m_strCompOf;		// COMPONENT: name of principal vector
	CString m_strCompDir;		// COMPONENT: name of axis of projection
	CString m_strOrientation;	// FORCE|MOTION: user-specified direction (as string)
	CString m_strTime;			// FORCE|MOTION: time for multi-time-pt probs
	
	int		m_nZDir;			// Z-axis direction used for rotational motion vectors
	// NB: Code order should parallel that of ID_ZDIR* commands
#define ZDIR_NONE		0		// not a z-axis vector
#define ZDIR_INTO		1		// points into plane of figure (clockwise by rh rule)
#define ZDIR_OUTOF		2		// points out of plane of figure (ccw by rh rule)
#define ZDIR_UNKNOWN	3		// is Z-Axis vec but which dir is unknown.
#define ZDIR_MAX		3		// max legal value for range checking
	BOOL IsZAxisVector() { return m_nZDir != ZDIR_NONE; } 
	enum { ZVEC_RADIUS = 8, };	// radius of circle drawn for vector into/outof plane
	void SetZDir(int nZDir);	// set the ZDir and update
	
	// set from diagram:
	BOOL m_bDecomposed;			// FORCE|MOTION: marked as resolved into components

	// Helper to derive:
	int GetDrawnMagnitude();			// Drawn magnitude in logical units
	BOOL IsZeroMag();			// true if this is NULL (zero-mag) vector

	// Gets dir told by user in dialog; returns FALSE if unknown, else T.
	// Differs from GetDirection which always returns the drawn direction,
	// even for entries representing vectors of unknown orientation.
	BOOL GetOrientation(int& nDegrees)
	{
		return sscanf(m_strOrientation, "%d", &nDegrees) == 1;
	};
	BOOL UnknownDir()			// true if represents vec of unknown orientation
	{
		int nTemp;
		return ! GetOrientation(nTemp);
	};

	virtual CString GetDef();
	virtual CString GetPrintDef();
	virtual void GetTypeName(CString& strType);
	virtual CString GetLabelPrefix();

	virtual BOOL HasComponents() {return (m_nVectorType != VECTOR_COMPONENT); }
	virtual BOOL IsComponent() {return (m_nVectorType == VECTOR_COMPONENT); }
	CDrawObjList m_Comps;		//list of drawn vector components	

//	virtual CDrawObj*	GetObjSameLabel();
	virtual BOOL HasSameDef(CDrawObj* pObj);
	virtual BOOL HasSameDef(CVariable* pVar);
	virtual BOOL HasSameDir(CDrawObj* pObj);
	CString GetBodies();
	
	virtual void	UpdateVarNames(CString strOldName = "");
	virtual void	RemoveVarNames(CString strOldName);

// Operations:
	virtual void Draw(CDC* pDC);
	virtual void DrawSelectState(CDC* pDC, TrackerState state);
	
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
	virtual BOOL Intersects(const CRect& rect);
	virtual CRect GetBoundingBox();
	virtual void MoveTo(const CRect& position, CFBDView* pView);
	virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView);
	virtual HCURSOR GetHandleCursor(int nHandle);
	virtual CPoint GetHandle(int nHandle);
	virtual int GetHandleCount();
	
	virtual void Delete();
	virtual CDialog* GetPropertyDlg();
	virtual BOOL CanEditProperties() { return TRUE; }
	virtual BOOL CanDuplicate();
	virtual CDrawObj* Clone();
	// specific to vectors:

	virtual void CheckObject();
	virtual void UpdateObj(CDrawObj* pObj);
	virtual void LogEntry();
	virtual BOOL SetFromLogStr(LPCTSTR pszStr);

	
	virtual BOOL GetLabelRect(CRect& posLabel) { posLabel = m_posLabel; return TRUE; }

	virtual CPoint GetBtnPos(CRect btnPos);//for example study

	// public helper to calculate arrowhead to place on line
	static void CalcArrowPts(CPoint ptFrom, CPoint ptTo, int lineWidth, 
		            /*out:*/ CPoint& ptEndShaft, CPoint arrowhead[3]);
	// public helper to calculate bounding poly of wide line:
	static void CalcLinePts(CPoint ptFrom, CPoint ptTo, int linewidth, 
					/* out: */ CPoint linePts[4]);

	// Agent string used for net field vectors: //
	static const char* c_szAllSources;

	// Implementation:
protected:
	CRect m_posLabel;			// position of vector label
	void PlaceLabel();
	CRgn m_vectorRgn;			// cached GDI region for hit-testing
	
	// helpers for drawing:
	void DrawLabel(CDC* pDC);
	void DrawMark(CDC* pDC, int lineWidth);
	void DrawZAxisVector(CDC* pDC);
	
	// Constants for default object drawing parameters: 
	enum {  
		nVectorWidth = 6,		// default vector line width, Forces
        nComponentWidth = 3,	// line width for Component vectors
	    nVelocityWidth = 2,		// line width for Velocities & accelerations
		nArrowLineWidth = 2,	// width used for drawing arrowhead lines
	};

	// get args for help system:
	CString MagArg();
	CString DirArg();

	// do type-dependent help system calls:
	LPCTSTR CheckCompVector();
	LPCTSTR CheckMoveVector();
	LPCTSTR CheckForceVector();
	LPCTSTR CheckTorqueVector();
	LPCTSTR CheckUnitVector();
	LPCTSTR CheckDipoleVector();
	/* LPCTSTR Check2DEntry(); */

	 
#ifdef _DEBUG
public:
	/* virtual void AssertValid() const; */
	virtual void Dump(CDumpContext& dc) const;
#endif
};

//
// Coordinate Axes
//
#define POSX	2
#define POSY	3
#define	NEGX	4
#define NEGY	5

class CAxes: public CCheckedObj	
{
// Constructors:
protected:
	DECLARE_SERIAL(CAxes);
	CAxes();
public:
	CAxes(const CRect& position);
	void Serialize(CArchive& ar);

// Attributes:
	CString m_strSystem;			// Name of associated system
	int m_nDirection;				// User-specified direction (as integer)
	int m_nAxis;					// Currently selected axis
	int m_nLastHit;					// Most recently "hit" axis
	int m_nIndex;					// 0-based index if multiple axes

// Operations:
	virtual	CDrawObj* Clone();
	virtual BOOL HasSameDir(CDrawObj* pObj);

	virtual void Draw(CDC* pDC);
	virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView);
	virtual void MoveTo(const CRect& position, CFBDView* pView);
	virtual CPoint GetHandle(int nHandle);
	virtual HCURSOR GetHandleCursor(int nHandle);
	virtual int GetHandleCount();
	virtual BOOL Intersects(const CRect& rect);
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
	virtual COLORREF StatusColor();

	virtual void Delete();
	virtual BOOL CanEditProperties() { return TRUE; }
	virtual CDialog* GetPropertyDlg();
	virtual void CheckObject();
	virtual void SetSelectedPart(int hit);
	virtual int GetSelectedPart();
	virtual int GetHit();

	virtual void UpdateObj(CDrawObj* pObj);
	virtual void LogEntry();
	virtual BOOL SetFromLogStr(LPCTSTR pszStr);

	// helper returns top-left for text block placed at directed line endpoint.
	// static so can be used by other objects, e.g. vectors
	static CPoint CalcLabelPos(CPoint ptEnd, int dx, int dy, CSize size);

	virtual BOOL GetLabelRect(CRect& posLabel) { posLabel = m_posLabel; return TRUE; }

	virtual CPoint GetBtnPos(CRect btnPos);//for example study

	CPoint GetAxisPoint();
	CPoint GetAxisPoint(int nAxis);

	CRect m_posLabel;				// position of X axis label

	virtual	void UpdateVarNames(CString strOldName);
	virtual void RemoveVarNames(CString strName);

// Implementation
protected:
	
	CRgn m_axisRgn;					// cached GDI region for hit testing
	CPoint m_negX;
	CPoint m_posX;
	CPoint m_negY;
	CPoint m_posY;
	CRgn   m_posXAxisRgn;
	CRgn   m_negXAxisRgn;
	CRgn   m_posYAxisRgn;
	CRgn   m_negYAxisRgn;

	

	enum {  nAxesWidth = 2 };		// width used for coordinate axes

#ifdef _DEBUG
public:
	/* virtual void AssertValid() const; */
	virtual void Dump(CDumpContext& dc) const;
#endif
};

//
// System objects:
//
class CSystem: public CCheckedObj
{
// Construction:
protected:
	DECLARE_SERIAL(CSystem);
	CSystem();
public:
	CSystem(const CRect& position);
	void Serialize(CArchive& ar);

// Attributes:
	CString m_strTime;				// name of time point represented
	CString m_strBodies;			// list of body names
	int m_nSystemType;				// whether this is compound-body system.

	virtual BOOL CanEditProperties() { return TRUE; }
	virtual BOOL CanDuplicate();

	virtual BOOL IsValid();		// check if newly created object unique
	virtual BOOL HasSameDef(CDrawObj* pObj);
	virtual BOOL HasSameDef(CVariable* pVar);

	virtual CString GetDef();		// used in variable definition
	virtual CString GetPrintDef();	// used when printing the list of objects
	virtual void	GetTypeName(CString& strType);
	virtual BOOL	HasSameName(CDrawObj* pObj);
	virtual void	UpdateObj(CDrawObj* pObj);	// transfer new props into object

	virtual void	UpdateVarNames(CString strOldName = "");
	virtual void	RemoveVarNames(CString strOldName);

	virtual BOOL	GetLabelRect(CRect& posLabel) { posLabel = m_posLabel; return TRUE; }
	virtual CPoint	GetBtnPos(CRect btnPos);//for example study

	virtual CRect		GetBoundingBox();
	virtual CDialog*	GetPropertyDlg();
	virtual void LogEntry();
	virtual BOOL SetFromLogStr(LPCTSTR pszStr);


// Values for m_nSystemType. 
#define SYSTEM_SINGLE_BODY   0
#define SYSTEM_COMPOUND_BODY 1

// Operations:
	virtual void Draw(CDC* pDC);
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
	virtual CDrawObj* Clone();
	virtual void CheckObject();

	
// Implementation:
protected:
	CRect	m_posLabel;				// label position
	CRgn	m_circleRgn;			// cached GDI region for hit testing

#ifdef _DEBUG
public:
	/* virtual void AssertValid() const; */
	virtual void Dump(CDumpContext& dc) const;
#endif

};

// 
// ids used to identify axes parts when defining angles
//
typedef struct { 
    char* strHelp;		// help system id
	char* strDef;		// human-readable choice
} AXESSTRINGS; 

const AXESSTRINGS axes[] =
{ 
	"posx", "positive x-axis",
	"posy", "positive y-axis",
	"negx",	"negative x-axis",
	"negy",	"negative y-axis",
};

const int nAxesStrs ARRAY_SIZE(axes);

class CAngle: public CCheckedObj		
{
protected:
	BOOL CreateAngleRegion(CFBDView* pView);
	CRect m_posLabel;
	CRgn m_angleRgn;
	DECLARE_SERIAL(CAngle);
	CAngle();
public:


	void UpdatePosition();
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);

	void DrawLabel(CDC* pDC);
	int m_nAxis;
	CPoint GetLabelPosition(CSize size);

	void Serialize(CArchive& ar);
	virtual void Delete();
	CRect RotateLine(CPoint intercept, CPoint startPt, CPoint endPt, double ang);

	void MoveTo(const CRect& position, CFBDView* pView);
	void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView);
	HCURSOR GetHandleCursor(int nHandle);
	CPoint GetHandle(int nHandle);
	int GetHandleCount();

	CDrawObj* m_pAngSide2;
	CDrawObj* m_pAngSide1;
	CRect GetAngleRect(CPoint intercept, int radius);
	void  SetAngleMag();
//	virtual BOOL OnEditProperties();

	CString GetDef();
	CString GetPrintDef();
	virtual void CheckObject();

	virtual CDialog* GetPropertyDlg();
	virtual int GetDirection();

	virtual BOOL	HasSameDef(CVariable* pVar);
	virtual BOOL	HasSameDef(CDrawObj* pObj);
	virtual void	UpdateObj(CDrawObj* pObj);	
	virtual CDrawObj*	Clone();//did not override CanDuplicate.  Only using internally.
	virtual void LogEntry();
	virtual BOOL SetFromLogStr(LPCTSTR pszStr);

	// m_degAng is the direction this angle represents, which will be -1 
	// if this represents an unknown angle,. This happens if one 
	// of the drawn sides represents a vector with an unknown orientation.
	int m_degAng;
	BOOL UnknownDir() { return m_degAng == -1; };

	// degDrawn is the drawn direction, for use in graphics calculations
	int m_degDrawn;

	CRect m_arcRect;
	int m_radius;
	int m_maxRadius;
	CPoint m_intercept;
	CAngle(const CRect& position);
	virtual void Draw(CDC* pDC);

};

//
// System objects:
//
class CRadius: public CCheckedObj
{
// Construction:
protected:
	DECLARE_SERIAL(CRadius);
	CRadius();
public:
	CRadius(const CRect& position);
	void Serialize(CArchive& ar);


// Attributes:
	CString m_strBodies;			// list of body names
	CString m_strType;				// "revolution" or "shape"
	CString m_strTime;

// Operations:
	virtual void Draw(CDC* pDC);
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
	virtual CRect GetBoundingBox();
	virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView);
	virtual BOOL CanEditProperties() { return TRUE; }
	virtual CDialog* GetPropertyDlg();
	virtual BOOL CanDuplicate();
	virtual CDrawObj* Clone();

	virtual CString GetDef();
	virtual CString GetPrintDef();
	virtual CString GetLabelPrefix();

	virtual void CheckObject();

	virtual BOOL GetLabelRect(CRect& posLabel) { posLabel = m_posLabel; return TRUE; }

	virtual BOOL HasSameDef(CDrawObj* pObj);
	virtual BOOL HasSameDef(CVariable* pVar);
	virtual void UpdateObj(CDrawObj* pObj);	
	virtual void LogEntry();
	virtual BOOL SetFromLogStr(LPCTSTR pszStr);

	
// Implementation:
protected:
	CRect m_posLabel;				// label position
	CRgn m_circleRgn;				// cached GDI region for hit testing
#ifdef _DEBUG
public:
	/* virtual void AssertValid() const; */
	virtual void Dump(CDumpContext& dc) const;
#endif
};

//
// "Guide line" -- line added to diagram to indicate a direction
//
// Originally intended for use to show vector projection lines or project other significant
// directions, e.g. for angle labelling. Now offered on menu for straight-line 
// trajectory sketching. Our first problems also used this as a graphic object, 
// although that is now unnecessary since we have CDrawRect Line objects. 
//
class CGuideLine: public CCheckedObj
   {
protected:
   	DECLARE_SERIAL(CGuideLine);
  	CGuideLine();
public:
   	CGuideLine(const CRect& position);
   	virtual void Serialize(CArchive& ar);

	// log line serialization/construction
	virtual void GetTypeName(CString & strType);
	virtual void LogEntry();
	virtual BOOL SetFromLogStr(LPCTSTR pszStr);

	// attributes
   	CString m_strBody;
	CString m_strTime;
	virtual CString GetDef();
	//virtual CString GetPrintDef();

	// to act like a vector to direction control
	BOOL IsZeroMag() { return FALSE; }
	BOOL IsZAxisVector() { return FALSE; }
	int m_nZDir;
	CString m_strOrientation;

	// Gets dir told by user in dialog; returns FALSE if unknown, else T.
	// Differs from GetDirection which always returns the drawn direction,
	// even for entries representing vectors of unknown orientation.
	BOOL GetOrientation(int& nDegrees)
	{
		return sscanf(m_strOrientation, "%d", &nDegrees) == 1;
	};
	BOOL UnknownDir()			// true if represents vec of unknown orientation
	{
		int nTemp;
		return ! GetOrientation(nTemp);
	};

	// drawable object protocol:
	virtual void Draw(CDC* pDC);
	virtual int HitTest(CPoint point, CFBDView* pView, BOOL bSelected);
	virtual BOOL Intersects(const CRect& rect);
	virtual CRect GetBoundingBox();
	virtual void MoveHandleTo(int nHandle, CPoint point, CFBDView* pView);
	virtual HCURSOR GetHandleCursor(int nHandle);
	virtual CPoint GetHandle(int nHandle);
	virtual int GetHandleCount();

	// Property editing protocol:
	virtual BOOL CanEditProperties() { return TRUE; }
	virtual CDialog* GetPropertyDlg();
	virtual CDrawObj* Clone();
	virtual void CheckObject();
	virtual void UpdateObj(CDrawObj* pObj);
	
	//implementation helpers
	enum { lineWidth = 2 };
	void DrawLabel(CDC* pDC);
	CRect m_posLabel;
	CRgn m_vectorRgn;			// cached GDI region for hit-testing
	void RotateDirection(int nDeg);
};

#endif FBDOBJ_INCLUDED
