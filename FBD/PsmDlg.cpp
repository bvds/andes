// PsmDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "mainfrm.h"
#include "PsmDlg.h"
#include "GreekOpts.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPsmDlg dialog


CPsmDlg::CPsmDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CPsmDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPsmDlg)
	//}}AFX_DATA_INIT
	m_nSelected = -1;
	// default is for modeless equation review: show all equations, not for selection.
	// must alter one of these before using modally to select.
	m_bPrinciples = FALSE;
	m_bSelect = FALSE;
}


void CPsmDlg::DoDataExchange(CDataExchange* pDX)
{
	CLogDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPsmDlg)
	DDX_Control(pDX, IDC_PROMPT, m_stcPrompt);
	DDX_Control(pDX, IDC_PSM_HELP, m_btnHelp);
	DDX_Control(pDX, IDOK, m_btnOk);
	DDX_Control(pDX, IDCANCEL, m_btCancel);
	DDX_Control(pDX, IDC_PSM_TREE, m_tree);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPsmDlg, CLogDialog)
	//{{AFX_MSG_MAP(CPsmDlg)
	ON_BN_CLICKED(IDC_PSM_HELP, OnPsmHelp)
	ON_NOTIFY(TVN_SELCHANGED, IDC_PSM_TREE, OnSelchangedPsmTree)
	ON_NOTIFY(NM_DBLCLK, IDC_PSM_TREE, OnDblclkPsmTree)
	ON_NOTIFY(TVN_ITEMEXPANDED, IDC_PSM_TREE, OnItemexpandedPsmTree)
	ON_WM_SIZE()
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPsmDlg message handlers

BOOL CPsmDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	
	theApp.GetMainFrame()->MoveDlgToBtmRight(this);

	// TODO: Add extra initialization here
	PopulateTree();

	// DumpTable();

	// Customize for different uses:
	// Default prompt OK for equations.
	if (! m_bSelect)		// just perusing help, not selecting 
	{
		m_btnOk.SetWindowText("Close");
		m_btCancel.ShowWindow(SW_HIDE);
	} 
	else if (m_bPrinciples) // selecting a principle
	{		
		m_stcPrompt.SetWindowText("Select Principle");
	}
	// else selecting an equation.

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

static char* szEndPrinciples = "Write a Principle"; // special text marks end of principles group in array 

// node type codes:
#define LEAF		 0		// leaf item, contains principle
#define GROUP		 1		// group heading, begins group of items.
#define END_GROUP    -1		// dummy entry to mark end of a group of item

/*  Replaced by table in external file
//
// Tree is described by linearized list of itemInfo structs:
//
static const struct itemInfo 
{


	int   nType;			// type of item table entry
	char* szText;			// text to show in tree
	char* szHelpID;			// associated help system id (used for LEAF only).
	char* szName;			// friendly name of principle, now used for deriving help page name only.
} 
treeItems[] = 
{
{GROUP, "Write a Principle"},
 {GROUP, "Kinematics"},
#if !LITE
	{GROUP, "Translational"},
		LEAF, "vavg = s/t                  average speed",        "sdd", "Average speed",
#endif
		// LEAF, "Net displacement", "net-disp", NULL,
		LEAF, "v(avg)_x = d_x/t            average velocity", 	   "(avg-velocity ((?axis . x)))", "Average velocity",
		LEAF, "v(avg)_y = d_y/t            average velocity",     "(avg-velocity ((?axis  .y)))", "Average velocity",
		LEAF, "a(avg)_x = (vf_x - vi_x)/t  average acceleration", "(lk-no-s ((?axis . x)))", "Average acceleration",
		LEAF, "a(avg)_y = (vf_y - vi_y)/t  average acceleration", "(lk-no-s ((?axis . y)))", "Average acceleration",
#if !LITE
		LEAF, "d_x = v0_x*t + 0.5*a_x*t^2 [ax is constant]",       "(lk-no-vf ((?axis . x)))", "Constant acceleration",
		LEAF, "d_y = v0_y*t + 0.5*a_y*t^2 [ay is constant]",       "(lk-no-vf ((?axis . y)))", "Constant acceleration",
		LEAF, "v_x = v0_x + a_x*t         [ax is constant (or time average)]","(lk-no-s ((?axis . x)))", "Constant acceleration",
		LEAF, "v_y = v0_y + a_y*t         [ax is constant (or time average)]", "(lk-no-s ((?axis . y)))", "Constant acceleration",
		LEAF, "v_x^2 = v0_x^2 + 2*a_x*d_x [ax is constant]",       "(lk-no-t ((?axis . x)))", "Constant acceleration",
		LEAF, "v_y^2 = v0_y^2 + 2*a_y*d_y [ay is constant]",       "(lk-no-t ((?axis . y)))", "Constant acceleration",
#endif
		LEAF, "d_x = v_x * t              [vx is constant (ax=0)]","(sdd-constvel ((?axis . x)))", "Constant velocity component",
		LEAF, "d_y = v_y * t              [vy is constant (ay=0)]","(sdd-constvel ((?axis . y)))", "Constant velocity component",
#if !LITE	
		LEAF, "ac = v^2/r                 Centripetal acceleration (instantaneous)", "centripetal-accel", "Centripetal acceleration", 
		LEAF, "Vac_x = Vab_x + Vbc_x      Relative velocity",      "(relative-vel ((?axis . x)))", "Relative Velocity",
		LEAF, "Vac_y = Vab_y + Vbc_y      Relative velocity",      "(relative-vel ((?axis . y)))", "Relative Velocity",
		// LEAF, "Constant acceleration", "lk", NULL,
	{END_GROUP}, // Kinematics/Translational

	{GROUP, "Rotational",},

		LEAF, "$w(avg) = $q/t                  average angular velocity", "ang-sdd", "Angular velocity",
		LEAF, "$a(avg) = ($wf - $wi)/t          average angular acceleration", "rk-no-vf", "Angular acceleration",
		LEAF, "$q_z = $w0_z*t + 0.5*$a_z*t^2   [$a_z is constant (or time average)]", "rk-no-vf", "Constant angular acceleration",
		LEAF, "$w_z = $w0_z + $a_z*t           [$a_z is constant (or time average)]", "rk-no-s", "Constant angular acceleration",
		LEAF, "$w_z^2 = $w0_z^2 + 2*$a_z*$q_z   [$a_z is constant (or time average)]", "rk-no-t", "Constant angular acceleration",
		LEAF,  "v = $w*r                       Linear velocity (instantaneous)", "linear-vel", "Linear velocity",
		// not used in any ANDES problem yet:
		// LEAF,  "ac = $w^2*r					   Linear acceleration (instantaneous)", "linear-accel", "Linear acceleration",
	{END_GROUP},
#endif !LITE
 {END_GROUP}, // Kinematics

#if !LITE
 {GROUP, "Newton's Laws"},
	{GROUP, "Translational"},
		LEAF, "$S F_x = m*a_x      Newton's Second Law", "(NSL ((?axis . x)))", "Newton's Second Law",
		LEAF, "$S F_y = m*a_y      Newton's Second Law", "(NSL ((?axis . y)))", "Newton's Second Law",
		LEAF, "Fof1on2 = Fof2on1   Newton's Third Law (magnitudes)", "NTL", "Newton's Third Law",
		// Following form not yet used in Andes yet:
		LEAF, "F12_x = - F21_x     Newton's Third Law (components)", "(NTL-vector ((?axis . x)))", "Newton's Third Law",
		LEAF, "F12_y = - F21_y     Newton's Third Law (components)", "(NTL-vector ((?axis . y)))", "Newton's Third Law",
		LEAF, "Fg = G*m1*m2/r^2    Universal gravitation", "ug", "Universal Gravitation",
	{END_GROUP}, // Newtons Laws/Translational

	{GROUP, "Rotational",},
		LEAF, "$tnet_z = I*$a_z        rotational form of Newton's 2nd Law", "NL-rot", "Newton's Law for rotation",
		LEAF, "$t_z = r*F*sin($qF-$qr)  torque defined", "torque-zc", "Individual torque magnitude",
		LEAF, "$tnet_z = $S $ti_z       Net torque defined", "net-torque-zc","Net torque",
		LEAF, "$t = r*F*sin($q)        torque magnitude", "mag-torque", "Individual torque magnitude",
	{END_GROUP}, // Newtons Laws/Rotational
 {END_GROUP}, // Newtons Laws

{GROUP, "Work and Energy",},
	 LEAF, "W = F*d*cos($q)         work defined", "work", "Work done by a force",
	 LEAF, "Wnet = $S Wi            Net work defined", "net-work",  "Net work",
	 LEAF, "Wnet = Kf - Ki         work-energy theorem", "work-energy", "Work-Energy",
	 LEAF, "ME = K + $S Ui          mechanical energy defined", "mechanical-energy", "Conservation of Energy",
	 LEAF, "ME1 = ME2              [Wnc=0] conservation of mechanical energy", "cons-energy", "Conservation of Energy",
	 LEAF, "Wnc = ME2 - ME1        change in mechanical energy", "change-me", "Conservation of Energy",
	 LEAF, "Pavg = W/t             average power defined", "power", "Power",
	 LEAF, "Pnet = Wnet/t          average net power defined", "net-power", "Power",
	 LEAF, "P = F*v*cos($q)         instantaneous power", "inst-power", "Power",
{END_GROUP}, // Work and Energy

 {GROUP, "Impulse Momentum",},
	{GROUP, "Translational"},
		LEAF, "pi_x = pf_x         Conservation of momentum", "(cons-linmom ((?axis . x)))","Conservation of Momentum",
		LEAF, "pi_y = pf_y         Conservation of momentum", "(cons-linmom ((?axis . y)))","Conservation of Momentum",
		LEAF, "K1 = K2             Elastic collision defined", "cons-ke-elastic", "Elastic collisions",
	{END_GROUP}, 
	{GROUP, "Rotational",},
	    LEAF, "L_z = I * $w_z       Angular momentum defined", "ang-momentum","Angular momentum definition",
		LEAF, "Li_z = Lf_z         Conservation of Angular momentum", "cons-angmom", "Conservation of Angular momentum",
	{END_GROUP}, 
 {END_GROUP},

{GROUP, "Electricity and Magnetism",},
	 LEAF, "F = abs(q)*E                  Electric field (magnitude)", "charge-force-Efield-mag", "Electric Field",
	 LEAF, "F_x = q*E_x                   Electric field", "(charge-force-Efield ((?axis . x)))", "Electric Field",
	 LEAF, "F_y = q*E_y                   Electric field", "(charge-force-Efield ((?axis . y)))", "Electric Field",
	 LEAF, "E = kelec*abs(q)/r^2          point charge field (magnitude)", "point-charge-Efield-mag", "Point Charge Field",
	 LEAF, "E_x = (kelec*q/r^2) * cos($qr) point charge field", "(point-charge-Efield ((?axis . x)))", "Point Charge Field",
	 LEAF, "E_y = (kelec*q/r^2) * sin($qr) point charge field", "(point-charge-Efield ((?axis . y)))", "Point Charge Field",
	 LEAF, "Enet_x = E1_x + E2_x + ...    Net Electric Field", "(net-Efield ((?axis . x)))", "Electric Field",
	 LEAF, "Enet_y = E1_y + E2_y + ...    Net Electric Field", "(net-Efield ((?axis . y)))", "Electric Field",
	 LEAF, "V = kelec*q/r                 point charge potential", "point-charge-potential", "Electric Potential",
	 LEAF, "Vnet = V1 + V2 + V3...        Net potential", "net-potential", "Electric Potential",
	 LEAF, "Ue = q*Vnet                   electric potential energy", "electric-energy", "Electric Potential",
	 LEAF, "F = abs(q)*v*B*sin($q)         Magnetic force (magnitude)", "charge-force-Bfield-mag", "Magnetic Field",
	 LEAF, "F_x = q*(v_y*B_z - v_z*B_y)   Magnetic force (x component)", "charge-force-Bfield-x", "Magnetic Field",
	 LEAF, "F_y = q*(v_z*B_x - v_x*B_z)   Magnetic force (y component)", "charge-force-Bfield-y", "Magnetic Field",
	 LEAF, "F_z = q*(v_x*B_y - v_y*B_x)   Magnetic force (z component)", "charge-force-Bfield-z", "Magnetic Field",
{END_GROUP},

 {GROUP, "DC Circuits",},
	 LEAF, "+/-V1 +/-V2 +/-V3...=0     Kirchoff's Loop rule", "loop-rule", "Loop Rule",
	 LEAF, "Iin = Iout                 Kirchoff's junction rule", "junction-rule", "Junction Rule",
	 LEAF, "P = V*I                    Electric power", "electric-power", "Electric Power",
	{GROUP, "Resistance"},
		LEAF, "Req = R1 + R2 + R3 ...     Equivalent resistance series", "equiv-resistance-series", "Series Resistors",
		LEAF, "1/Req = 1/R2 + 1/R2 ...    Equivalent resistance parallel", "equiv-resistance-parallel", "Parallel Resistors",
		LEAF, "V = I*R                    Ohm's Law", "ohms-law", "Ohm's Law",
	{END_GROUP}, // Resistance
	{GROUP, "Capacitance"},
		LEAF, "C = q/V                    Capacitance defined", "cap-defn", "Capacitance",
		LEAF, "Ceq = C1 + C2 + C3 + ...   Equivalent capacitance parallel", "equiv-capacitance-parallel", "Parallel Capacitors",
		LEAF, "1/Ceq = 1/C1 + 1/C2 + ...  Equivalent capacitance series", "equiv-capacitance-series", "Series Capacitors",
		LEAF, "q1 = q2                    Charge on series capacitors", "charge-same-caps-in-branch", "Series Capacitors",
		LEAF, "U = 0.5*q*V                Energy stored in a capacitor", "cap-energy", "Capacitor Energy",
		LEAF, "q = C*Vb*(1 - exp(-t/(R*C))) Charge on RC circuit capacitor", "charge-on-capacitor-at-time", "RC Circuits",
		LEAF, "I = (Vb/R)*exp(-t/(R*C)))    Current in RC circuit", "current-in-RC-at-time", "RC Circuits",
		LEAF, "q = fraction*C*Vb          RC charge as fraction of max", "charge-on-capacitor-percent-max", "RC Circuits",
	{END_GROUP}, // Capacitance
	{GROUP, "Inductance"},
		LEAF, "V = -L*dIdt               Inductor EMF", "inductor-emf", "Inductance",
		LEAF, "dIdt = (I2-I1)/t12        Constant rate of change (or avg)", "avg-rate-current-change", "Inductance",
		LEAF, "U = 0.5*L*I^2             Energy stored in inductor", "inductor-energy", "Inductance",
		LEAF, "$t = L/R                   LR time constant", "LR-time-constant", "LR Circuits",
		LEAF, "I = If*(1 - exp(-t/$t)     LR current growth", "LR-current-growth", "LR Circuits",
		LEAF, "If = Vb/R                 LR growth final current", "LR-growth-Imax", "LR Circuits",
		LEAF, "I = I0*exp(-t/$t)          LR current decay", "LR-current-decay", "LR Circuits",
		LEAF, "I0 = Vb/R                 LR decay initial current", "LR-decay-Imax", "LR Circuits",
	{END_GROUP}, // Inductance
 {END_GROUP}, // DC Circuits

 {GROUP, "Optics",},
	 LEAF, "1/do + 1/di = 1/f       Thin Lens/Mirror equation", "lens-eqn", "Lens Equation",
	 LEAF, "m = -di/do              (Lateral) Magnification", "magnification-eqn", "Magnification",
	 LEAF, "f = r/2                 Focal Length of Mirror", "focal-length-mirror",  "Spherical Mirror",
	 LEAF, "do2 = d12 - di1         Combination of Lenses", "lens-combo", "Combined Lenses",
	 LEAF, "m12 = m1*m2             Combined Magnification", "combo-magnification", "Magnification",
	 LEAF, "1/f12 = 1/f1 + 1/f2     Compound Lens (Touching Lenses)", "compound-focal-length", "Touching Lenses",
{END_GROUP, },

#endif !LITE
 // special text here used to mark end of major principle section:
{END_GROUP, szEndPrinciples},  // End "Apply a Principle

{GROUP, "Apply a Definition or Auxiliary Law"},
 {GROUP, "Kinematics"},
#if !LITE
   {GROUP, "Translational"},
        LEAF, "dnet_x = $S di_x            net displacement",      "(net-disp ((?axis . x)))", "Net Displacement",
		LEAF, "dnet_y = $S di_y            net displacement",      "(net-disp ((?axis . x)))", "Net Displacement",
#endif
   	    LEAF, "v(avg)_x = d_x/t            average velocity", 	   "(avg-velocity ((?axis . x)))", "Average velocity",
		LEAF, "v(avg)_y = d_y/t            average velocity",     "(avg-velocity ((?axis . y)))", "Average velocity",
		LEAF, "a(avg)_x = (vf_x - vi_x)/t  average acceleration", "(lk-no-s ((?axis . x)))", "Average acceleration",
		LEAF, "a(avg)_y = (vf_y - vi_y)/t  average acceleration", "(lk-no-s ((?axis . y)))", "Average acceleration",
		LEAF, "a = g                       accel in free-fall", "free-fall-accel","Free fall acceleration", 
		LEAF, "g = 9.8 m/s^2               g near Earth",    "std-constant-g", "Value of g near Earth",
		LEAF, "vf_x = vi_x                 [v_x is constant (a_x =0)]", "const-vx", "Constant velocity component",
#if !LITE
		LEAF, "T = 2*$p*r/v                period, uniform circular motion", "period-circle", "Period Circular",
  {END_GROUP}, // Kinematics/Translational
  {GROUP, "Rotational"},
  		LEAF, "$w(avg) = $q/t                  average angular velocity", "ang-sdd", "Angular velocity",
		LEAF, "$a(avg) = ($wf - $wi)/t          average angular acceleration", "rk-no-vf", "Angular acceleration",
  {END_GROUP}, // Kinematics/Rotational
#endif 
 {END_GROUP}, // Kinematics

#if !LITE
{GROUP, "Newton's Laws"},
   {GROUP, "Translational"},
	{GROUP,"Force Laws",},
		 LEAF, "Fw = m * g                Weight Law", "wt-law", "Weight Law",
		 LEAF, "Ffk = $u * Fn              Kinetic Friction", "kinetic-friction","Kinetic Friction",
		 LEAF, "Ffs = $u * Fn              Static Friction (at max)", "static-friction", "Static Friction max",
		 LEAF, "Fs = k * d                Hooke's Law", "spring-law", "Hooke's Law",
		 LEAF, "Ft1 = Ft2                 Equal tensions at both ends", "tensions-equal", "Equal tensions at both ends" ,
		 LEAF, "Fg = G*m1*m2/r^2          Universal gravitation", "ug", "Universal Gravitation",
		 LEAF, "G = 6.67E-11 N*m^2/kg^2   Gravitational constant", "grav-constant", "Universal Gravitation",
		 // LEAF, "Count Forces", "num-forces", "Count Forces",
	{END_GROUP}, // Newtons/Translational/ForceLaws
	{GROUP, "Compound Bodies",},
		 LEAF, "m12 = m1 + m2              Mass of compound", "mass-compound","Mass of a compound body",
		 LEAF, "v_compound = v_part        Velocity of compound", "(kine-compound ((?vec-type . velocity)))","Kinematics of compound same as part",
		 LEAF, "a_compound = a_part        Acceleration of compound", "(kine-compound ((?vec-type . acceleration)))","Kinematics of compound same as part",
		 LEAF, "F_on_part = F_on_compound  Force on compound", "force-compound","Force on a compound body",
	{END_GROUP},

  {END_GROUP}, // Newton's Laws/Translational
  {GROUP, "Rotational"},
     LEAF, "$tnet_z = $S $ti_z             Net torque defined", "net-torque-zc","Net torque",
	 LEAF, "$t_z = r*F*sin($qF-$qr)        torque defined", "torque-zc", "Individual torque magnitude",
	 LEAF, "$t = r*F*sin($q)              torque magnitude", "mag-torque", "Individual torque magnitude",
	 {GROUP, "Moment of Inertia"},
		  LEAF, "I = (1/12) m*L^2          rod about center", "I-rod-cm","Long thin rod about center of mass",
		  LEAF, "I = (1/3) m*L^2           rod about end",    "I-rod-end","Long thin rod about end",
		  LEAF, "I = m*r^2                 hoop",             "I-hoop-cm","Hoop about center of mass",
		  LEAF, "I = (1/2) m*r^2           cylinder",         "I-cylinder",  "Cylinder",  
		  LEAF, "I = (1/12) m*(L^2 + W^2)  rectangular plate","I-rect-cm", "Rectangle about center of mass",
		  LEAF, "I12 = I1 + I2             compound body",    "I-compound","Compound body",
	 {END_GROUP},

   {END_GROUP}, // Newton's Laws/Rotational
 {END_GROUP}, //Newton's Laws

 {GROUP, "Work and Energy"},
  	 LEAF, "W = F*d*cos($q)         work done by a force", "work", "Work done by a force",
	 LEAF, "Wnet = $S Wi            Net work", "net-work",  "Net work",
	 LEAF, "Wnc = $S Wncf_i         work by non-conservative", "work-nc", "Conservation of Energy",
	 LEAF, "ME = K + $S Ui          mechanical energy defined", "mechanical-energy", "Conservation of Energy",
	 LEAF, "K = 0.5*m*v^2          kinetic energy defined", "kinetic-energy", "Conservation of Energy",
	 LEAF, "Ug = m*g*h             gravitational potential energy [near Earth]", "grav-energy", "Conservation of Energy",
	 LEAF, "Us = 0.5*k*d^2         spring potential energy", "spring-energy", "Conservation of Energy",
	 LEAF, "Ue = q*V               electric potential energy", "electric-energy", "Electric Potential",
	 LEAF, "h2 - h1 = d12_y        change in height", "height-dy", "Height and Displacement",
	 LEAF, "Pavg = W/t             average power defined", "power", "Power",
	 LEAF, "Pnet = Wnet/t          avg. net power defined", "net-power", "Power",
 {END_GROUP}, // Work and Energy

 {GROUP, "Impulse Momentum"},
   {GROUP, "Translational"},
  	   LEAF, "pi_x = m * v_x      momentum (x component)", "(momentum-compo ((?axis . x)))","Conservation of Momentum",
	   LEAF, "pi_y = m * v_y      momentum (y component)", "(momentum-compo ((?axis . y)))","Conservation of Momentum",
   {END_GROUP}, // Momentum/Translational
   {GROUP, "Rotational"},
       LEAF, "L_z = I * $w_z          Angular momentum", "ang-momentum","Angular momentum definition",
   {END_GROUP}, // Momentum/rotational
 {END_GROUP}, // Impulse Momentum
#endif

{END_GROUP}, // End apply a definition/Auxiliary Law

// LEAF, "Projection Equations", "proj",  // this is projection when it's a top-level psm.
{GROUP, "Calculate a vector component"},
     {GROUP, "Displacement"},
	    LEAF, "d_x = d*cos($qd - $qx)   on x-axis", "(projection ((?axis . x) (?vector . (displacement ?body))))", "Projection Equations",
		LEAF, "d_y = d*sin($qd - $qx)   on y-axis", "(projection ((?axis . y) (?vector . (displacement ?body))))", "Projection Equations",
	 {END_GROUP},
	 {GROUP, "Velocity"},
	   LEAF, "v_x = v*cos($qv - $qx)    on x-axis", "(projection ((?axis . x) (?vector . (velocity ?body))))", "Projection Equations",
	   LEAF, "v_y = v*sin($qv - $qx)    on y-axis", "(projection ((?axis . y) (?vector . (velocity ?body))))", "Projection Equations",
	 {END_GROUP},
	 {GROUP, "Acceleration"},
	   LEAF, "a_x = a*cos($qa - $qx)    on x-axis", "(projection ((?axis . x) (?vector . (acceleration ?body))))", "Projection Equations",
	   LEAF, "a_y = a*sin($qa - $qx)    on y-axis", "(projection ((?axis . y) (?vector . (acceleration ?body))))","Projection Equations",
	 {END_GROUP},
#if!LITE
	{GROUP, "Force"},
	      LEAF, "F_x = F*cos($qF - $qx)    on x-axis", "(projection ((?axis . x) (?vector . (force . ?args))))", "Projection Equations",
	      LEAF, "F_y = F*sin($qF - $qx)    on y-axis", "(projection ((?axis . y) (?vector . (force . ?args))))","Projection Equations", 
	 {END_GROUP},
	 {GROUP, "Relative Position"},
	   LEAF, "r_x = r*cos($qr - $qx)    on x-axis", "(projection ((?axis . x) (?vector . (relative-position ?body ?origin))))", "Projection Equations",
	   LEAF, "r_y = r*sin($qr - $qx)    on y-axis", "(projection ((?axis . y) (?vector . (relative-position ?body ?origin))))","Projection Equations",
	 {END_GROUP},
	 {GROUP, "Momentum"},
	   LEAF, "p_x = p*cos($qp - $qx)    on x-axis", "(projection ((?axis . x) (?vector . (momentum ?body))))", "Projection Equations",
	   LEAF, "p_y = p*sin($qp - $qx)    on y-axis", "(projection ((?axis . y) (?vector . (momentum ?body))))", "Projection Equations",
     {END_GROUP},
#endif
{END_GROUP}, // Projections

#if !LITE
{GROUP, "Use information specific to this problem"},
	// not clear if we want 'given here:
	// LEAF, "var = N units       [Enter a given value]", "given", "Use Given Information",
	LEAF, "s1 = s2              Equivalent distances",  "(equals ((?quant1 . (at (distance ?body) ?time))))", "Equivalent quantities",
	LEAF, "t02 = t01 + t12      Sum of times",   "sum-times", "Sum of times",
	LEAF, "c^2 = a^2 + b^2      Pythagorean Thm", "pyth-thm", "Pythagorean Theorem",
	LEAF, "a1 = a2              Connected accelerations","connected-accels","Equal accelerations",
	LEAF, "v1 = v2              Connected velocities", "connected-velocities","Equal velocities",
	LEAF, "Ft1 = Ft2            Equal tensions at both ends", "tensions-equal", "Equal tensions at both ends",
	LEAF, "r = sqrt(r_x^2 + r_y^2) Position mag from components", "rmag-pyth", "Pythagorean Theorem",
	LEAF, "r21_x = x2 - x1      Relative Position from coordinates", "(rdiff ((axis . ?x)))", NULL,
	LEAF, "r21_y = y2 - y1      Relative Position from coordinates", "(rdiff ((axis . ?y)))", NULL, 

{END_GROUP}, // End use problem-specific info
#endif

};

#define NTREEITEMS sizeof(treeItems)/sizeof(treeItems[0])

void CPsmDlg::DumpTable()
{
	CStdioFile fileOut("C:/Andes2/kb/principles.tsv", CFile::modeWrite |CFile::modeCreate);
	for (int i = 0; i<NTREEITEMS; i++) {
		CString strLine;
		const struct itemInfo* pItem = &treeItems[i];
		char* szType = (pItem->nType == GROUP) ? "GROUP" : (pItem->nType==END_GROUP) ? "END_GROUP" : "LEAF";
		// convert NULL string pointers as empty strings
		char* szText = pItem->szText ? pItem->szText : "";
		char* szHelpID = pItem->szHelpID ? pItem->szHelpID : "";
		char* szName = pItem->szName ? pItem->szName : "";
		strLine.Format("%s\t%s\t%s\t%s\n", szType, szText, szHelpID, szName);
		fileOut.WriteString(strLine);
	}
}
*/

// Struct for dynamically loaded psm info, uses CStrings
typedef struct
{	
	int nType;				// GROUP, LEAF, END_GROUP
	CString strText;		// equation text plus name
	CString strHelpID;		// term to send to help sys
	CString strName;		// "principle name" = help file name
} ItemInfo;
	
typedef CTypedPtrArray<CPtrArray, ItemInfo*> CInfoArray;
static CInfoArray m_items;

void CPsmDlg::AddInfo(CString strType, CString strText, CString strHelpID, CString strName, int nLine)
{
	if (strType.IsEmpty() && strText.IsEmpty() && strHelpID.IsEmpty() && strName.IsEmpty()) {
		TRACE("Ignoring blank line %d in principles.tsv\n", nLine);
		return;
	}

	int nType = LEAF;	// to barge on with if unparseable type
	if (strType.CompareNoCase("LEAF") == 0) nType = LEAF;
	else if (strType.CompareNoCase("GROUP") == 0) nType = GROUP;
	else if (strType.CompareNoCase("END_GROUP") == 0) nType = END_GROUP;
	else { 
		CString strMsg;
		strMsg.Format("Warning: Bad item type string at principles.tsv line %d: |%s|", nLine, strType);
		AfxMessageBox(strMsg);
	}
	ItemInfo* pItem = new ItemInfo();
	pItem->nType = nType;
	pItem->strText = strText;
	if (nType == LEAF) {
		pItem->strHelpID = strHelpID;
		pItem->strName = strName;
	}
	m_items.SetAtGrow(m_items.GetSize(), pItem);
}

void CPsmDlg::LoadPsmInfo(LPCSTR pszPathName)
{
	extern int split(const CString& str, const CString& Sep, CStringArray& result);
	CStdioFile fileSrc(pszPathName, CFile::modeRead);
	CString strLine;
	CStringArray strFields;
	int nLine = 0;
	while (fileSrc.ReadString(strLine)) {
		++nLine;
		if (strLine.IsEmpty()) continue;
		int nFields = split(strLine, "\t", strFields);
		// fourth field is optional
		ASSERT(nFields >= 3);
		if (nFields == 3) strFields.Add("");
		AddInfo(strFields[0], strFields[1], strFields[2], strFields[3], nLine);
	}
}

void CPsmDlg::InitPsmInfo()
{
	try {
		LoadPsmInfo(g_strAndesDir + "kb/" + "principles.tsv");
	}
	catch (CFileException* pEx) {
		pEx->ReportError();
		pEx->Delete();
	}
}

// 
// Although the tree control allows us to do custom drawing to draw the text for items with Greek characters,
// it has no way of letting us tell it the true size of an item. If we give it the text containing the dollar signs
// it will think the text size is larger than it in fact is. So we remove $'s from strings we install in tree 
// control, so that the width it infers for an item's text will be closer to correct. This width affects when it
// decides to scroll, show tooltips. We also use it as the width we use for drawing the highlight rectangle on
// selected items. So it is tolerable if this width is too large; worse if it is too small.
//
// Note this means we must always lookup the text from table entry associated with the hItem, not fetch it from 
// the hItem itself.
CString CPsmDlg::RemoveDollars(LPCTSTR szText)
{
	CString strResult = szText;
	strResult.Remove('$');
	return strResult;
}

// find item in treeItems by text
static int FindItemIndex(LPCSTR szText)
{
	// start after first one
	for (int i = 1; i < m_items.GetSize(); i++) {
		if (m_items[i]->strText == szText)
			return i;
	}
	return -1;
}

void CPsmDlg::PopulateTree()
{
	if (m_bPrinciples) // show Major principles subtree only
	{	
		// Add all subtrees till reach end of the Principles section, marked by special value.
		// start at 1 to skip root-level Apply Principles section heading. 
		int nEndPrinciples = FindItemIndex(szEndPrinciples);
		ASSERT(nEndPrinciples > 0 && nEndPrinciples <= m_items.GetSize());
		for (int i = 1; i < nEndPrinciples; i++)	
			AddSubtree(i); 
	}
	else // add all subtrees in list 
	{
		for (int i = 0;i< m_items.GetSize(); i++) {
			AddSubtree(i);
		}
	}	
	// set initial selection ?
}

// Add a subtree starting with item at index i from table under given parent. 
// For leaves, just adds a single item
// If this item is a group heading, adds all its subitems, consuming items from table through 
// the matching END_GROUP marker for this group level. 
// On exit, possibly modified i is index of last table entry that was consumed:
//   if i is a group, i on exit is on the END_GROUP item.
void CPsmDlg::AddSubtree(int& i, HTREEITEM hParent /* =TVI_ROOT */)
{
	static int iDepth = 1;
	ASSERT(i >= 0 && i < m_items.GetSize());
	ASSERT(m_items[i]->nType == LEAF || m_items[i]->nType == GROUP); // shouldn't be called on END_GROUP markers

	TRACE("[%d] AddSubtree i= %d: %s\n", iDepth, i, m_items[i]->strText);

	// insert it, associating index as item data
	HTREEITEM hItem = m_tree.InsertItem(RemoveDollars(m_items[i]->strText), 0, 0, hParent, TVI_LAST);
	m_tree.SetItemData(hItem, i);
	if (i == m_nSelected)
		m_tree.SelectItem(hItem);
	
	if (m_items[i]->nType == GROUP) 
	{
		// Group headings are in boldface
		m_tree.SetItemState(hItem, TVIS_BOLD , TVIS_BOLD);
	
		// loop until hit group end to add all following child items
		CString strThis = m_items[i]->strText;
		TRACE("'%s' Is Group heading, recursing to add subitems\n", strThis);
		while (m_items[++i]->nType != END_GROUP) {
			++iDepth;
			AddSubtree(i, hItem); // recurse to save current parent on stack
			--iDepth;
			TRACE("[%d] Finished a subtree under %s, i now = %d\n", iDepth, strThis, i);
		}

		TRACE("[%d] Hit end of group %s, i now = %d\n", iDepth, strThis, i);
	} 
}

void CPsmDlg::OnOK() 
{
	// If not selecting, just close. Note selection doesn't persist in this case.
	if (! m_bSelect) {
		CLogDialog::OnOK();
		return;
	}

	// Else ensure they selected a principle.

	// Get the selection
	HTREEITEM hSel = m_tree.GetSelectedItem();
	if (hSel == NULL) {
		theApp.DoWarningMessage("Please open a section and select a principle", this);
		return;
	}
	// else got a selection
	int j = m_tree.GetItemData(hSel);
	TRACE("Selected item no %d\n", j);
	// Complain if it is a group item, opening the group to show them.
	if (m_items[j]->nType == GROUP) {
		m_tree.Expand(hSel, TVE_EXPAND);
		theApp.DoWarningMessage("You selected a section heading. Please open a section and select a principle within it.", this);
		// Don't go through to CLogDialog::OnOK
		return;
	}

	ASSERT(j >= 0 && j < m_items.GetSize());
	m_strPSM = m_items[j]->strText;
	m_strHelpID = m_items[j]->strHelpID;
	// remember in case save for next time
	m_nSelected = j;

	CLogDialog::OnOK();
}

CString CPsmDlg::GetItemLesson(HTREEITEM hItem)
{
	int i = m_tree.GetItemData(hItem);
	ASSERT(0 <= i && i <= m_items.GetSize());

	// minilesson base name is principle name with spaces and puncutation removed
	// This comes from the name field; if NULL, just use item text.
	CString strLessonName;
	if (m_items[i]->strName)
		strLessonName = m_items[i]->strName;
	else
		strLessonName = m_items[i]->strText;

	// special case for Average Speed = distance/Time
	// int posEq = strItemText.Find('=');
	// if (posEq!= -1)
	//		strLessonName = strItemText.Left(posEq);

	strLessonName.Remove(' ');
	strLessonName.Remove('\''); // in Hooke's, Newton's Laws 
	// preserve hyphens as in Work-Energy-

	return strLessonName + ".html";
}

void CPsmDlg::OnPsmHelp() 
{
	// Get the selection's text, if any.
	HTREEITEM hSel = m_tree.GetSelectedItem();
	if (!hSel) return;

	CString strLessonName;
	strLessonName = GetItemLesson(hSel);
	if (strLessonName.IsEmpty()) 
		return;

	// Log it by psm id. (Really want it by KC, i.e. operator name). 

	// show it in our lesson browser
	theApp.ShowLesson(strLessonName);
}

void CPsmDlg::UpdateHelpButton()
{
	// Get the selection's text, if any.
	HTREEITEM hSel = m_tree.GetSelectedItem();
	if (!hSel) {
		m_btnHelp.EnableWindow(FALSE);
		return;
	}

	// Enable it as lesson file exists
	CString strLessonName;
	strLessonName = GetItemLesson(hSel);
	if (strLessonName.IsEmpty()) {
		m_btnHelp.EnableWindow(FALSE);
	}

	CString strPath = g_strAndesDir + g_szLessonDir + "\\" + strLessonName;
	CFileStatus statFile;
	m_btnHelp.EnableWindow(CFile::GetStatus(strPath, statFile));
}

void CPsmDlg::OnSelchangedPsmTree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM hSel = pNMTreeView->itemNew.hItem;
	
	// Log selection change for log playback
	// include id in log message for robustness, since  labels, number and 
	// organization of items may change. Include label for readability.
	int j = m_tree.GetItemData(hSel);
	CString strId = "NIL";
	if (j >= 0 && j < m_items.GetSize()) {
		strId = m_items[j]->strHelpID;
	}
	CString strLabel = m_tree.GetItemText(hSel);
	LogEventf(EV_PSM_SELECT, "%d %s %s", j, strId, strLabel);

	// Do something on selection change like update help button 
	UpdateHelpButton();
	*pResult = 0;
}




void CPsmDlg::OnItemexpandedPsmTree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTV = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM hItem = pNMTV->itemNew.hItem;
	// UINT action = pNMTV->action; // TVE_COLLAPSE or TVE_EXPAND
	BOOL bExpanded = (pNMTV->itemNew.state & TVIS_EXPANDED);
	
	// Log expand/collapse for replay
	int j = m_tree.GetItemData(hItem);
	CString strLabel = m_tree.GetItemText(hItem);

	// !!! Log includes section names but not all unique:
	// "Translational", "Rotational" multiply used.
	// bExpanded comes first in arg list.
	LogEventf(EV_PSM_EXPAND, "%d %d %s", bExpanded, j, strLabel);

	*pResult = 0;
}


void CPsmDlg::OnDblclkPsmTree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	// Do something on double-click like OK or show lesson?
	
	*pResult = 0;
}

BOOL CPsmDlg::DispatchEvent(EventID id, LPCTSTR pszArgs)
{
	int nData;
	BOOL bExpand;
	CString strId;

	switch (id) 
	{
	case EV_PSM_SELECT:	// "%d %s %s", j, strId, strLabel);
		// cheap, find item by item data integer code
		// !!! should replay by id for compatibility over change in indices
		// Id can contain white space, but its either inside parens or a single token.
		if (sscanf(pszArgs, "%d", &nData) == 1) {
			HTREEITEM hItem = FindItemData(nData);
			m_tree.SelectItem(hItem);
		}
		break;

	case EV_PSM_EXPAND:	// "%d %d %s", bExpanded, j, strLabel);
		// cheap, find item by item data integer code
		// should try to replay by label for compatibility over change in indices
		if (sscanf(pszArgs, "%d %d", &bExpand, &nData) == 2) {
			HTREEITEM hItem = FindItemData(nData);
			m_tree.Expand(hItem, bExpand ? TVE_EXPAND : TVE_COLLAPSE);
		}
		break; 


	case EV_LEFT_UNSENT:	// from wizard of OZ logging, now just ignore
	case EV_UNSENT_CONTENTS:
		return TRUE;

	default:
		break;
	}
	// else pass to base class for generic dialog events like button presses and moves
	return CLogDialog::DispatchEvent(id, pszArgs);
}

// generic routine to find tree item by item data. Copied from CodeGuru site.
HTREEITEM CPsmDlg::FindItemData(DWORD dwData, HTREEITEM hItem /* = TVI_ROOT */)
{
	if(!hItem)
		return NULL;
	
	// see if it's on this item
	// AW-added: seems to bomb on TVI_ROOT so inserted test
	if(hItem != TVI_ROOT && m_tree.GetItemData(hItem) == dwData) 
		return hItem;

	HTREEITEM hRet = NULL;
	HTREEITEM hChild = m_tree.GetChildItem(hItem);
	if(hChild) 
		hRet = FindItemData(dwData, hChild);

	if(hRet == NULL) {
		HTREEITEM hSibling = m_tree.GetNextSiblingItem(hItem);
		if(hSibling)
			hRet = FindItemData(dwData, hSibling);
	}

	return hRet;
}


// handle custom-draw messages sent in WM_NOTIFY messages to take control of rendering 
// mixed-font text items containing Greek characters.
BOOL CPsmDlg::OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult) 
{
	LPNMHDR pNmhdr = (LPNMHDR)lParam;
	switch (pNmhdr->code)
	{
	case NM_CUSTOMDRAW:
		{
		LPNMTVCUSTOMDRAW pCD = (LPNMTVCUSTOMDRAW)lParam;
		switch (pCD->nmcd.dwDrawStage)
		{
			case CDDS_PREPAINT:
				// Need to process this case and set pResult to CDRF_NOTIFYITEMDRAW, 
				// otherwise parent will never receive CDDS_ITEMPREPAINT notification.  
				*pResult = CDRF_NOTIFYITEMDRAW;
				return true;

			case CDDS_ITEMPREPAINT:
			{
				int i;
				i  = pCD->nmcd.lItemlParam;
				ASSERT(i >= 0 && i < m_items.GetSize());
				CString strText = m_items[i]->strText;
			
				// if no Greek, just let tree draw the whole item as usual
				if (strText.Find('$') == -1) { 
					*pResult = CDRF_DODEFAULT;
					return FALSE;
				}

				// Else we let the control paint the item so we don't have to paint expand/collapse widgets and 
				// lines, but set text background and text color to window background color
				// so text portion remains blank while control paints the item.  We will paint the mixed font
				// text over the blank text region after the control is done doing its part (below).
				pCD->clrText = ::GetSysColor(COLOR_WINDOW);
				pCD->clrTextBk = ::GetSysColor(COLOR_WINDOW);
				// enroll to receive POSTPAINT notification for the item.
				*pResult = CDRF_NOTIFYPOSTPAINT;
				return TRUE;
			}

			case CDDS_ITEMPOSTPAINT:
			{
				HTREEITEM hItem = (HTREEITEM)pCD->nmcd.dwItemSpec;
				UINT uItemState = pCD->nmcd.uItemState;
				BOOL bSelected = (uItemState & CDIS_SELECTED) ? 1 : 0;
				BOOL bFocus = (uItemState & CDIS_FOCUS) ? 1 : 0;
				BOOL bHot = (uItemState & CDIS_HOT) ? 1 : 0;

				int i = pCD->nmcd.lItemlParam;
				ASSERT(i >= 0 && i < m_items.GetSize());
				CString strText = m_items[i]->strText;
			
				// When painting item text, need to respect state flags:
				// TRACE("Draw %s  Selected:%d  Focus:%d  Hot:%d\n", strText, bSelected, bFocus, bHot);
			
				CDC* pDC = CDC::FromHandle(pCD->nmcd.hdc);
				COLORREF clrTextSave, clrBkSave;
				CRect rcItem;
				m_tree.GetItemRect(hItem, &rcItem, TRUE);
				// Note rcItem width is not perfect because based on width of plain text string we installed, and
				// Greek character metrics may be slightly different. Hopefully drawn text will always be smaller
				// so it doesn't overflow this box. Seems to be OK in practice.
				
				// CDIS_SELECTED state: draw highlight rectangle
				// In standard control, selected text is HILIGHTTEXT on HIGHLIGHT bg when focussed, 
				// but normal text on somewhat dark grey (what sys color???) when not focussed
				if (bSelected)
				{
					clrTextSave = pDC->SetTextColor(bFocus ? ::GetSysColor(COLOR_HIGHLIGHTTEXT) : ::GetSysColor(COLOR_WINDOWTEXT));
					clrBkSave = pDC->SetBkColor(bFocus ? ::GetSysColor(COLOR_HIGHLIGHT) : RGB(192,192,192));

					pDC->FillRect(rcItem, &CBrush(bFocus ? ::GetSysColor(COLOR_HIGHLIGHT) : RGB(192,192,192)));
				}
				else if (bHot) // && ! bSelected
				{
					// CDIS_HOT state: when tracking mouseovers, hot text is colored blue if not selected
					//                     and underlined in all cases
					// Think hot color is GetSysColor(COLOR_HOTLIGHT) which exists on Win98, NT5.0 & later
					clrTextSave = pDC->SetTextColor(RGB(0, 0, 255)); 
				}			

				// Draw the text. Note returned rcText may be different than initial rcItem the control gave us
				// Hopefully drawn text should always be smaller so text doesn't overflow item bounds
				CRect rcText = CGreekText::DrawText(pDC, rcItem + CPoint(2, 1), strText);
				
				// draw focus rect if needed
				if (bFocus)
					pDC->DrawFocusRect(rcItem);

				// if hot, add underline in same color as text (blue or black).
				if (bHot) {
					CPen penLine(PS_SOLID, 0, pDC->GetTextColor());
					CPen* pOldPen = pDC->SelectObject(&penLine);
					// what vertical position to use? Should derive from baseline using text metrics?
					pDC->MoveTo(rcItem.left, rcItem.bottom - 3); 
					pDC->LineTo(rcItem.right, rcItem.bottom - 3);
					pDC->SelectObject(pOldPen);
				}
				
				// restore colors changed if item was selected or hot
				if (bSelected) {
					pDC->SetTextColor(clrTextSave);
					pDC->SetBkColor(clrBkSave);
				} else if (bHot)
					pDC->SetTextColor(clrTextSave);

				*pResult = CDRF_SKIPDEFAULT;
				return TRUE;
			}

		}
	}
	break;
	}

	// else pass along to base class handler
	return CLogDialog::OnNotify(wParam, lParam, pResult);
}

// adjust controls to handle resize of window
void CPsmDlg::OnSize(UINT nType, int cx, int cy) 
{
	CLogDialog::OnSize(nType, cx, cy);
	
	// for now just adjust width of tree control.

	if (!m_tree.m_hWnd)
		return;
	
	CRect rcTree;
	m_tree.GetWindowRect(rcTree);
	ScreenToClient(rcTree);
	int cxLeftMargin = rcTree.left;
	int cyTopMargin = rcTree.top; // assume top margin around tree OK for bottom.
	m_tree.MoveWindow(rcTree.left, rcTree.top, cx - 2*cxLeftMargin, cy - 2*cyTopMargin);
	
	// move the buttons up or down to lie along the bottom row.
	CRect rcOK, rcCancel;
	m_btnOk.GetWindowRect(rcOK); ScreenToClient(rcOK);
	m_btCancel.GetWindowRect(rcCancel); ScreenToClient(rcCancel);
	int cyBottomMargin = 9; // guess
	m_btnOk.MoveWindow(rcOK.left, cy - (rcOK.Height() + cyBottomMargin), rcOK.Width(), rcOK.Height());
	m_btCancel.MoveWindow(rcCancel.left, cy - (rcCancel.Height() + cyBottomMargin), rcCancel.Width(), rcCancel.Height());
	
}




void CPsmDlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CLogDialog::OnShowWindow(bShow, nStatus);
	
	// In modeless dialog, log hiding of window
	if (! m_bSelect) // now only true for modeless use
	{ 
		if (! bShow)
			LogEventf(EV_HIDE_REVIEW_EQN, "");
	}
}
