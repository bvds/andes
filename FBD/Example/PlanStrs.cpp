//PlanStrs.cpp 
//
//$Id: PlanStrs.cpp,v 1.1 2005/01/24 16:28:10 bvds Exp $
#include "PlanStrs.h"

extern const char* const goalValues[] =
{
	"force",//index = 0
	"mass",//index = 1
	"acceleration",//index = 2
	"velocity",//index = 3
	"displacement",//index = 4
	"compression",//index = 5
	"radius",//index = 6
	"distance",//index = 7
	"time",// index = 8 always should be last
	""
};




extern const stepInfo stepStrings[] =
{
	ID_GENERAL,  "Apply Conservation of Energy",		"Conservation_Of_Energy",
	ID_GENERAL,  "Apply Kinematics",					"Kinematics",
	ID_GENERAL,  "Apply Newton's Second Law",			"Newtons_Second",
	ID_THIRD,    "Apply Newton's third law",			"Newtons_Third",
	ID_FORCE_EQN,"Apply Force equations",				"Force_Equations",
};



extern const stepInfo midstepStrings[]=
{
	
	ID_SYSPROPS,"Describe body's properties",	"Define_Body_Properties",    
	ID_EQNS,	"Write component equations",		"Write_Equations",			 
	ID_FORCES,	"Identify all forces on the body",	"Identify_Forces",			 
	ID_QUANTS,	"Find quantities algebraically",	"Find_Quants_Algebraically", 
	ID_SYSBODY,	"Choose body",						"Choose_Body",				 
	
	
};

extern const substepInfo substepStrings[]=
{
//	ID_SYSPROPS, "Describe body's mass",				"Identify_Mass",			MASS,
//	ID_SYSPROPS, "Identify zero energy points",	"Identify_0_Energy_Points",	ENERGY_POINTS,
//	ID_SYSPROPS, "Identify displacement",		"Identify_Displacement",	DISPLACEMENT,
//	ID_SYSPROPS, "Describe body's acceleration",		"Identify_Acceleration",	ACCELERATION,
//	ID_SYSPROPS, "Identify final velocity",		"Identify_Final_Velocity",	FIN_VELOCITY,
//	ID_SYSPROPS, "Identify initial velocity",	"Identify_Initial_Velocity",INI_VELOCITY,
//	ID_SYSPROPS, "Identify duration",			"Identify_Duration",
	
	ID_SYSPROPS, "Describe body's mass",				"Identify_Mass",			MASS,
	ID_SYSPROPS, "Describe body's acceleration",		"Identify_Acceleration",	ACCELERATION,
	
	ID_EQNS, "Find vector components",		"Find_Components",			  COMPONENTS,
	ID_EQNS, "Choose coordinate axes",		"Choose_Axis",				  AXIS,
	ID_EQNS, "Write equations for Kinematics",	"Write_Kinematics_Equations", EQUATION,
	ID_EQNS, "Write equations for Newton's 2nd law","Write_Newtons_Equations",	  EQUATION,
	ID_EQNS, "Write energy equations",		"Write_Energy_Equations",	  EQUATION,
	
	ID_QUANTS, "Solve for desired quantities", "Solve_For_Sought",		QUANTITIES,
	ID_QUANTS, "Find remaining unknowns",	 "Find_Unknowns",			UNKNOWNS,
		
	ID_FORCE_EQN, "Apply Weight equation",			"Weight_Equation",			FORCE_EQUATION,
	ID_FORCE_EQN, "Apply Static Friction equation",	"Static_Friction_Equation", FORCE_EQUATION,
	ID_FORCE_EQN, "Apply Kinetic Friction equation","Kinetic_Friction_Equation",FORCE_EQUATION,
	ID_FORCE_EQN, "Apply Spring Force equation",	"Spring_Force_Equation",	FORCE_EQUATION,
	
};

#define ID_VCTR_RULES	0
#define ID_NEWTON		1
#define ID_BODY_RULES	2	
#define ID_FORCE_RULES	3
#define ID_AXIS_RULES	4
#define ID_KINEMATIC	5
#define ID_CMPBDY_RULES	6

	
extern const topRuleInfo topRules[]=
{
	ID_BODY_RULES,	"Choosing a Body",		
	ID_CMPBDY_RULES,"Compound Body Properties",
	ID_FORCE_RULES,	"Describing Forces",	
	ID_NEWTON,		"Newton's Laws",		
	ID_AXIS_RULES,	"Choosing Axes",
	ID_VCTR_RULES,	"Finding Vector Components",
	ID_KINEMATIC,	"Kinematics",			
	
};

extern const ruleInfo rules[]=
{
	ID_BODY_RULES,	"USING-FORCE",							ID_UF,
	ID_BODY_RULES,	"USING_VELOCITY",						ID_UV,
	ID_BODY_RULES,	"USING_ACCELERATION",					ID_UA,	
	ID_BODY_RULES,	"COMPOUND_BODY_WITH_SURFACE_CONTACT",		ID_CCB,
	ID_BODY_RULES,	"COMPOUND_BODY_TIED_TOGETHER",		ID_CBBTT,
	ID_CMPBDY_RULES,"MASS_OF_COMPOUND_BODY",				ID_MOCB,
	ID_CMPBDY_RULES,"FORCE_ON_COMPOUND_BODY",					ID_FOCB,
//	ID_FORCE_RULES, "FORCES_ON_COMPOUND_OBJECTS",			ID_HEADER,
//	ID_FORCE_RULES,	"FORCE_ON_COMPOUND_BODY",				ID_FOCB,
//	ID_FORCE_RULES,	"PULL",									ID_HEADER,
//	ID_FORCE_RULES,	"PULL_FORCE_DIRECTION",					ID_UNDEF,
//	ID_FORCE_RULES,	"FORCES_IN_A_TAUT_STRING",				ID_UNDEF,
//	ID_FORCE_RULES,	"TENSION_IS_MAG_OF_TENSION_FORCE",		ID_UNDEF,
//	ID_FORCE_RULES,	"TENSION_MEANS_TENSION_FORCE",			ID_UNDEF,
//	ID_FORCE_RULES,	"TENSION_FORCE_IS_TENSION",				ID_UNDEF,
	ID_FORCE_RULES,	"Weight",								ID_HEADER,
	ID_FORCE_RULES,	"WEIGHT_FORCE_EXISTS",					ID_WFE,
	ID_FORCE_RULES,	"WEIGHT_FORCE_DIRECTION",				ID_WFD,
	ID_FORCE_RULES,	"WEIGHT_FORCE_MAGNITUDE",				ID_WMG,
	ID_FORCE_RULES,	"Tension",								ID_HEADER,
	ID_FORCE_RULES,	"TENSION_EXISTS",						ID_TE,
	ID_FORCE_RULES,	"TENSION_FORCE_DIRECTION",				ID_TFD,
	ID_FORCE_RULES,	"TENSION_FORCE_MAGNITUDE",				ID_UNDEF,
	ID_FORCE_RULES,	"TENSION_EQUAL_EVERYWHERE_IN_MASSLESS_STRING",	ID_UNDEF,
	ID_FORCE_RULES,	"Normal",								ID_HEADER,
	ID_FORCE_RULES,	"NORMAL_FORCE_EXISTS",					ID_NFE,
	ID_FORCE_RULES,	"NORMAL_FORCE_DIRECTION",				ID_NFD,
	ID_FORCE_RULES,	"NORMAL_FORCE_MAGNITUDE",				ID_UNDEF,
	ID_FORCE_RULES,	"Spring",								ID_HEADER,
	ID_FORCE_RULES,	"SPRING_FORCE_EXISTS",					ID_UNDEF,
	ID_FORCE_RULES, "SPRING_FORCE_MAGNITUDE",				ID_UNDEF,
	ID_FORCE_RULES,	"SPRING_FORCE_DIRECTION",				ID_UNDEF,
	ID_FORCE_RULES, "Net Force",                            ID_HEADER,
	ID_FORCE_RULES, "See Newton's 2nd Law...",              ID_TEXT,
	ID_FORCE_RULES,	"Force Components",						ID_HEADER,
	ID_FORCE_RULES, "See Vector Components...",              ID_TEXT,
	ID_FORCE_RULES, "Force on a Compound Body",               ID_HEADER,
    ID_FORCE_RULES, "See Compound Body Properties...",         ID_TEXT,
	ID_NEWTON,		"Newton's Second Law",					ID_HEADER,
	ID_NEWTON,		"EQUATION_FOR_SECOND_LAW",				ID_NSL,
	ID_NEWTON,		"DEFINITION_OF_NET_FORCE",				ID_SOF,
	ID_NEWTON,		"Action Reaction Law",					ID_HEADER,
	ID_NEWTON,		"REACTION_FORCE_EXISTS",				ID_UNDEF,
	ID_NEWTON,		"REACTION_FORCE_MAGNITUDE",				ID_UNDEF,
	ID_NEWTON,		"REACTION_FORCE_DIRECTION",				ID_UNDEF,
//	ID_AXIS_RULES,	"AXIS_ALONG_SOUGHT_FORCE",				ID_AASF,
//	ID_AXIS_RULES,	"AXIS_ALONG_ACCELERATION",				ID_AAA,
//	ID_AXIS_RULES,	"AXIS_ALONG_VELOCITY",					ID_AAV,
//	ID_AXIS_RULES,	"AXIS_ALONG_HORIZONTAL",				ID_AAH,
	ID_AXIS_RULES,	"HOW_TO_CHOOSE_AXES",					ID_HCA,
	ID_VCTR_RULES,	"VECTOR_SAME_DIRECTION_AS_AXIS",		ID_VSDA,
	ID_VCTR_RULES,	"VECTOR_OPPOSITE_DIRECTION_THAN_AXIS",	ID_VAGA,
	ID_VCTR_RULES,  "VECTOR_PERPENDICULAR_TO_AXIS",			ID_VOA,  
	ID_VCTR_RULES,	"VECTOR_INCLINED_RESPECT_TO_AXIS",		ID_VIRA,
	ID_VCTR_RULES,	"NULL_VECTOR_NULL_COMPONENTS",			ID_NVNC,
	ID_KINEMATIC,	"Qualitative",							ID_HEADER,
	ID_KINEMATIC,	"VELOCITY_AT_REST",						ID_UNDEF,
	ID_KINEMATIC,	"ACCELERATION_WITH_CONSTANT_VELOCITY",	ID_UNDEF,
	ID_KINEMATIC,	"APEX_OF_TRAJECTORY",					ID_UNDEF,
	ID_KINEMATIC,	"VELOCITY_AT_APEX",						ID_UNDEF,
	ID_KINEMATIC,	"VELOCITY_DIRECTION",					ID_UNDEF,
	ID_KINEMATIC,	"Equations",							ID_HEADER,
	ID_KINEMATIC,	"KINE_MISSING_V0",						ID_UNDEF,
	ID_KINEMATIC,	"KINE_MISSING_V1",						ID_UNDEF,
	ID_KINEMATIC,	"KINE_MISSING_DX",						ID_UNDEF,
	ID_KINEMATIC,	"KINE_MISSING_ACCEL",					ID_UNDEF,
	ID_KINEMATIC,	"KINE_MISSING_T",						ID_UNDEF,

};
extern const tmplSetup ruleSetup[]=
{
	
	ID_NSL,		"IF an object",	OBJ_PROPS,
	ID_NSL,		"", NOTHING,
	ID_NSL,		"along a certain axis",		NOTHING,
	ID_NSL,		"AND the object",	OBJ_PROPS,
	ID_NSL,		"THEN",	EXISTANCE,
	
	ID_TE,		"IF an object is",						OBJ_RELS,
	ID_TE,		"AND the string",				OBJ_PROPS,
	ID_TE,		"THEN there is a tension force on the object",	NOTHING,
	ID_TE,		"EXERTED BY",					AGENTS,

	ID_WFE,		"IF an object",	OBJ_PROPS,
	ID_WFE,		"AND the object",	OBJ_PROPS,
	ID_WFE,		"THEN there is a weight force on the object",			NOTHING,
	ID_WFE,		"DUE TO",	PRINCIPLES,
	ID_WFE,		"EXERTED BY",	AGENTS,
//*********************************************
	ID_WFD,		"IF",		EXISTANCE,
	ID_WFD,		"THEN it is directed downward ", 		NOTHING,
	ID_WFD,		"SINCE gravitational attraction is directed", DIRECTIONS,

	ID_NFE,		"IF an object is",		OBJ_RELS,
	ID_NFE,		"THEN there is a normal force on the object",			NOTHING,
	ID_NFE,		" EXERTED BY",	AGENTS,

	
	ID_VAGA,	"IF a vector",		VEC_PROPS,	
	ID_VAGA,	"",					NOTHING,
	ID_VAGA,	"THEN the vector component along the axis",	COMP_PROPS,		

	ID_VOA,		"IF a vector",		VEC_PROPS,
	ID_VOA,		"",	NOTHING,
	ID_VOA,		"THEN the vector component along the axis",				COMP_PROPS,

	ID_VSDA,	"IF a vector",		VEC_PROPS,
	ID_VSDA,	"",	NOTHING,
	ID_VSDA,	"THEN the vector component along the axis",			COMP_PROPS,

	ID_NVNC,	"IF a vector",		VEC_PROPS,
	ID_NVNC,	"",	NOTHING,
	ID_NVNC,	"THEN the vector component along the axis",			COMP_PROPS, 

	ID_SOF,		"The net force on an object along an axis is the",		NET_FORCE,
	ID_SOF,		"",		NOTHING,
	ID_SOF,		"",			AXIS_DIR,

	ID_AASF,	"IF we want to find",	DESIRED,
	ID_AASF,	"",		NOTHING,
	ID_AASF,	"THEN we can choose an axis", DIRECTIONS,

	ID_AAA,		"IF the chosen body",	OBJ_PROPS,
	ID_AAA,		"",  NOTHING,
	ID_AAA,		"THEN we can choose an axis", DIRECTIONS,

	ID_AAV,		"IF the chosen body",	OBJ_PROPS,
	ID_AAV,		"",	NOTHING,
	ID_AAV,		"THEN we can choose one of the axes",	DIRECTIONS,

	ID_AAH,		"Choose one of the axes",		DIRECTIONS,

	ID_UF,		"IF we want to find",			DESIRED,
	ID_UF,		"THEN we can choose that object as the body",	NOTHING,
	ID_UF,		"",	NOTHING,

	ID_UA,		"IF we want to find",			DESIRED,
	ID_UA,		"THEN we can choose that object as the body",	NOTHING,
	ID_UA,		"",	NOTHING,


	ID_UV,		"IF we want to find",			DESIRED,
	ID_UV,		"THEN we can choose that object as the body",	NOTHING,
	ID_UV,		"",	NOTHING,

	ID_MOCB,	"The mass of a compound body is the",	MASS_BODY,
	ID_MOCB,	"in the compound",						NOTHING,

	ID_HCA,		"Choose axes to maximize the number of vectors",	AXIS_DIR,
	ID_HCA,		"",	NOTHING,
	ID_HCA,		"IN ORDER TO minimize the number of vectors to be",	AXIS_PROP,


//***************************8
	ID_VIRA,		"IF a vector",		VEC_PROPS,
	ID_VIRA,		"THEN the vector component",	NOTHING,	
	ID_VIRA,		"along the axis",						COMP_PROPS,             
	ID_VIRA,		"AND its sign is",	AXIS_SIGN,

	ID_WMG,		"IF",		EXISTANCE,
	ID_WMG,		"AND the object",		OBJ_PROPS,
	ID_WMG,		"AND",		CONSTANTS,
	ID_WMG,		"THEN the magnitude of the weight force is w = m*g",	NOTHING,


	ID_CCB,		"IF we want to find",		DESIRED,
	ID_CCB,		"AND the object is",	OBJ_RELS,
	ID_CCB,		"AND the two objects",	TWO_OBJ_RELS,
	ID_CCB,		"THEN we can choose the two objects as a compound body", NOTHING,
	
	ID_CBBTT,	"IF we want to find", DESIRED,
	ID_CBBTT,	"AND the object is",	OBJ_RELS,
	ID_CBBTT,	"AND the object is",	OBJ_RELS,
	ID_CBBTT,	"THEN we can choose the two objects as a compound body",	NOTHING,

	ID_FOCB,	"IF an object is",	OBJ_RELS,
	ID_FOCB,	"AND the object",		OBJ_PROPS,
	ID_FOCB,	"EXERTED BY",	AGENTS,
	ID_FOCB,	"THEN there is the same force on the compound body",	NOTHING,

	ID_TFD,		"IF",							EXISTANCE,
	ID_TFD,		"EXERTED BY",					AGENTS,
	ID_TFD,		"THEN it is directed",			DIRECTIONS,
	ID_TFD,		"AND",							REL_SENSE,

	ID_NFD,		"IF",		EXISTANCE,
	ID_NFD,		"",			NOTHING,
	ID_NFD,		"THEN it is directed", DIRECTIONS,
	ID_NFD,		"",		NOTHING,

};


extern const tmplInfo tmplStrs[]=
{
//	OBJ_PROPS,	"is at rest",									"at-rest",
	OBJ_PROPS,	"is taut",										"is-taut",
	OBJ_PROPS,	"has a force exerted on it",					"force-on-object",	
	OBJ_PROPS,	"is near the earth",							"near-earth",
	OBJ_PROPS,	"has mass m",									"mass-var",
	OBJ_PROPS,	"has acceleration a",							"acceleration-var",
	OBJ_PROPS,	"has velocity v",								"velocity-var",

	PRINCIPLES, "Newton's action-reaction law",					"action-reaction",
	PRINCIPLES, "Gravitational Attraction",						"gravitational-attaction",
	PRINCIPLES,	"Friction",										"friction",

	TWO_OBJ_RELS,	"are at rest",								"both-at-rest",
	TWO_OBJ_RELS,	"have same velocity and acceleration",		"same-motion",
	TWO_OBJ_RELS,	"have the same size",						"both-same-size",	

	OBJ_RELS,	"in surface contact with a second object",	"surface-contact",
	OBJ_RELS,       "part of a compound body",                "part-of-compound",
//	OBJ_RELS,	"tied to a second object",					"tied-together-contact",	
	OBJ_RELS,	"tied to a string",					"tied-to-string",
	
	DESIRED,	"the force on an object",						"desired-force",
	DESIRED,	"an object's acceleration",					"desired-acceleration",
	DESIRED,	"an object's velocity",						"desired-velocity",
	DESIRED,	"an object's mass",							"desired-mass",
	
	EXISTANCE,	"there is tension force on object",			"tension-exists",
	EXISTANCE,	"there is weight force on object",			"weight-exists",
	EXISTANCE,  "net force on the object along that axis = m*a",            "net-force",
	EXISTANCE,  "there is normal force on object",			"normal-exists",
	
	AGENTS,		"the string",								"string",			
	AGENTS,		"the earth",								"earth",
	AGENTS,		"the second object",						"second-object",
	AGENTS,		"an object outside the compound body",		"object-outside-compound",	
	
	CONSTANTS,	"Uk is the coefficient of kinetic friction",			"uk",
	CONSTANTS,	"Us is the coefficient of static friction",				"us",
	CONSTANTS,	"g is the gravitational acceleration",	"g",
	CONSTANTS,	"c is the speed of light",								"c",
	
	VEC_PROPS,	"has zero magnitude",							"zero-magnitude",
	VEC_PROPS,	"has same direction as an axis",			"parallel",
	VEC_PROPS,	"has direction opposite to an axis",			"opposite",	
	VEC_PROPS,	"is perpendicular to an axis",		"perpendicular",
	VEC_PROPS,	"forms an angle A with an axis",		"angle-A",
	
	COMP_PROPS,	"equals the vector magnitude",		   		"component-equal-magnitude",
	COMP_PROPS,	"is equal and opposite to the vector magnitude", "component-opposite-magnitude",
	COMP_PROPS,	"is zero",                                       "component-equal-0",
	COMP_PROPS, "equals (the vector magnitude) * cos(A)", 		"components-equal-cos",
	
	NET_FORCE,	"weight of the object",                             "weight-of",
	NET_FORCE,	"sum of all force components acting on the object",       "forces-on",
	NET_FORCE,	"sum of all force components exerted by the object",      "forces-by",
	NET_FORCE,	"friction force component on the object",                     "friction-on",
	
//	DIRECTIONS, "with the same direction as that force", "axis-par-force",
	DIRECTIONS,	"along the string",					"along-string",
	DIRECTIONS, "toward the center of the earth", "center-earth",
	DIRECTIONS, "upwards",							"upwards",
	DIRECTIONS, "parallel to surface of contact", "parallel-to-surface",
	DIRECTIONS, "perpendicular to surface of contact", "perpendicular-to-surface",
	
	AXIS_DIR,	"along that axis",			"parallel-axis",
	AXIS_DIR,	"in the same direction as the X-axis",			"parallel-x",
	AXIS_DIR,	"in the same direction as the Y-axis",			"parallel-y",
	AXIS_DIR,	"in the same direction as the X or Y axis",		"parallel-xy",
//	AXIS_DIR,	"perpendicular to the X-axis",			"perpendicular-x",
//	AXIS_DIR,	"perpendicular to the Y-axis",			"perpendicular-y",	
	
	AXIS_SIGN,	"opposite the direction of that axis",	"opposite-sign",
	AXIS_SIGN,	"always positive",						"positive",
	AXIS_SIGN,  "equal to the direction of that axis",	"same-sign",
	AXIS_SIGN,  "always negative",						"negative",
	
	REL_SENSE,	"toward the object",				"toward",
	REL_SENSE,	"away from the object",				"away",
	
	MASS_BODY,	"mass of the smallest object",		"smallest-mass",
	MASS_BODY,	"mass of the biggest object",		"biggest-mass",
	MASS_BODY,	"sum of the masses of the objects",	"sum-masses",
	AXIS_PROP,  "summed",							"summed",
	AXIS_PROP,	"decomposed into components",		"decomposed",
	AXIS_PROP,  "used in Newton's Law",				"used",
};

#define ARRAY_SIZE(array_name) (sizeof(array_name)/sizeof(array_name[0]))

const int NMIDSTEPSTRINGS ARRAY_SIZE(midstepStrings);
const int NSUBSTEPSTRINGS ARRAY_SIZE(substepStrings);
const int NSTEPSTRINGS	ARRAY_SIZE(stepStrings);
const int NRULES			ARRAY_SIZE(rules);
const int NTOPRULES		ARRAY_SIZE(topRules);	
const int NTMPLSTRS ARRAY_SIZE(tmplStrs);
const int NRULESETUPS ARRAY_SIZE(ruleSetup);
	
	
	
	
	
	