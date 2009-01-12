//PlanStrs.h

typedef struct
{	int	stepID;
	char* listItem;
	char* lispString;
} stepInfo;


typedef struct
{
	int stepID;
	char* listItem;
	char* lispString;
	int searchType;
} substepInfo;


extern const char* const goalValues[];
extern const stepInfo    stepStrings[];
extern const stepInfo	 midstepStrings[];
extern const substepInfo substepStrings[];

typedef struct
{
	int ruleID;
	char* rule;
} topRuleInfo;

typedef struct
{
	int ruleID;
	char* rule;
	int nRule;
}ruleInfo;

extern const topRuleInfo topRules[];
extern const ruleInfo rules[];

typedef struct
{
	int nRule;
	char* prepStr;
	int catID;
}tmplSetup;

typedef struct
{
	int catID;
	char* catStrs;
	char* idTmplStrs;
}tmplInfo;

extern const tmplSetup ruleSetup[];
extern const tmplInfo tmplStrs[];



extern const int NMIDSTEPSTRINGS; 
extern const int NSUBSTEPSTRINGS; 
extern const int NSTEPSTRINGS;	
extern const int NRULES;			
extern const int NTOPRULES;
extern const int NTMPLSTRS;
extern const int NRULESETUPS;	

//ID'S CONNECT TWO ARRAYS
//Can't have a zero, so those without types can be initialized to it
//SEARCHTYPE
#define MASS			1	//mass must be one to agree with its index number 
							//in the plan dialog (creating the goal)
#define ACCELERATION	2	//acceleration must be one to agree with its index number 
							//in the plan dialog (creating the goal)

#define ID_SYSBODY		3//doubles as a searchType
#define DISPLACEMENT	4


#define	ID_QUANTS		5
#define ID_SYSPROPS		7
#define DURATION		8	//duration must be 6 to agree with its index number 
							//in the plan dialog (creating the goal)
#define ID_FORCES		9//doubles as a searchType


//SEARCHTYPE
#define ID_EQNS			10

#define ID_FORCE_EQN	11//connector to proper array(step to substep)
#define ID_THIRD		12//connector to proper array(step to new goal)
#define ID_GENERAL		13//connector to proper array(step to midstep)

//SEARCHTYPES
#define AXIS			14
#define INI_VELOCITY	15	
#define ENERGY_POINTS	16
#define FIN_VELOCITY	17
#define COMPONENTS		18
#define UNKNOWNS		19
#define QUANTITIES		20
#define EQUATION		21
#define FORCE_EQUATION	22


//individual RULE ID's
#define ID_UNDEF		0
#define ID_TEXT         1	//????
#define	ID_AAH			3	//1

#define	ID_AAV			4	//2
#define	ID_AAA			5	//2	
#define	ID_AASF			6	//2

#define ID_NFE			7	//3
#define ID_WFD			8	//3

#define ID_SOF			9	//3

#define ID_NSL			16	//3 to 4

#define ID_WMG			17	//4
#define ID_NFD			18	//2
#define ID_TFD			19	//4
#define ID_TE			20	//4


#define ID_WFE			21	//5 lines



#define ID_HEADER		22//all declarations after this have no heading
#define IDH_UNDEF		23
#define ID_VEC_RUL		24

#define ID_UF			25//2
#define ID_UA			26//2
#define ID_UV			27//2
#define ID_MOCB			28//2
#define ID_VAGA			29//2
#define ID_VOA			30//2 but long, maybe three
#define ID_NVNC			31//2 but long , maybe three
#define ID_HCA			32//2 but long maybe three
#define ID_VSDA			33//2 but long, maybe three
#define ID_VIRA			34//3 but long, maybe 4
#define	ID_FOCB			35//4
#define ID_CCB			36//4
#define	ID_CBBTT		37//4//they are inserted at the root






//template categories
#define OBJ_PROPS		0
#define PRINCIPLES		1
#define OBJ_RELS		2
#define DESIRED			3
#define EXISTANCE		4
#define	AGENTS			5
#define CONSTANTS		6
#define NOTHING			7
#define VEC_PROPS		8
#define	COMP_PROPS		9
#define NET_FORCE		10
#define DIRECTIONS		11
#define TWO_OBJ_RELS	12
#define REL_SENSE		13
#define AXIS_DIR		14
#define MASS_BODY		15
#define AXIS_PROP		16
#define AXIS_SIGN		17
