define(["andes/startup"], function(andes){

    // It would be better that this module returns an object called "variablename."
    // This is just to get things working with minimal changes to the pre-AMD version.
    andes.variablename={};

// pick out variable name from definition string
// The forms that are matched:
//    ?var is ...
//    ?var: ...  [space optional]
//    ?var = ...  [spaces optional]
//    Let ?var be ...
//    Define ?var (as|to be) ...
//
// Variables are any alphanumeric, _ \ and $,
// for LaTeX compatibility
//
// Returns empty string in case of no match.  This correctly
// handles the case where a modification to the text removes the symbol.
//
// This routine needs to match function pull-out-quantity
// in Help/Entry-API.cl
//
// To do:
//    Test for variable names that begin with a number
//
//    Handle variable names containing parentheses or brackets:
//    "H(green house) is the height of the house on Green St."
//    "A_{t=0} is the initial area"

andes.variablename.parse = function(intext){
	if(intext){
		// canonicalize whitespace
		var cantext = intext.replace(/\s+/g," ");
		cantext = cantext.replace(/\s*=\s*/," = ");
		cantext = cantext.replace(/\s*:\s*/,": ");
		cantext = cantext.replace(/^\s/,"");
		// Remove leading puctuation (for comments).
		cantext = cantext.replace(/^[?!;:,&#%]/,"");
		// match for forms like ?var is ...
		var equality=/^([\w\\$]+)(:| is| =) /i;
		var match = equality.exec(cantext);
		// console.log("equality match ",match);
		if (match) return match[1];
		// match for let ...
		var letre=/^let ([\w\\$]+) (=|be) /i;
		match = letre.exec(cantext);
		// console.log("letre match ",match," for ",cantext);
		if(match) return match[1];
		// match for define ...
		var definere=/^define ([\w\\$]+) (to be|=|as) /i;
		match = definere.exec(cantext);
		// console.log("definere match ",match);
		if (match) return match[1];	
	}
	return "";
}
});
