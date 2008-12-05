// pick out variable name from definition string
// The forms that are matched:
//    ?var is ...
//    ?var: ...
//    ?var = ...
//    Let ?var be ...
//    Define ?var (as|to be) ...
//
//  To do:
//    Test for variable names that begin with a number
//
//    Handle variable names containing parentheses:   
//    "H(house) is the height of the house"

function getVariableName(intext) {
  // canonicalize whitespace
  var cantext = intext.replace(/\s+/g," ");
  cantext = cantext.replace(/\s*=\s*/," = ");
  cantext = cantext.replace(/^\s+/,"");
  // match for forms like ?var is ...
  var equality=/^([\w\\]+)(:| is| =) /i;
  var match = equality.exec(cantext);
  // console.log("equality match ",match);
  if (match) return match[1];
  // match for let ...
  var letre=/^let ([\w\\]+) (=|be) /i;
  match = letre.exec(cantext);
  // console.log("letre match ",match," for ",cantext);
  if(match) return match[1];
  // match for define ...
  var definere=/^define ([\w\\]+)(to be|=|as) /i;
  match = definere.exec(cantext);
  // console.log("definere match ",match);
  if (match) return match[1];
  return null;
}
