dojo.provide("drawing.tools.custom.Equation");
dojo.require("drawing.tools.TextBlock");

drawing.tools.custom.Equation = drawing.util.oo.declare(
	drawing.tools.TextBlock,
	function(options){
		
	},
	{
		//type:"drawing.tools.custom.Equation"
	}
);

drawing.tools.custom.Equation.setup = {
	name:"drawing.tools.custom.Equation",
	tooltip:"Equation Tool",
	iconClass:"iconEq"
}