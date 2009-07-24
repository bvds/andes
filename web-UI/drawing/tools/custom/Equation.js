dojo.provide("drawing.tools.custom.Equation");
dojo.require("drawing.tools.TextBlock");

drawing.tools.custom.Equation = drawing.util.oo.declare(
	// summary:
	//	Essentially the same as the TextBlock tool, but
	//	allows for a different icon and tooltip title.
	//
	drawing.tools.TextBlock,
	function(options){
	
	},
	{
		customType:"equation"
	}
	
);

drawing.tools.custom.Equation.setup = {
	// summary: See stencil._Base dojox.__ToolsSetup
	//
	name:"drawing.tools.custom.Equation",
	tooltip:"Equation Tool",
	iconClass:"iconEq"
};