dojo.provide("drawing.tools.custom.Equation");
dojo.require("drawing.tools.TextBlock");

drawing.tools.custom.Equation = drawing.util.oo.declare(
	// summary:
	
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