dojo.provide("drawing.tools.custom.Vector");
dojo.require("drawing.tools.custom._Base");
dojo.require("drawing.tools.Arrow");

drawing.tools.custom.Vector = drawing.util.oo.declare(
	drawing.tools.Arrow,
	drawing.tools.custom._Base,
	function(options){
		//
	},
	{
		draws:true,
		type:"drawing.tools.custom.Vector"
	}
	
);

drawing.tools.custom.Vector.setup = {
	name:"drawing.tools.custom.Vector",
	tooltip:"Vector Tool",
	iconClass:"iconVector"
};