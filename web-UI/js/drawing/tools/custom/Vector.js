dojo.provide("drawing.tools.custom.Vector");
dojo.require("drawing.tools.custom._Base");
dojo.require("drawing.tools.Arrow");
dojo.require("drawing.util.positioning");

drawing.tools.custom.Vector = drawing.util.oo.declare(
	drawing.tools.Arrow,
	drawing.tools.custom._Base,
	function(options){
		//
	},
	{
		draws:true,
		type:"drawing.tools.custom.Vector",
		
		labelPosition: function(){
			var d = this.data;
			var pt = drawing.util.positioning.label({x:d.x1,y:d.y1},{x:d.x2,y:d.y2});
			return {
				x:pt.x,
				y:pt.y
			}
		}
	}
	
);

drawing.tools.custom.Vector.setup = {
	name:"drawing.tools.custom.Vector",
	tooltip:"Vector Tool",
	iconClass:"iconVector"
};