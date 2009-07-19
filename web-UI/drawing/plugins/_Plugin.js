dojo.provide("drawing.plugins._Plugin");

drawing.plugins._Plugin = drawing.util.oo.declare(
	function(options){
		dojo.mixin(this, options);
	},
	{
		util:null,
		keys:null,
		mouse:null,
		drawing:null,
		stencils:null,
		anchors:null,
		canvas:null,
		node:null,
		type:"drawing.plugins._Plugin"
	}
);