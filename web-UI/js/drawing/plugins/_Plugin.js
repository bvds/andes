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
		
		onSurfaceReady: function(){
			// summary:
			//	Called when Canvas surface is ready to draw to
			// Can be connected to or overwritten
		}
	}
);