dojo.provide("drawing.Drawing");

dojo.require("dojox.gfx");
dojo.require("drawing.util.oo");
dojo.require("drawing.manager.Mouse");


// not using widget, but just dojo.declare
// could add a widget that extends this

(function(){
	
	var surface;
	var createSurface = function(node, w, h){
		surface = dojox.gfx.createSurface(node, w, h);
	};
	
	dojo.declare("drawing.Drawing", [], {
		width:0,
		height:0,
		grid:"",
		constructor: function(props, node){
			console.log("Drawing constructor");
			this.id = node.id;
			this.domNode = node;
			this.postCreate();
		},
		postCreate: function(){
			console.log("Drawing DOM ready", this.domNode);
			dojo.setSelectable(this.domNode, false);
			var dim = dojo.contentBox(this.domNode);
			this.height = dim.h;
			this.width = dim.w;
			createSurface(this.domNode, this.width, this.height);
			
			this.mouse = new drawing.manager.Mouse({container:this.domNode});
		}
	});
	
})();