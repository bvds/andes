dojo.provide("drawing.Drawing");

dojo.require("dojox.gfx");
dojo.require("drawing.util.oo");
dojo.require("drawing.util.common");
dojo.require("drawing.manager.Mouse");
dojo.require("drawing.stencil.Stencil");
dojo.require("drawing.stencil.Line");
dojo.require("drawing.stencil.Rect");

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
			this.util = drawing.util.common;
			this.util.register(this);
			this.domNode = node;
			this.tools = {};
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
		},
		onRenderStencil: function(stencil){
			console.warn("Stencil created:", stencil);
			this.unSetTool();
			this.setTool(this.currentType);
		},
		registerTool: function(type, constr){
			this.tools[type] = constr;
		},
		setTool: function(type){
			if(this.currentStencil){
				this.unSetTool();
			}
			this.currentType = type;
			try{
				console.log("this.currentType:", this.currentType);
				this.currentStencil = new this.tools[this.currentType]({parent:surface.createGroup(), mouse:this.mouse});
				console.log("this.currentStencil:", this.currentStencil)
				this._toolCon = dojo.connect(this.currentStencil, "onRender", this, "onRenderStencil");
			}catch(e){
				console.error("Error:", e);
				console.error(this.currentType + " is not a constructor: ", this.tools[this.currentType]);
			}
		},
		unSetTool: function(){
			dojo.disconnect(this._toolCon);
			if(!this.currentStencil.created){
				this.currentStencil.destroy();	
			}
			
		}
	});
	
})();