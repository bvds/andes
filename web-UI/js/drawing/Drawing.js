dojo.provide("drawing.Drawing");

dojo.require("dojox.gfx");
dojo.require("drawing.util.oo");
dojo.require("drawing.util.common");
dojo.require("drawing.manager.Undo");
dojo.require("drawing.manager.keys");
dojo.require("drawing.manager.Mouse");
dojo.require("drawing.manager.Stencil");
dojo.require("drawing.manager.Anchors");
dojo.require("drawing.stencil.Stencil");
dojo.require("drawing.stencil.Line");
dojo.require("drawing.stencil.Rect");
dojo.require("drawing.stencil.Ellipse");

// not using widget, but just dojo.declare
// could add a widget that extends this

(function(){
	
	var surface;
	var createSurface = function(node, w, h, uid){
		surface = dojox.gfx.createSurface(node, w, h);
		if(dojo.isIE){
			surface.rawNode.parentNode.id = uid;
		}else{
			surface.rawNode.id = uid;
		}
	};
	
	dojo.declare("drawing.Drawing", [], {
		width:0,
		height:0,
		grid:"",
		constructor: function(props, node){
			this.id = node.id;
			this.util = drawing.util.common;
			this.util.register(this);
			this.domNode = node;
			this.tools = {};
			this.postCreate();
		},
		postCreate: function(){
			dojo.setSelectable(this.domNode, false);
			var dim = dojo.contentBox(this.domNode);
			this.height = dim.h;
			this.width = dim.w;
			createSurface(this.domNode, this.width, this.height, this.util.uid("surface"));
			
			this.mouse = new drawing.manager.Mouse({container:this.domNode});
			this.keys = drawing.manager.keys;
			this.undo = new drawing.manager.Undo({keys:this.keys});
			this.anchors = new drawing.manager.Anchors({mouse:this.mouse, undo:this.undo});
			
			this.stencils = new drawing.manager.Stencil({surface:surface, mouse:this.mouse, undo:this.undo, keys:this.keys, anchors:this.anchors, grid:{gap:100}});
			
			this.stencils.register(new drawing.stencil.Rect({
				parent:surface.createGroup(),
				mouse:this.mouse,
				data:{x:100, y:100, width:100, height:100}							  
			}));
			
			
			this.stencils.register(new drawing.stencil.Ellipse({
				parent:surface.createGroup(),
				mouse:this.mouse,
				data:{cx:150, cy:150, rx:50, ry:50}							  
			}));
			
			
			this.stencils.register(new drawing.stencil.Line({
				parent:surface.createGroup(),
				mouse:this.mouse,
				points:[{x:300,y:100},{x:500,y:200}]							  
			}));
		},
		onRenderStencil: function(stencil){
			console.log("onRenderStencil:", stencil)
			this.stencils.register(stencil);
			this.unSetTool();
			this.setTool(this.currentType);
		},
		registerTool: function(type, constr){
			this.tools[type] = constr;
		},
		setTool: function(type){
	//return;
			if(this.currentStencil){
				this.unSetTool();
			}
			this.currentType = type;
			try{
				//console.log("this.currentType:", this.currentType);
				this.currentStencil = new this.tools[this.currentType]({parent:surface.createGroup(), mouse:this.mouse, keys:this.keys});
				//console.log("this.currentStencil:", this.currentStencil)
				this._toolCon = dojo.connect(this.currentStencil, "onRender", this, "onRenderStencil");
			}catch(e){
				console.error("Drawing.setTool Error:", e);
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