dojo.provide("drawing.Drawing");

dojo.require("dojox.gfx");
dojo.require("drawing.util.oo");
dojo.require("drawing.util.common");
dojo.require("drawing.defaults");
dojo.require("drawing.manager.Canvas");
dojo.require("drawing.manager.Undo");
dojo.require("drawing.manager.keys");
dojo.require("drawing.manager.Mouse");
dojo.require("drawing.manager.Stencil");
dojo.require("drawing.util.SubStencil");
dojo.require("drawing.manager.Anchors");
dojo.require("drawing.stencil.Stencil");
dojo.require("drawing.stencil.Line");
dojo.require("drawing.stencil.Rect");
dojo.require("drawing.stencil.Ellipse");
dojo.require("drawing.stencil.Path");
dojo.require("drawing.stencil.Text");
dojo.require("drawing.stencil.TextBlock");
dojo.require("drawing.manager.Silverlight");

// not using widget, but just dojo.declare
// could add a widget that extends this

(function(){
	
	dojo.declare("drawing.Drawing", [], {
		width:0,
		height:0,
		grid:"",
		constructor: function(props, node){
			this.id = node.id;
			this.util = drawing.util.common;
			this.util.register(this);
			this.keys = drawing.manager.keys;
			this.mouse = new drawing.manager.Mouse({util:this.util, keys:this.keys});
			dojo.connect(this.keys, "onKeyUp", this, "onKeyUp");
			dojo.connect(this.keys, "onKeyDown", this, "onKeyDown");
			this.tools = {};
			this.srcRefNode = node;
			var str = dojo.attr(node, "plugins");
			if(str){
				console.log("plugins:", str)
				this.plugins = eval(str);
				console.dir(this.plugins);
			}
			this.canvas = new drawing.manager.Canvas({
				srcRefNode:node,
				util:this.util,
				mouse:this.mouse,
				callback: dojo.hitch(this, "onSurfaceReady")
			});
			
		},
		
		getShapeProps: function(data) {
			// convenience. May or may not be in final code.
			return dojo.mixin({
				parent:this.canvas.surface.createGroup(),
				util:this.util,
				keys:this.keys,
				mouse:this.mouse
			}, data || {});
		},
		
		addPlugin: function(plugin){
			console.warn("ADD PLUGIN", plugin);
			this.plugins.push(plugin);
		},
		initPlugins: function(){
			console.log("INIT PLUGINS:", this.plugins);
			dojo.forEach(this.plugins, function(p, i){
				console.log("PLUGIN:", p)
				var props = dojo.mixin({
					util:this.util,
					keys:this.keys,
					mouse:this.mouse,
					drawing:this,
					stencils:this.stencils,
					anchors:this.anchors,
					canvas:this.canvas
				}, p.options || {});
				this.registerTool(p.name, dojo.getObject(p.name));
				this.plugins[i] = new this.tools[p.name](props);
			}, this);
					
		},
		onSurfaceReady: function(){
			this.domNode = this.canvas.domNode;
			this.mouse.init(this.domNode);
			this.undo = new drawing.manager.Undo({keys:this.keys});
			this.anchors = new drawing.manager.Anchors({mouse:this.mouse, undo:this.undo, util:this.util});
			this.stencils = new drawing.manager.Stencil({canvas:this.canvas, surface:this.canvas.surface, mouse:this.mouse, undo:this.undo, keys:this.keys, anchors:this.anchors});
			
			// plugin??
			new drawing.manager.Silverlight({mouse:this.mouse, stencils:this.stencils, anchors:this.anchors, canvas:this.canvas});
			
			this.initPlugins();
			
			// objects for testing. Final code will move these into test HTML
			this.stencils.register(new drawing.stencil.Rect(this.getShapeProps(
				{data:{x:100, y:-100, width:200, height:200}}
			)));
	
		/*	this.stencils.register(new drawing.stencil.Rect(this.getShapeProps(
				{data:{x:400, y:100, width:200, height:200}}
			)));
			
			this.stencils.register(new drawing.stencil.TextBlock(this.getShapeProps(
				{	align:"end",
					valign:"middle",
					data:{x:200, y:300, width:"auto", text:"Mike's\nFantabulous Dynamic\nText"}}
			)));
			
			this.stencils.register(new drawing.stencil.TextBlock(this.getShapeProps(
				{	align:"end",
					valign:"middle",
					data:{x:300, y:150, width:300, text:"Dynamic Text"}}
			)));
			
			this.stencils.register(new drawing.stencil.Ellipse(this.getShapeProps(
				{data:{cx:150, cy:150, rx:50, ry:50}}
			)));
			
			this.stencils.register(new drawing.stencil.Ellipse(this.getShapeProps(
				{points:[{x:300,y:300},{x:500,y:300},{x:500,y:400},{x:300,y:400}]}
			)));
			
			this.stencils.register(new drawing.library.Arrow(this.getShapeProps(
				{points:[{x:300,y:300},{x:500,y:200}]}
			)));*/
			
			dojo.forEach(this.plugins, function(p){
				p.onSurfaceReady && p.onSurfaceReady();	
			});
		},
		onRenderStencil: function(stencil){
			
			console.info("drawing.onRenderStencil:", stencil)
			this.stencils.register(stencil);
			this.unSetTool();
			this.setTool(this.currentType);
		},
		registerTool: function(type, constr){
			this.tools[type] = constr;
		},
		setTool: function(type){
			if(!this.canvas.surface){
				var c = dojo.connect(this, "onSurfaceReady", this, function(){
					dojo.disconnect(c);
					this.setTool(type);
				});
				return;
			}
			if(this.currentStencil){
				this.unSetTool();
			}
			this.currentType = type;
			console.log("REG TOOL :", this.currentType)
			try{
				this.currentStencil = new this.tools[this.currentType]({parent:this.canvas.surface.createGroup(), util:this.util, mouse:this.mouse, keys:this.keys});
				this._toolCon = dojo.connect(this.currentStencil, "onRender", this, "onRenderStencil");
			}catch(e){
				console.error("Drawing.setTool Error:", e);
				console.error(this.currentType + " is not a constructor: ", this.tools[this.currentType]);
				//console.trace();
			}
		},
		unSetTool: function(){
			dojo.disconnect(this._toolCon);
			if(!this.currentStencil.created){
				this.currentStencil.destroy();	
			}
			
		},
		
		pan:function(evt, sel){
			this.canvas.setPan(sel)
		},
		onKeyUp: function(evt){
			this.pan(null, false);
		},
		onKeyDown: function(evt){
			if(evt.keyCode == 32){
				this.pan(null, true);
			}
		}
	});
	
})();