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
dojo.require("drawing.manager.Anchors");
dojo.require("drawing.stencil._Base");
dojo.require("drawing.stencil.Line");
dojo.require("drawing.stencil.Rect");
dojo.require("drawing.stencil.Ellipse");
dojo.require("drawing.stencil.Path");
dojo.require("drawing.stencil.Text");
dojo.require("drawing.stencil.Image");
dojo.require("drawing.manager.Silverlight");

dojo.require("drawing.tools.TextBlock");
dojo.require("drawing.tools.Rect");
dojo.require("drawing.tools.Ellipse");
dojo.require("drawing.tools.Line");

// not using widget, but just dojo.declare
// could add a widget that extends this

(function(){
	var _plugsInitialized = false;
	dojo.declare("drawing.Drawing", [], {
		width:0,
		height:0,
		grid:"",
		constructor: function(props, node){
			// FIXME:
			// currently most objects get their styles/defaults
			// like:drawing.defaults.copy();
			// - is this good or should the object always be passed?
			this.defaults = drawing.defaults;
			
			this.id = node.id;
			this.util = drawing.util.common;
			this.util.register(this); // So Toolbar can find this Drawing
			this.keys = drawing.manager.keys;
			this.mouse = new drawing.manager.Mouse({util:this.util, keys:this.keys});
			this.tools = {};
			this.stencilTypes = {};
			this.srcRefNode = node; // need this?
			this.domNode = node;
			var str = dojo.attr(node, "plugins");
			if(str){
				this.plugins = eval(str);
			}
			
			if(dijit){
				this.widgetId = this.id;
				dojo.attr(this.domNode, "widgetId", this.widgetId)
				dijit.registry.add(this);
			}else{
				this._createCanvas();
			}
			
		},
		
		_createCanvas: function(){
			this.canvas = new drawing.manager.Canvas({
				srcRefNode:this.domNode,
				util:this.util,
				mouse:this.mouse,
				callback: dojo.hitch(this, "onSurfaceReady")
			});
			this.initPlugins();
		},
		resize: function(box){
			dojo.style(this.domNode, {
				width:box.w+"px",
				height:box.h+"px"
			});
			if(!this.canvas){
				this._createCanvas();		
			}else{
				this.canvas.resize(box.w, box.h);
			}
		},
		startup: function(){
			console.info("drawing startup")
		},
		getShapeProps: function(data) {
			// For convenience. May or may not be in final code.
			return dojo.mixin({
				parent:this.canvas.surface.createGroup(),
				util:this.util,
				keys:this.keys,
				mouse:this.mouse
			}, data || {});
		},
		
		addPlugin: function(plugin){
			// summary:
			//	Add a toolbar plugin object to plugins array
			//	to be parsed
			
			this.plugins.push(plugin);
		},
		initPlugins: function(){
			// called from Toolbar after last plugin has been loaded
			// The call to this coming from toobar is a bit funky as the timing
			// of IE for canvas load is different than other browsers
			
			if(!this.canvas.surfaceReady){
				var c = dojo.connect(this, "onSurfaceReady", this, function(){
					dojo.disconnect(c);
					this.initPlugins();
				})
				return;
			}
			dojo.forEach(this.plugins, function(p, i){
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
			this.plugins = [];
			_plugsInitialized = true;
			// In IE, because the timing is different we have to get the
			// canvas position after everything has drawn. *sigh*
			this.mouse.setCanvas();
		},
		onSurfaceReady: function(){
			console.info("Surface ready")
			this.mouse.init(this.canvas.domNode);
			this.undo = new drawing.manager.Undo({keys:this.keys});
			this.anchors = new drawing.manager.Anchors({mouse:this.mouse, undo:this.undo, util:this.util});
			this.stencils = new drawing.manager.Stencil({canvas:this.canvas, surface:this.canvas.surface, mouse:this.mouse, undo:this.undo, keys:this.keys, anchors:this.anchors});
			
			// plugin??
			new drawing.manager.Silverlight({mouse:this.mouse, stencils:this.stencils, anchors:this.anchors, canvas:this.canvas});
			
			// objects for testing. Final code will move these into test HTML
			
			/*
			this.stencils.register(new drawing.stencil.Rect(this.getShapeProps(
				{data:{x:100, y:100, width:93, height:93}}
			)));
			
			this.stencils.register(new drawing.stencil.Line(this.getShapeProps(
				{points:[{x:300,y:300},{x:500,y:200}]}
			)));
		
			this.stencils.register(new drawing.stencil.Image(this.getShapeProps(
				{data:{src:"images/BallOnWall.png", x:300, y:200, width:"auto"}}
			)));
			
			this.stencils.register(new drawing.stencil.Image(this.getShapeProps(
				{data:{src:"images/BallOnWall.png", x:110, y:110, width:320, height:220}}
			)));
		 	
		 	this.stencils.register(new drawing.stencil.Rect(this.getShapeProps(
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
			
			// register stencils that are not in the tool bar
			this.registerTool("drawing.stencil.Image");
			this.registerTool("drawing.stencil.Path");
			this.registerTool("drawing.stencil.Text");
		},
		
		
		addStencil: function(type, options){
			return this.stencils.register( new this.stencilTypes[type](this.getShapeProps(options)));
		},
		
		onRenderStencil: function(stencil){
			
			console.info("drawing.onRenderStencil:", stencil)
			this.stencils.register(stencil);
			this.unSetTool();
			this.setTool(this.currentType);
		},
		
		registerTool: function(type){
			var constr = dojo.getObject(type);
			this.tools[type] = constr;
			var abbr = type.substring(type.lastIndexOf(".")+1).charAt(0).toLowerCase()
				+ type.substring(type.lastIndexOf(".")+2);
			this.stencilTypes[abbr] = constr;
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
			
		}
	});
	
})();