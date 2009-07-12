dojo.provide("drawing.stencil._Base");

(function(){
		
	drawing.stencil._Base = drawing.util.oo.declare(
		
		function(options){
			// clone style so changes are reflected in future shapes
			dojo.mixin(this, options);
			
			this.style = options.style || drawing.defaults.copy();
			this.isText = this.type=="drawing.stencil.Text" || this.type=="drawing.tools.TextBlock";
			this.marginZero = options.marginZero || this.style.anchors.marginZero;
			this.id = options.id || this.util.uid(this.type);
			this._cons = [];
			if(this.draws){
				this.connectMouse();
				this._postRenderCon = dojo.connect(this, "render", this, "_onPostRender");
			}
			if(!this.annotation && !this.subShape){
				this.util.attr(this.parent, "id", this.id);
			}
			
			this.connect(this, "onBeforeRender", "preventNegativePos");
			
			this._offX = this.mouse.origin.x;
			this._offY = this.mouse.origin.y;
			
			if(this.isText){
				this.align = options.align || this.align;
				this.valign = options.valign || this.valign;
				this.textSize = parseInt(this.style.text.size, 10);
				this._lineHeight = this.textSize * 1.5;
				this.style.hitSelected.width *= 0.5;
				this.style.hitHighlighted.width *= 0.5;
			}
			if(options.points){
				this.setPoints(options.points);
				this.render();
			}else if(options.data){
				options.data.width = options.data.width ? options.data.width : this.style.text.minWidth;
				options.data.height = options.data.height ? options.data.height : this._lineHeight;
				this.setData(options.data);
				this.render(options.data.text);
			}
			
		},
		{
			
			//public
			parent:null, // shape(s) container TODO: change to 'container'
			type:"drawing.stencil",
			isText:false,
			minimumSize:10,
			annotation:false,
			subShape:false,
			style:null,
			util:null,
			mouse:null,
			keys:null,
			points:[],
			data:null,
			// how closely shape can get to y:0 or x:0 ÐÊzero may show bugs in VML
			zeroMargin:0,
			
			//readonly
			created: false,
			enabled:true,
			highlighted:false,
			selected:false,
			draws:false,
			
			onDelete: function(/* Stencil */ stencil){
				// summary:
				//	Stub - fires before this is destroyed
				console.info("onDelete", this.id);
			},
			
			onBeforeRender: function(/*Object*/ stencil){
				// summary:
				//	Stub - Fires before render occurs.
			},
			
			onModify: function(/*Object*/stencil){
				// summary:
				//	Stub - fires on change of any property,
				// including style properties
				
			},
			
			onChangeData: function(/*Object*/ stencil){
				// summary:
				//	Stub - fires on change of dimensional
				//	properties or a text change	
			},
			
			onChangeText: function(value){ // value or 'this' ?
				// summary:
				//	Stub - fires on change of text in a
				//	TextBlock tool only
			},
			
			onRender: function(/*Object*/ stencil){
				// summary:
				//	Stub - Fires on creation.
				// 	Drawing connects to this (once!) to be
				// 	notified of drag completion. But only if it
				//	was registered as a Tool. Creating Stencil in and of
				// 	itself does not register it.
				//
				// 	This should fire
				// 	at the *end* of each render (not during drag)
				// 	(Maybe should be onRenderComplete?)
				//
				if(!this._postRenderCon){
					this._postRenderCon = dojo.connect(this, "render", this, "_onPostRender");
				}
				this.created = true;
				this.disconnectMouse();
				
				// for Silverlight 
				if(this.shape) {
					this.shape.superClass = this;
				}else{
					this.parent.superClass = this;
				}
				this._setNodeAtts(this);
				
			},
			
			onChangeStyle: function(/*Object*/stencil){ 
				
				this._isBeingModified = true; // need this to prevent onRender
				
				// TODO: Make this _changeStyle and call onChangeSyyle
				//
				// ??? -> TODO - try mixin so if new style does not have fill, the norm.fill will be used
				//
				if(this.selected){
					//this.style.current = this.style.selected;
					this.style.currentHit = this.style.hitSelected;
					//this.style.currentText = this.style.textSelected;
					
				}else if(this.highlighted){
					//this.style.current = this.style.highlighted;
					this.style.currentHit = this.style.hitHighlighted;
					//this.style.currentText = this.style.textHighlighted;
					
				}else if(!this.enabled){
					this.style.current = this.style.disabled;
					this.style.currentText = this.style.textDisabled;	
					this.style.currentHit = this.style.hitNorm;
					
				}else{
					this.style.current = this.style.norm;
					this.style.currentHit = this.style.hitNorm;
					this.style.currentText = this.style.text;
				}
				// NOTE: Can't just change props like setStroke
				//	because Silverlight throws error
				this.render();
			},
			
			attr: function(/*String | Object*/key, /* ? String | Number */value){
				// summary
				//	Changes properties in the normal-style.
				var n = this.style.norm, h = this.style.hitNorm, t = this.style.text, o;
				
				if(typeof(key)!="object"){
					o = {};
					o[key] = value;
				}else{
					o = key;
				}
				for(var nm in o){
					if(nm in n){ n[nm] = o[nm]; }
					//if(nm in h){ h[nm] = o[nm]; }
					if(nm in t){ t[nm] = o[nm]; }
				}
				if(this.isText){
					//h.fill = {r:255, g:255, b:255, a:0}
				}
				this.render();
			},
			
			//	TODO:
			// 		Makes these all called by att()
			//		Should points and data be?
			//
			disable: function(){
				console.info("disable:", this.id)
				this.enabled = false;
				this.onChangeStyle(this);
			},
			
			enable: function(){
				this.enabled = true;
				this.onChangeStyle(this);
			},
			
			select: function(){
				console.log("------------select-------------")
				this.selected = true;
				this.onChangeStyle(this);
			},
			
			deselect: function(){
				this.selected = false;
				this.onChangeStyle(this);
				// should not have to render here because the deselection
				// re-renders after the transform
				// but... oh well.
			},
			
			highlight: function(){
				console.log("HIGHT:", this.id)
				this.highlighted = true;
				this.onChangeStyle(this);
			},
			
			unhighlight: function(){
				this.highlighted = false;
				this.onChangeStyle(this);
			},
			
			
			onTransformBegin: function(anchor){
				// called from anchor point up mouse down
				this._isBeingModified = true;
			},
			
			onTransformEnd: function(anchor){
				// called from anchor point up mouse up
				this._isBeingModified = false;
				this.onModify(this);
			},
			
			onTransform: function(anchor){
				// called from anchor point mouse drag
				// also called right away from plugins.Pan.checkBounds
				if(!this._isBeingModified){
					this.onTransformBegin();
				}
				// this is not needed for anchor moves, but it
				// is for stencil move:
				this.setPoints(this.points);
				this.render();			
			},
			
			transformPoints: function(mx){
				//
				// should have two sets of points
				// bounding box points - for transforms
				// and shape points - for editing
				//
				dojo.forEach(this.points, function(o){
					o.x += mx.dx;
					o.y += mx.dy;
				});
				this.onTransform();
				this.onTransformEnd();
			},
			
			setTransform: function(mx){
				this.transformPoints(mx);
			},
			
			setData: function(d){
				this.data = d;
				this.points = this.dataToPoints();
			},
			
			setPoints: function(p){
				this.points = p;
				// Path doesn't do data
				if(this.pointsToData){
					this.data = this.pointsToData();
				}
				
			},
			
			setLabel: function(text){
				if(!this._label){
					this._label = new drawing.stencil._Label({
						text:text,
						util:this.util,
						mouse:this.mouse,
						stencil:this,
						annotation:true,
						parent:this.parent,
						labelPosition:this.labelPosition
					});
				}else if(text){
					this._label.setLabel(text);
				}
			},
			
			
			
			getBounds: function(){
				// NOTE: Won't work for paths or annotations (labels, Axes, arrow tips)
				//	They should overwrite.
				var p = this.points;
				if(p.length==2){
					var x1 = p[0].x < p[1].x ? p[0].x : p[1].x;
					var y1 = p[0].y < p[1].y ? p[0].y : p[1].y;
					var x2 = p[0].x < p[1].x ? p[1].x : p[0].x;
					var y2 = p[0].y < p[1].y ? p[1].y : p[0].y;
					return {
						x1:x1,
						y1:y1,
						x2:x2,
						y2:y2,
						x:x1,
						y:y1,
						w:x2-x1,
						h:y2-y1
					};
				}else{
					return {
						x1:p[0].x,
						y1:p[0].y,
						x2:p[2].x,
						y2:p[2].y,
						x:p[0].x,
						y:p[0].y,
						w:p[2].x - p[0].x,
						h:p[2].y - p[0].y
					};
				}
			},
			
			
			preventNegativePos: function(){
				// PRIVATE
				// summary:
				//	Prevent item from being drawn/rendered less than zero
				// on the X or Y.
				//
				// if being modified anchors will prevent less than zero.
				if(this._isBeingModified){ return; }
				// why is this sometimes empty?
				if(!this.points.length){ return; }
				
				dojo.forEach(this.points, function(p){
					p.x = p.x < 0 ? this.zeroMargin : p.x;
					p.y = p.y < 0 ? this.zeroMargin : p.y;
				});
				this.setPoints(this.points);
			},
			
			
			
			_onPostRender: function(/*Object*/data){
				// drag-create should call onRender
				// afterwards, this calls onRender
				if(this._isBeingModified){
					this.onModify(this);
					this._isBeingModified = false;
				}else{
					this.onRender(this);	
				}
				if(!this.selected && this._prevData && dojo.toJson(this._prevData) != dojo.toJson(this.data)){
					this.onChangeData(this);
					this._prevData = dojo.clone(this.data);
				}else if(!this._prevData && (!this.isText || this._text)){
					console.warn("SET PREV FIRST TIME", this.data, this._text)
					this._prevData = dojo.clone(this.data);
				}else{
					//console.info("data not changed:");console.info("prev:", dojo.toJson(this._prevData));console.info("curr:", dojo.toJson(this.data))
				}
				
			},
			
			_setNodeAtts: function(shape){
				// summary:
				//	Sets the rawNode attribute. (Or in Silverlight
				//	an "object attribute". "stencil" is
				// 	used by the application to determine if
				//	something is selectable or not. This also
				//	sets the mouse custom events like:
				//	"onStencilUp". To disable the selectability,
				//	make the att "", which causes  standard
				//	mouse event.
				var att = this.enabled && !this.annotation ? "stencil" : "";
				this.util.attr(shape, "drawingType", att);
			},
		
		
			destroy: function(){
				// summary:
				//	Destroys this Stencil
				// Note:
				//	Can connect to this, or connect
				//	to onDelete
				//
				// prevent loops:
				if(this.destroyed){ return; }
				if(this.data || this.points && this.points.length){
					this.onDelete(this);
				}
				console.info("shape.destroy", this.id, this.points);
				this.disconnectMouse();
				this.disconnect(this._cons);
				dojo.disconnect(this._postRenderCon);
				this.remove(this.shape, this.hit);
				// silly I know, but we need to know it's going to be gone
				// before it is:
				this.destroyed = true;
			},
			
			remove: function(/*Shape...*/){
				// summary:
				//	Removes shape(s), typically before a re-render
				// 	No args defaults to this.shape
				//	Pass in multiple args to remove multiple shapes
				//
				//this.parent.clear(); return;
				var a = arguments;
				if(!a.length){
					if(!this.shape){ return; }
					a = [this.shape];
				}
				for(var i=0;i<a.length;i++){
					//a[i] && a[i].removeShape();
					if(a[i]) {
						a[i].removeShape();
					}
				}
			},
			
			connectMult: function(){
				// summary:
				//	Convenience method for batches of quick connects
				// 	Handles are not returned and therefore cannot be
				//	disconnected until Shape destroy time
				//
				if(arguments.length>1){
					// arguments are the connect params
					this._cons.push(this.connect.apply(this, arguments));
				}else if(dojo.isArray(arguments[0][0])){
					// an array of arrays of params
					dojo.forEach(arguments[0], function(ar){
						this._cons.push(this.connect.apply(this, ar));	
					}, this);
				}else{
					//one array of params
					this._cons.push(this.connect.apply(this, arguments[0]));
				}
				
			},
			
			// TODO: connect to a Shape event from outside class
			connect: function(o, e, s, m, /* Boolean*/once){
				// summary:
				//	Convenience method for quick connects
				//	See comments below for possiblities
				//	functions can be strings
				// once:
				//	If true, the connection happens only
				//	once then disconnects. Five args are required
				//	for this functionality.
				//
				var c;
				if(typeof(o)!="object"){
					if(s){
						// ** function object function **
						m = s; s = e; e=o; o = this;
					}else{
						// ** function function **
						m = e; e = o; o = s = this;
					}
				}else if(!m){
					// ** object function function **
					m = s; s = this;
				}else if (once){
					// ** object function object function Boolean **
					c = dojo.connect(o, e, function(evt){
						dojo.hitch(s, m)(evt);
						dojo.disconnect(c);
					});
					this._cons.push(c);
					return c;
				}else{
					// ** object function object function **
				}
				c = dojo.connect(o, e, s, m);
				this._cons.push(c);
				return c;
			},
			
			disconnect: function(handles){
				if(!handles) { return };
				if(typeof(handles)!="array"){ handle=[handle]; }
				dojo.forEach(handles, dojo.disconnect, dojo);
			},
			
			connectMouse: function(){
				this._mouseHandle = this.mouse.register(this);
			},
			disconnectMouse: function(){
				this.mouse.unregister(this._mouseHandle);
			},
			
			// Should be overwritten by sub class:
			render: function(){},
			renderOutline: function(){},
			dataToPoints: function(o){},
			pointsToData: function(p){},
			onDown: function(obj){
				// by default, object is ready to accept data
				// turn this off for dragging or onRender will
				// keep firing and register the shape
				dojo.disconnect(this._postRenderCon);
				this._postRenderCon = null;
			},
			onMove: function(){},
			onDrag: function(){},
			onUp: function(){}
		}
	);
})();