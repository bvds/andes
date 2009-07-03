dojo.provide("drawing.stencil._Base");

(function(){
	
	drawing.stencil._Base = drawing.util.oo.declare(
		
		function(options){
			// clone style so changes are reflected in future shapes
			dojo.mixin(this, options);
			
			this.style = options.style || drawing.defaults.copy();
			
			this.id = options.id || this.util.uid(this.type);
			//console.log("ID:", this.id, ":::::", this.type, this)
			this._cons = [];
			
			if(this.draws){
				this.connectMouse();
				this._postRenderCon = dojo.connect(this, "render", this, "_onPostRender");
			}
			if(!this.annotation && !this.subShape){
				this.util.attr(this.parent, "id", this.id);
			}
			
			this._offX = this.mouse.origin.x;
			this._offY = this.mouse.origin.y;
			
			if(options.points){
				this.setPoints(options.points);
				this.render();
			}else if(options.data){
				this.setData(options.data);
				this.render();
			}
		},
		{
			
			//public
			type:"drawing.stencil",
			minimumSize:10,
			annotation:false,
			subShape:false,
			style:null,
			util:null,
			mouse:null,
			keys:null,
			points:[],
			data:null,
			
			//readonly
			created: false,
			
			//private
			_cons:[],
			
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
			getBounds: function(){
				// NOTE: Won't work for paths or annotations (labels, arrow tips)
				var p = this.points;
				var idx = p.length==2 ? 1 : 2;
				return {
					x1:p[0].x,
					y1:p[0].y,
					x2:p[idx].x,
					y2:p[idx].y,
					x:p[0].x,
					y:p[0].y,
					w:p[idx].x - p[0].x,
					h:p[idx].y - p[0].y
				}
			},
			
			attr: function(/*String*/key, /* optional anything */value){
				
				// CURRENTLY NOT USED
				// looking for a solution still. Haven't implemented
				// changing styles yet (switching yes, changing, no).
				
				//experimenting. currently only works with style object
				
				var prop;
				// TODO
				//if(prop === undefined){
				//	console.error("Stencil.attr not found:", key);
				//	return false;
				//}
				if(value !== undefined){
					if(typeof(value) == "object"){
						prop = dojo.getObject(key, false, this);
						console.log("val obj")
						for(var nm in value){
							prop[nm] = value[nm];		
						}
					}else{
						var k = key.substring(key.lastIndexOf(".")+1)
						prop = dojo.getObject(key.substring(0, key.lastIndexOf(".")), false, this);
						prop[k] = value;
					}
					this.render();
				}
				return prop;
			},
			
			_onPostRender: function(/*Object*/data){
				// drag-create should call onRender
				// afterwards, this calls onRender
				if(this.isBeingModified){
					this.onModify(this);
					this.isBeingModified = false;
				}else{
					this.onRender(this);	
				}
				
			},
			
			onBeforeRender: function(/*Object*/stencil){
				//stub
			},
			
			onModify: function(/*Object*/stencil){
			
			},
			
			onRender: function(/*Object*/stencil){
				// Drawing connects to this (once!) to be
				// notified of drag completion. But only if it
				//	was registered as a Tool. Creating Stencil in and of
				// itself does not register it.
				//
				// This should fire
				// at the *end* of each render (not during drag)
				// (Maybe should be onRenderComplete?)
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
				this.util.attr(this, "drawingType", "stencil");
				
			},
			
			select: function(){
				// NOTE: Can't setStroke because Silverlight throws error
				this.selected = true;
				this.style.current = this.style.selected;
				this.style.currentHit = this.style.hitSelected;
				this.isBeingModified = true;
				console.info("stencil.select", this.id)
				this.render();
			},
			
			deselect: function(){
				this.selected = false;
				this.style.current = this.style.norm;
				this.style.currentHit = this.style.hitNorm;
				this.isBeingModified = true;
				console.log("deselect", this.id)
				
				// should not have to render here because the deselection
				// re-renders after the transform
				this.render();
			},
			
			toggleSelected: function(){
				this._upselected = !this._upselected;
				this.selected = this._upselected;
			},
			
			// change these to onModify's
			onTransformBegin: function(anchor){
				// called from anchor point up mouse down
				this.isBeingModified = true;
			},
			
			onTransformEnd: function(anchor){
				// called from anchor point up mouse up
				this.isBeingModified = false;
			},
			
			onTransform: function(anchor){
				// called from anchor point mouse drag
				if(!this.isBeingModified){
					this.onTransformBegin();
				}
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
			
			
			destroy: function(){
				// summary:
				//	Destroys this Stencil
				// Note:
				// 	Unregistering selection or shapes
				// 	needs to be done outside of this object
				console.info("shape.destroy", this.id);
				//this.onBeforeRender(this);
				this.disconnectMouse();
				this.disconnect(this._cons);
				dojo.disconnect(this._postRenderCon);
				this.remove(this.shape, this.hit);
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
				// Note: 
				if(typeof(o)!="object"){
					if(s){
						// function object function
						m = s; s = e; o = this;
					}else{
						// function function
						m = e; e = o; o = s = this;
					}
				}else if(!m){
					// object function function
					m = s; s = this;
				}else if (once){
					// object function object function Boolean
					var c = dojo.connect(o, e, function(evt){
						dojo.hitch(s, m)(evt);
						dojo.disconnect(c);
					});
					return c;
				}else{
					// object function object function
				}
				return dojo.connect(o, e, s, m);
			},
			
			disconnect: function(handles){
				if(handles) return;
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