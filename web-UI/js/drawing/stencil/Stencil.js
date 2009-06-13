dojo.provide("drawing.stencil.Stencil");

(function(){
	
	drawing.stencil.Stencil = drawing.util.oo.declare(
		
		function(options){
			this.util = drawing.util.common;
			this.parent = this.orgParent = options.parent;
			this.mouse = options.mouse;
			this.id = options.id || this.util.uid(this.type);
			this._shapeCons = [];
			this.connectMouse();
			//dojo.attr(this.parent.rawNode, "id", "G_"+this.id);
			//console.log("GROUP", this.parent.id)
			this._postRenderCon = dojo.connect(this, "render", this, "_onPostRender");
		},
		{
			style:{
				line:{
					width:3,
					color:"#FF0000",
					style:"Solid",
					cap:"round" // square?, butt, round
				},
				hitline:{
					width:10,
					color:"#FFFF00",
					style:"Solid",
					cap:"round"
				},
				outline:{
					width:4,
					color:"#666666",
					style:"Dash"
				},
				fill:"#FF00FF"
			},
			created: false,
			minimumSize:10,
			attr: function(/*String*/key, /* optional anything */value){
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
						console.log("val str", prop)
						prop[k] = value;
					}
					this.render();
				}
				return prop;
			},
			
			
			
			_onPostRender: function(/*Object*/data){
				// drag-create should call onRender
				// afterwards, this calls onRender
				this.onRender(data);
			},
			onRender: function(/*Object*/stencil){
				// Drawing connects to this (once!) to be
				// notified of drag completion. This should fire
				// at the *end* of each render (not during drag)
				// (Maybe should be onRenderComplete?)
		console.warn("stencil.onRender")
				if(!this._postRenderCon){
					this._postRenderCon = dojo.connect(this, "render", this, "_onPostRender");
				}
				this.createSelectionOutline();
				this.created = true;
				this.connectShape();
				dojo.attr(this.shape.rawNode, "drawingType", "stencil");
				dojo.attr(this.shape.rawNode, "id", this.id);
				this.disconnectMouse();
				
			},
			
			
			
			
			select: function(){
				// on mouse down - always select
				this.selected = true;
				this.attr("style.line.color", "#FFFF00");
			},
			deselect: function(){
				// on mouse down - always select
				this.selected = false;
				this.attr("style.line.color", "#FF0000");
			},
			transformPoints: function(mx){
				//
				// should have two sets of points
				// bounding box points - for transforms
				// and shape points - for editing
				//
				console.warn("transformPoints")
				dojo.forEach(this.points, function(o){
					o.x += mx.dx;
					o.y += mx.dy;
				});
				this.render();
			},
			toggleSelected: function(){
				this._upselected = !this._upselected;
				this.selected = this._upselected;
			},
			setParent: function(container){
				this.parent = container || this.orgParent;
				console.log(this.id, "set parent", this.parent.rawNode)
				this.parent.add(this.shape);
			},
			destroy: function(){
				// unregistering selection or shapes
				// needs to be done outside of this object
				console.info("shape.destroy", this.id)
				this.disconnectMouse();
				this.disconnectShape();
				dojo.disconnect(this._postRenderCon);
				this.remove();
			},
			remove: function(){
				this.disconnectShape();
				var a = arguments;
				if(!a.length){
					a = [this.shape];
				}
				for(var i=0;i<a.length;i++){
					if(a[i]){
						a[i].removeShape();
					}
				}
			},
			
			onShapeOver: 	function(evt, item){},
			onShapeOut: 	function(evt, item){},
			onShapeDown: 	function(evt, item){},
			onShapeUp: 		function(evt, item){},
			
			connectShape: function(){
		return;
				//TODO:
				// leave over and out
				
				this._shapeCons = [
					this.shape.connect("onmousedown", this, function(evt){
						this.onShapeDown(evt, this);
					}),
					this.shape.connect("onmouseup", this, function(evt){
						this.onShapeUp(evt, this);
					}),
					this.shape.connect("onmouseover", this, function(evt){
						this.onShapeOver(evt, this);
					}),
					this.shape.connect("onmouseout", this, function(evt){
						this.onShapeOut(evt, this);
					})
					
				];
			},
			disconnectShape: function(){
				if(this.shape && this._shapeCons.length){
					dojo.forEach(this._shapeCons, function(h){
						this.shape.disconnect(h);
					}, this);
				}
			},
			
			connectMouse: function(){
				this._mouseHandle = this.mouse.register(this);
			},
			disconnectMouse: function(){
				this.mouse.unregister(this._mouseHandle);
			},
			
			// Should be overwritten by sub class:
			createSelectionOutline: function(){},
			onDown: function(){
				console.log("shape down");
			},
			onMove: function(){
				//console.log("shape move");	
			},
			onDrag: function(){
				console.log("shape drag");
			},
			onUp: function(){
				console.log("shape up");
			}
		}
	);
})();