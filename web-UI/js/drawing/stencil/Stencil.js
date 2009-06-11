dojo.provide("drawing.stencil.Stencil");

drawing.stencil.Stencil = drawing.util.oo.declare(
	
	function(options){
		this.util = drawing.util.common;
		this.parent = options.parent;
		this.mouse = options.mouse;
		this.id = options.id || this.util.uid(this.type);
		this.connectMouse();
	},
	{
		style:{
			line:{
				width:3,
				color:"#FF0000",
				style:"solid",
				cap:"square" // butt, round
			},
			hitline:{
				width:10,
				color:"#FFFF00",
				style:"solid",
				cap:"round"
			},
			outline:{
				width:4,
				color:"#666666",
				style:"dash"
			},
			fill:"#FFFFFF"
		},
		created: false,
		render: function(/* Stencil Props*/obj){
			
		},
		_onRender: function(/*Object*/data){
			// stub that class onRender
			// or is blocked during drag-create
			this.created = true;
			this.onRender(data);
		},
		onRender: function(/*Object*/data){
			this.disconnectMouse();
		},
		create: function(/* Mouse Props*/obj){
			
		},
		onCreate: function(/*Stencil*/stencil){
			
		},
		destroy: function(){
			// unregistering selection or shapes
			// needs to be done outside of this object
			console.info("shape.destroy", this.id)
			this.disconnectMouse();
			this.remove();
		},
		remove: function(){
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
		
		connectMouse: function(){
			this._mouseHandle = this.mouse.register(this);
		},
		disconnectMouse: function(){
			this.mouse.unregister(this._mouseHandle);
		},
		
		// Should be overwritten by sub class:
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