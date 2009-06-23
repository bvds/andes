dojo.provide("drawing.manager.Mouse");

drawing.manager.Mouse = drawing.util.oo.declare(
	//
	// singleton? Would need to:
	// track multiple containers & drawings
	// comm back only to that drawing
	// more difficult to maintain selection
	//
	function(options){
		this.container = options.container;
		this.util = options.util;
		this.keys = options.keys;
		var pos = dojo.coords(this.container);
		this.origin = dojo.clone(pos);
		var c;
		var _isDown = false;
		dojo.connect(this.container, "mousedown", this, function(evt){
			this.down(evt);
			_isDown = true;
			c = dojo.connect(document, "mousemove", this, "drag");
		});
		dojo.connect(document, "mouseup", this, function(evt){
			dojo.disconnect(c);
			_isDown = false;
			this.up(evt);
		});
		dojo.connect(document, "mousemove", this, function(evt){
			if(!_isDown){
				this.move(evt);
			}
		});
		dojo.connect(this.keys, "onEsc", this, function(evt){
			this._dragged = false;	
		});
	},
	
	{
		doublClickSpeed:400,
		registerd:{},
		_lastx:0,
		_lasty:0,
		__reg:0,
		register: function(scope){
			var handle = scope.id || "reg_"+(this.__reg++);
			if(!this.registerd[handle]) { this.registerd[handle] = scope; }
			return handle;
		},
		unregister: function(handle){
			if(!this.registerd[handle]){ return; }
			delete this.registerd[handle];
		},
		
		_broadcastEvent:function(strEvt, obj){
			for(var nm in this.registerd){
				if(this.registerd[nm][strEvt]) this.registerd[nm][strEvt](obj);
			}
		},
		
		
		
		onCanvasUp: function(evt){
			//console.log("mouse onCanvasUp")
		},
		
		onDown: function(obj){
			//console.info(this.eventName("down"))
			this._broadcastEvent(this.eventName("down"), obj);			
		},
		onDrag: function(obj){
			var nm = this.eventName("drag");
			if(this._selected && nm == "onDrag"){
				nm = "onStencilDrag"
			}
			this._broadcastEvent(nm, obj);
		},
		onMove: function(obj){
			this._broadcastEvent("onMove", obj);
		},
		onUp: function(obj){
			// blocking first click-off (deselect), largely for TextBlock
			// TODO: should have param to make this optional??
			var nm = this.eventName("up");
			//console.info("1)", nm, this._selected)
			
			if(nm == "onStencilUp"){
				this._selected  = true;
			}else if(this._selected && nm == "onUp"){ //////////////////////////////////////////
				nm = "onStencilUp";
				this._selected = false;
			}
			//console.info("2)", nm, this._selected)
			this._broadcastEvent(nm, obj);
			
			// Silverlight double-click handled in Silverlight class
			if(dojox.gfx.renderer == "silverlight"){ return; }
			
			this._clickTime = new Date().getTime();
			if(this._lastClickTime){
				if(this._clickTime-this._lastClickTime<this.doublClickSpeed){
					var dnm = this.eventName("doubleClick");
					console.warn("DOUBLE CLICK", dnm, obj);
					this._broadcastEvent(dnm, obj);
				}else{
					console.log("    slow:", this._clickTime-this._lastClickTime)
				}
			}
			this._lastClickTime = this._clickTime;
			
		},
		
		zoom: 1,
		setZoom: function(zoom){
			this.zoom = 1/zoom;
		},
		
		eventName: function(name){
			name = name.charAt(0).toUpperCase() + name.substring(1);
			var dt = !this.drawingType || this.drawingType=="surface" || this.drawingType=="canvas" ? "" : this.drawingType;
			var t = !dt ? "" : dt.charAt(0).toUpperCase() + dt.substring(1);
			return "on"+t+name;
		},
		
		up: function(evt){
			this.onUp(this.create(evt));
		},
		
		down: function(evt){
			var dim = this._getXY(evt);
			var x = dim.x - this.origin.x;
			var y = dim.y - this.origin.y;
			
			x*= this.zoom;
			y*= this.zoom;
			
			this.origin.startx = x;
			this.origin.starty = y;
			this._lastx = x;
			this._lasty = y;
			
			this.drawingType = this.util.attr(evt, "drawingType") || "";
		
		console.log("evt:", evt);
		console.log("this.drawingType:", this.drawingType)
		
			this.onDown({x:x,y:y, pageX:dim.x, pageY:dim.y, id:this._getId(evt)});
			dojo.stopEvent(evt);
		},
		move: function(evt){
			//this.onMove(this.create(evt));
		},
		drag: function(evt){
			this.onDrag(this.create(evt));
		},
		create: function(evt){
			var dim = this._getXY(evt);
			var x = dim.x - this.origin.x;
			var y = dim.y - this.origin.y;
			var o = this.origin;
			
			var withinCanvas = x>=0 && y>=0 && x<=o.w && y<=o.h;
			var id = withinCanvas ? this._getId(evt) : "";
			x*= this.zoom;
			y*= this.zoom;
			
			
			var ret = {
				x:x,
				y:y,
				pageX:dim.x,
				pageY:dim.y,
				orgX:o.x,
				orgY:o.y,
				last:{
					x: this._lastx,
					y: this._lasty
				},
				start:{
					x: this.origin.startx,
					y: this.origin.starty
				},
				id:id,
				withinCanvas:withinCanvas
			};
			this._lastx = x;
			this._lasty = y;
			dojo.stopEvent(evt);
			return ret;
		},
		_getId: function(evt){
				return this.util.attr(evt, "id") || this.util.attr(evt.target.parentNode, "id");
		},
		_getXY: function(evt){
			return {x:evt.pageX, y:evt.pageY};
		}
	}
);
